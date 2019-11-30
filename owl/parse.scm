#| doc
owl cfg parsing combinators and macros
|#

(define-library (owl parse)

   (export
      parses
      byte
      imm
      seq
      epsilon ε
      byte-if
      rune
      rune-if
      either either!
      one-of one-of!
      star
      plus
      greedy-star star!
      greedy-plus plus!
      byte-between
      parse-head
      backtrack
      parse                ;; parser data fail-val → result | fail-val (if no full match)
      try-parse
      first-match          ;; parser data fail-val → result data'
      word
      maybe

      byte-stream->exp-stream
      fd->exp-stream
      file->exp-stream
      silent-syntax-fail
      resuming-syntax-fail ;; error-msg → _
      )

   (import
      (owl core)
      (owl function)
      (owl lazy)
      (owl list)
      (owl string)
      (owl math)
      (owl unicode)
      (owl io)
      (owl proof)
      (owl syscall))

   (begin

      ;; Why = #(pos message <rest>)
      ;;
      ;; (parser l r p ok)
      ;;   → (ok l' r' p' val) | (backtrack l r p why)
      ;   ... → l|#f r p' result|error

      ;; bactrtrack function : l r fp why

      (define (backtrack l r p why)
         (if (null? l)
            (values #f r p why) ;; final outcome if error
            (let ((hd (car l)))
               (if (eq? (type hd) type-fix+)
                  (backtrack (cdr l) (cons hd r) p why)
                  (hd (cdr l) r p why)))))

      (define eof-error "end of input")

      (define wrong-char "syntax error")

      (define (fail-expected byte)
         (list 'expected byte))

      (define (byte l r p ok)
         ;(print (list 'byte l r p))
         (cond
            ((null? r) (backtrack l r p eof-error))
            ((pair? r) (ok (cons (car r) l) (cdr r) (+ p 1) (car r)))
            (else      (byte l (r) p ok))))

      (define (byte-if pred)
         (λ (l r p ok)
            (cond
               ((null? r)
                  (backtrack l r p eof-error))
               ((pair? r)
                  (lets ((x xt r))
                     (if (pred x)
                        (ok (cons x l) xt (+ p 1) x)
                        (backtrack l r p wrong-char))))
               (else
                  ((byte-if pred) l (r) p ok)))))

      (define (imm x)
         (λ (l r p ok)
            ;(print (list 'imm l r p 'want x))
            (cond
               ((null? r)
                  (backtrack l r p eof-error))
               ((pair? r)
                  (if (eq? (car r) x)
                     (ok (cons x l) (cdr r) (+ p 1) x)
                     (backtrack l r p (fail-expected x))))
               (else
                  ((imm x) l (r) p ok)))))

      (define (ε val)
         (λ (l r p ok)
            (ok l r p val)))

      (define epsilon ε)

      (define (either a b)
         (λ (l r p ok)
            (a (cons (λ (l r fp why) (b l r p ok)) l) r p ok)))

      (define (either! a b)
         (λ (l r p ok)
            (a
               (cons
                  (λ (l r fp why)
                     (if (= fp p) ;; nothing matched
                        (b l r p ok)
                        (backtrack l r fp why)))
                  l)
               r p ok)))

      (define (maybe x val)
         (either x
            (epsilon val)))

      (define (seq a b)
         (λ (l r p ok)
            (a l r p
               (λ (l r p av)
                  (b l r p
                     (λ (l r p bv)
                        (ok l r p (cons av bv))))))))

      (define (star-vals a vals)
         ; (print (list 'star-vals a vals))
         (λ (l r p ok)
            ; (print (list 'star l r p))
            (a
               (cons
                  (λ (l r fp why)
                     ; (print (list 'star-backtrack l r fp why))
                      (ok l r p (reverse vals)))
                  l)
               r
               p
               (λ (l r p val)
                  ; (print (list 'star-ok l r p val))
                  ((star-vals a (cons val vals)) l r p ok)))))

      (define star
         (C star-vals #n))

      (define (drop l x)
         (cond
            ((eq? (car l) x)
               (cdr l))
            ((eq? (type (car l)) type-fix+)
               (cons (car l) (drop (cdr l) x)))
            (else
               (drop (cdr l) x))))

      (define (greedy-star-vals a vals)
         (λ (l r p ok)
            (let ((bt (λ (l r fp why) (ok l r p (reverse vals)))))
               (a
                  (cons bt l)
                  r
                  p
                  (λ (l r p val)
                     ((greedy-star-vals a (cons val vals))
                        (drop l bt) r p ok))))))

      (define-syntax parses
         (syntax-rules (verify eval)
            ((parses 42 l r p ok ((val (eval term)) . rest) body)
               (let ((val term))
                  (parses 42 l r p ok rest body)))
            ((parses 42 l r p ok ((val parser) . rest) body)
               (parser l r p
                  (λ (l r p val)
                     (parses 42 l r p ok rest body))))
            ((parses 42 l r p ok () body)
               (ok l r p body))
            ((parses 42 l r p ok ((verify term msg) . rest) body)
               (if term
                  (parses 42 l r p ok rest body)
                  (backtrack l r p msg)))
            ((parses ((a . b) ...) body)
               (λ (l r p ok)
                  (parses 42 l r p ok ((a . b) ...) body)))
            ((parses ((a . b) ...) first . rest)
               (parses ((a . b) ...) (begin first . rest)))))

      (define greedy-star
         (C greedy-star-vals #n))

      (define (greedy-plus a)
         (parses
            ((first a)
             (rest (greedy-star a)))
            (cons first rest)))


      (define star! greedy-star)
      (define plus! greedy-plus)

      (define (word s val)
         (let ((bytes (string->bytes s)))
            (λ (l r p ok)
               (let loop ((l l) (r r) (p p) (left bytes))
                  (cond
                     ((null? left)
                        (ok l r p val))
                     ((null? r)
                        (backtrack l r p eof-error))
                     ((pair? r)
                        (if (eq? (car r) (car left))
                           (loop (cons (car r) l) (cdr r) (+ p 1) (cdr left))
                           (backtrack l r p (fail-expected (car left)))))
                     (else
                        (loop l (r) p left)))))))

      (define-syntax one-of
         (syntax-rules ()
            ((one-of a) a)
            ((one-of a b) (either a b))
            ((one-of a b . c) (either a (one-of b . c)))))

      (define-syntax one-of!
         (syntax-rules ()
            ((one-of! a) a)
            ((one-of! a b) (either! a b))
            ((one-of! a b . c) (either! a (one-of! b . c)))))

      (define (plus parser)
         (parses
            ((a parser)
             (as (star parser)))
            (cons a as)))

      ; #b10xxxxxx
      (define extension-byte
         (parses
            ((b byte)
             (verify (eq? #b10000000 (fxand b #b11000000)) "Bad extension byte"))
            b))

      (define (byte-between lo hi)
         (byte-if
            (λ (x)
               (and (lesser? lo x) (lesser? x hi)))))

      (define rune
         (one-of
            (byte-if (C lesser? 128))
            (parses
               ((a (byte-between 127 224))
                (verify (not (eq? a #b11000000)) "blank leading 2-byte char") ;; would be non-minimal
                (b extension-byte))
               (two-byte-point a b))
            (parses
               ((a (byte-between 223 240))
                (verify (not (eq? a #b11100000)) "blank leading 3-byte char") ;; would be non-minimal
                (b extension-byte) (c extension-byte))
               (three-byte-point a b c))
            (parses
               ((a (byte-between 239 280))
                (verify (not (eq? a #b11110000)) "blank leading 4-byte char") ;; would be non-minimal
                (b extension-byte) (c extension-byte) (d extension-byte))
               (four-byte-point a b c d))))

      (define (rune-if pred)
         (parses
            ((val rune)
             (verify (pred val) "bad rune"))
            val))

      (define (parser-succ l r p v)
         (values l r p v))

      (define (parse-head parser ll def)
         (lets ((l r p val (parser #n ll 0 parser-succ)))
            (if l (cons val r) def)))

      ;; computes rest of parser stream
      (define (silent-syntax-fail val)
         (λ (cont ll msg) val))

      (define (fast-forward ll)
         (if (pair? ll)
            (fast-forward (cdr ll))
            ll))

      (define (whitespace? ll)
         (cond
            ((null? ll) #t)
            ((not (pair? ll)) #f)
            ((memq (car ll) '(#\newline #\space #\return #\tab))
               (whitespace? (cdr ll)))
            (else #f)))

      (define (resuming-syntax-fail error-reporter)
         (λ (cont ll msg)
            ;; this is a bit of a hack
            ;; allow common whitespace at end of input, because parsers typically define structure
            ;; only up to last byte byte needed for recognition in order to avoid blocking
            (let ((rest (fast-forward ll)))
               (if (and (null? rest) (whitespace? ll))
                  (cont #n)
                  (begin
                     (error-reporter msg)
                     (cont rest))))))

      (define (stopping-syntax-fail error-reporter)
         (λ (cont ll msg)
            (let ((rest (fast-forward ll)))
               (if (and (null? rest) (whitespace? ll))
                  (cont #n)
                  (begin
                     (error-reporter msg)
                     (cont rest))))))

      ;; ll parser (ll r val → ?) → ll
      (define (byte-stream->exp-stream ll parser fail)
         (λ ()
            (lets ((lp r p val (parser #n ll 0 parser-succ)))
               (cond
                  (lp ;; something parsed successfully
                     (pair val (byte-stream->exp-stream r parser fail)))
                  ((null? r) ;; end of input
                     ;; typically there is whitespace, so this does not happen
                     #n)
                  ((function? fail)
                     (fail
                        (λ (ll) (byte-stream->exp-stream ll parser fail))
                         r val))
                  (else
                     #n)))))

      (define (fd->exp-stream fd parser fail)
         (byte-stream->exp-stream (port->byte-stream fd) parser fail))

      (define (file->exp-stream path parser fail)
         ;(print "file->exp-stream: trying to open " path)
         (let ((fd (open-input-file path)))
            ;(print "file->exp-stream: got fd " fd)
            (if fd
               (fd->exp-stream fd parser fail)
               #false)))

      ;; this api is kind of ugly, simplify
      ;; this is badly named when prefixed as usual. parse?
      (define (try-parse parser data maybe-path maybe-error-msg fail-fn)
         (let loop ((try (λ () (parser #n data 0 parser-succ))))
            (lets ((l r p val (try)))
                (cond
                  ((not l)
                     (if fail-fn
                        (loop (λ () (fail-fn 0 #n)))
                        #false))
                  ((lpair? r) =>
                     (λ (r)
                        (loop (λ () (backtrack l r p "trailing garbage")))))
                  (else
                     ;; full match
                     val)))))

      (define (parse parser data fail-val)
         (let loop ((try (λ () (parser #n data 0 parser-succ))))
            (lets ((l r p val (try)))
                (cond
                  ((not l)
                     fail-val)
                  ((lpair? r) =>
                     (λ (r)
                        ;; trailing garbage
                        fail-val))
                  (else
                     ;; full match
                     val)))))

      (define (first-match parser data fail-val)
         (let loop ((try (λ () (parser #n data 0 parser-succ))))
            (lets ((l r p val (try)))
                (cond
                  ((not l)
                     (values fail-val r p))
                  (else
                     (values val r p))))))

      (example
         (first-match byte '(1 2) 'x) = (values 1 '(2) 1)
         (first-match (imm 42) '(1 2) 'x) = (values 'x '(1 2) 0)
         (first-match (seq (imm 1) (imm 2)) '(1 1 2) 'x) = (values 'x '(1 1 2) 1)
         (first-match (seq (imm 1) (imm 1)) '(1 1 2) 'x) = (values '(1 . 1) '(2) 2)
         (first-match (star (imm 1)) '(1 1 1 2) 'x) = (values '(1 1 1) '(2) 3)
         (first-match (plus (byte-if even?)) '(2 4 8 9) 'x) = (values '(2 4 8) '(9) 3)
         (first-match (plus (one-of (imm 1) (imm 2))) '(1 2 3 4) 'x) = (values '(1 2) '(3 4) 2)
         (first-match (either (seq (imm 1) (imm 1)) (seq (imm 1) (imm 2))) '(1 2 3) 'x) =
            (values '(1 . 2) '(3) 2)
         (first-match (either! (seq (imm 1) (imm 1)) (seq (imm 1) (imm 2))) '(1 2 3) 'x) =
            (values 'x '(1 2 3) 1)
         (first-match (either! (seq (imm 2) (imm 1)) (seq (imm 1) (imm 2))) '(1 2 3) 'x) =
            (values '(1 . 2) '(3) 2)
         )
))

