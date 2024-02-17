#| doc
Parsing DSL

This library implements a simple DSL for constructing parsing functions. The
operation is traditional top down backtracking parsing.

|#

(define-library (owl parse)

   (export
      parses
      byte
      imm                       ;; byte
      imm-or                    ;; byte error-msg
      input-ready?              ;; parser, receives fd
      seq
      epsilon ε
      byte-if
      peek-byte                 ;; like byte-if, but do not consume
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

      error-context                ;; epsilon + leave a syntax error message (parser)

      syntax-error?                ;; val -> bool
      report-syntax-error          ;; val -> _ + side-effects

      byte-stream->exp-stream
      fd->exp-stream
      file->exp-stream
      silent-syntax-fail           ;; stream-val
      syntax-errors-as-values
      resuming-syntax-fail ;; error-msg → _
      )

   (import
      (owl core)
      (owl function)
      (owl lazy)
      (owl list)
      (owl port)
      (owl list-extra)
      (owl string)
      (owl math)
      (owl unicode)
      (owl io)
      (owl proof)
      (owl syscall))

   (begin

      ;; l = reverse list of left (handled) runes and (pos . error-message) parse error messages
      ;; r = a list or a ll of unparsed data
      ;; p = position in input
      ;; why = error message
      ;; ok = parser
      ;; parser = λ (l r p ok) → (ok l' r' p' val)
      ;;                       → (backtrack l r p why)
      ;;    backtracking with #f left means parse failure
      ;;    if r was a ll, it will be forced at least up to the syntax error

      (define (backtrack l r p why)
         (if (null? l)
            (values #f r p why) ;; final outcome if error
            (let ((hd (car l)))
               (cond
                  ((eq? (type hd) type-fix+)
                     (backtrack (cdr l) (cons hd r) p why))
                  ((pair? hd)
                     (cond
                        ((not (number? (car hd)))
                           (if (eq? (car hd) why)
                              (let ((node (cdr hd)))
                                 (backtrack (cdr l) r (+ (car node) 1) (cdr node))) ;; advance by 1 to distinquish from subsequent failed parses at the same point (temporary)
                              (backtrack (cdr l) r p why)))
                        ((> (car hd) p) ;; a deeper error message
                           ;; grab a parse error that managed to get deeper
                           (backtrack (cdr l) r (car hd) (cdr hd)))
                        (else
                           (backtrack (cdr l) r p why))))
                  (else
                     ;; backtrack point
                     (hd (cdr l) r p why))))))

      (define (input-ready? port)
         (λ (l r p ok)
            ;; already read tell what happened
            ;; uncomputed = unread, check if port is ready for reading (without blocking)
            ;;   - note: may also be readable but will give eof
            (ok l r p
               (cond
                  ((null? r) #false)
                  ((pair? r) #true)
                  ((pair? (interact 'iomux (tuple 'read-timeout port 100)))
                     ;; returns (timeout . message) on timeout
                     #false)
                  (else #true)))))

      (define eof-error "end of input")

      (define wrong-char "syntax error")

      (define (fail-expected byte)
         (list 'expected byte))

      (define (byte l r p ok)
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

      (define (peek-byte pred)
         (λ (l r p ok)
            (cond
               ((null? r)
                  (backtrack l r p eof-error))
               ((pair? r)
                  (if (pred (car r))
                     (ok l r p (car r))
                     (backtrack l r p wrong-char)))
               (else
                  ((peek-byte pred) l (r) p ok)))))

      (define (imm-or x why)
         (λ (l r p ok)
            (cond
               ((null? r)
                  (backtrack l r p why))
               ((pair? r)
                  (if (eq? (car r) x)
                     (ok (cons x l) (cdr r) (+ p 1) x)
                     (backtrack l r p (fail-expected x))))
               (else
                  ((imm-or x why) l (r) p ok)))))

      (define (imm x)
         (imm-or x eof-error))

      (define (ε val)
         (λ (l r p ok)
            (ok l r p val)))

      ;; leave a (error-message . (position . better-error-message)) -error node
      (define (error-context msg)
         (λ (l r p ok)
            (ok
               (cons (cons msg (cons p "Unterminated")) l)
               r p msg)))

      (define epsilon ε)

      (define (either a b)
         (λ (l r p ok)
            (a
               (cons
                  (λ (l r fp why)
                     ;; leave the error position and reason, so that in
                     ;; case of backtrack we can chich which unded up
                     ;; furthest
                     (b (cons (cons fp why) l) r p ok))
                  l)
               r p ok)))

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
                     (ok (cons (cons fp why) l) r p (reverse vals)))
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
            (let ((bt (λ (l r fp why) (ok (cons (cons fp why) l) r p (reverse vals)))))
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
            ((parses 42 l r p ok ((verify term) . rest) body)
               (if term
                  (parses 42 l r p ok rest body)
                  (backtrack l r p "verify failed")))
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
                (b extension-byte)
                (verify (not (< (two-byte-point a b) min-2byte)) "invalid 2-byte utf-8"))
               (two-byte-point a b))
            (parses
               ((a (byte-between 223 240))
                (b extension-byte)
                (c extension-byte)
                (verify (not (< (three-byte-point a b c) min-3byte)) "invalid 3-byte utf-8"))
               (three-byte-point a b c))
            (parses
               ((a (byte-between 239 280))
                (b extension-byte)
                (c extension-byte)
                (d extension-byte)
                (verify (not (< (four-byte-point a b c d) min-3byte)) "invalid 4-byte utf-8"))
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
         (λ (cont ll pos msg) val))

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

      (define (seek-line data pos)
         (let loop ((rl '()) (pos pos) (data data))
            (cond
               ((= pos 0)
                  (lets
                     ((prefix (reverse (take rl 80)))
                      (suffix rest (break (lambda (x) (eq? x #\newline)) data))
                      (suffix (take suffix 80))
                      (error-position (length prefix)))
                     (values
                        (list->string (append prefix suffix))
                        error-position)))
               ((null? data)
                  (values
                     "end of input"
                     0))
               ((eq? (car data) #\newline)
                  (loop '() (- pos 1) (cdr data)))
               (else
                  (loop
                     (cons (car data) rl)
                     (- pos 1)
                     (cdr data))))))

      ;; syntax error values
      (define *syntax-error-tag* "syntax-error") ;; any unique value

      (define (syntax-error data pos msg)
         (list *syntax-error-tag* data pos msg))

      (define (syntax-error? x) (and (pair? x) (eq? (car x) *syntax-error-tag*)))

      (define (verbose-error msg data pos val)
         (lets ((line pos (seek-line data pos)))
            (print-to stderr (or msg (or val "Syntax error")))
            (print-to stderr "  " line)
            (print-to stderr (list->string (map (lambda (x) #\space) (iota 0 1 (+ pos 1)))) "^")))

      (define (report-syntax-error e)
         (let ((e (cdr e)))
            (verbose-error #f (car e) (cadr e) (caddr e))))

      ;; todo: rename to silent-fail-at-trailing-whitespace or something
      (define (resuming-syntax-fail error-msg thunk)
         (λ (cont ll pos msg)
            ;; this is a bit of a hack
            ;; allow common whitespace at end of input, because parsers typically define structure
            ;; only up to last byte byte needed for recognition in order to avoid blocking
            (let ((rest (fast-forward ll)))
               (if (and (null? rest) (whitespace? ll))
                  (cont #n)
                  (begin
                     (verbose-error error-msg ll pos #f)
                     (thunk)
                     (cont rest))))))

      (define syntax-errors-as-values
         (λ (cont ll pos msg)
            ;; this is a bit of a hack
            ;; allow common whitespace at end of input, because parsers typically define structure
            ;; only up to last byte byte needed for recognition in order to avoid blocking
            (cons
               (syntax-error ll pos msg)
               (lambda ()
                  (cont (fast-forward ll))))))

      ;; computes rest of parser stream
      (define (silent-syntax-fail-at-end error-msg val)
         (λ (cont ll pos msg)
            (if (not (whitespace? ll))
               (begin
                  (print "Not whitespace: " ll)
                  (verbose-error error-msg ll pos #f)))
            val))

      ;; report only if resuming data is not totally whitespace
      (define (verbose-syntax-fail-sans-eof error-val whitespace-val)
         (λ (cont ll pos msg)
            (if (whitespace? ll)
               whitespace-val
               (begin
                  (verbose-error #f ll pos #f)
                  error-val))))

      ;; ll parser (ll r val → ?) → ll
      (define (byte-stream->exp-stream ll parser fail)
         (λ ()
            (lets ((lp r p val (parser #n ll 0 parser-succ)))
               (cond
                  (lp ;; something parsed successfully
                     (pair val (byte-stream->exp-stream r parser fail)))
                  ((null? r) ;; end of input, note that may also be
                     ;; typically there is whitespace, so this does not happen
                     #n)
                  ((function? fail)
                     ;(verbose-error "Stream parse error" r p val)
                     ;(verbose-error #f r p val)
                     (fail
                        (λ (ll) (byte-stream->exp-stream ll parser fail))
                        r p val))
                  (else
                     ;(verbose-error "stream parse error" r p val)
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

      (define (try-parse parser data maybe-path maybe-error-msg fail-fn)
         (let loop ((try (λ () (parser #n data 0 parser-succ))))
            (lets ((l r p val (try)))
                (cond
                  ((not l)
                     (verbose-error maybe-error-msg r p val)
                     (if fail-fn
                        (loop (λ () (fail-fn 0 #n)))
                        #false))
                  ((lpair? r) =>
                     (λ (r)
                        (loop (λ () (backtrack l r p "trailing garbage")))))
                  (else
                     ;; full match
                     val)))))

      ;; error is printed to stderr
      (define (parse parser data fail-val)
         (lets ((l r p val (parser #n data 0 parser-succ)))
             (cond
               ((not l)
                  (verbose-error #f r p val)
                  fail-val)
               ((lpair? r) =>
                  (λ (r)
                     ;; trailing garbage
                     fail-val))
               (else
                  ;; full match
                  val))))

      (define (first-match parser data fail-val)
         (lets ((l r p val (parser #n data 0 parser-succ)))
             (cond
               ((not l)
                  ;; p = point of failure
                  (values fail-val r p))
               (else
                  (values val r p)))))

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
         (first-match (either (seq (imm 1) (imm 1)) (seq (imm 1) (imm 2))) '(1 3 4) 'x) =
            (values 'x '(1 3 4) 1)
         (first-match (either (seq (imm 1) (imm 2)) (seq (imm 3) (imm 4))) '(1 3 4) 'x) =
            (values 'x '(1 3 4) 1)
         (first-match (either! (seq (imm 1) (imm 1)) (seq (imm 1) (imm 2))) '(1 2 3) 'x) =
            (values 'x '(1 2 3) 1) ;; first one matched something, so second must not be tried
         (first-match (either! (seq (imm 2) (imm 1)) (seq (imm 1) (imm 2))) '(1 2 3) 'x) =
            (values '(1 . 2) '(3) 2)
         (first-match (either! (seq (imm 2) (imm 1)) (seq (imm 1) (imm 2))) '(2 2 3) 'x) =
            (values 'x '(2 2 3) 1)
         (first-match (seq (imm 1) (peek-byte (λ (x) (eq? x 2)))) '(1 2 3) 'x)
            = (values '(1 . 2) '(2 3) 1)
         (first-match (seq (imm 1) (peek-byte (λ (x) (eq? x 2)))) '(1 3 3) 'x)
            = (values 'x '(1 3 3) 1)
         )


))

