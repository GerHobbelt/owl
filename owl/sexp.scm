
(define-library (owl sexp)

   (export
      sexp-parser
      read-exps-from
      list->number
      get-sexps         ;; greedy* get-sexp
      get-padded-sexps  ;; whitespace at either end
      string->sexp
      vector->sexps
      list->sexps
      read read-ll)

   (import
      (owl core)
      (owl eof)
      (prefix (owl parse) get-)
      (owl math)
      (owl string)
      (owl list)
      (owl math extra)
      (owl vector)
      (owl list-extra)
      (owl ff)
      (owl lazy)
      (owl symbol)
      (owl io) ; testing
      (owl port)
      (owl unicode)
      (owl proof)
      (only (owl syscall) error)
      (only (owl intern) intern-symbols string->uninterned-symbol))

   (begin

      ;; character classes
      (define classes #u8(0 0 0 0 0 0 0 0 0 8 8 8 8 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 1 16 0 1 1 1 0 0 0 1 3 0 3 2 1 6 6 6 6 6 6 6 6 6 6 1 0 1 1 1 1 1 5 37 5 37 69 5 1 1 65 1 1 1 1 1 33 1 1 1 1 1 1 1 1 33 1 1 0 16 0 1 1 0 21 53 5 37 69 5 1 1 65 1 1 1 1 17 33 1 1 17 1 17 1 1 1 33 1 1 0 16 0 1 0))

      (define-syntax is-class?
         (syntax-rules (x)
            ((is-class? class)
               (λ (x)
                  ;; out-of-bound access returns #f, which matches 1
                  (lesser? 0 (fxand (ref classes x) class))))))

      (define is-exactness? (is-class? #x40))
      (define is-radix? (is-class? #x20))
      (define is-escaped? (is-class? #x10))
      (define is-space? (is-class? 8))
      (define is-xdigit? (is-class? 4))
      ;; set the lowest bit to match high code points, too
      (define is-subsequent? (is-class? 3))
      (define is-initial? (is-class? 1))

      (define get-hash (get-imm #\#))
      (define get-pipe (get-imm #\|))

      (define (digit-char? base)
         (if (eq? base 16)
            is-xdigit?
            (λ (x) (lesser? (fxxor x #\0) base))))

      (define (bytes->number digits base)
         (fold
            (λ (n digit)
               (+ (* n base)
                  (fxand (if (lesser? #\9 digit) (lets ((d _ (fx- digit 7))) d) digit) 15)))
            0 digits))

      (define get-sign
         (get-one-of (get-imm 43) (get-imm 45) (get-epsilon 43)))

      (define bases
         (list->ff '((#\b . 2) (#\o . 8) (#\d . 10) (#\x . 16))))

      ;; the exactness prefixes are ignored in owl
      (define get-exactness-base
         (get-either
            (get-parses
               ((skip get-hash)
                (char
                  (get-either
                     (get-parses
                        ((char (get-byte-if is-radix?))
                         (skip
                           (get-either
                              (get-parses
                                 ((skip get-hash)
                                  (skip (get-byte-if is-exactness?)))
                                 #t)
                              (get-epsilon #f))))
                        char)
                     (get-parses
                        ((skip (get-byte-if is-exactness?))
                         (char
                           (get-either
                              (get-parses
                                 ((skip get-hash)
                                  (char (get-byte-if is-radix?)))
                                 char)
                              (get-epsilon #\d))))
                        char))))
               (get bases (fxior char 32))) ;; switch to lower case
            (get-epsilon 10)))

      (define (get-natural base)
         (get-parses
            ((digits (get-greedy-plus (get-byte-if (digit-char? base)))))
            (bytes->number digits base)))

      (define (get-integer base)
         (get-parses
            ((sign-char get-sign) ; + / -, default +
             (n (get-natural base)))
            (if (eq? sign-char #\+) n (- 0 n))))

      ;; → n, to multiply with
      (define (get-exponent base)
         (get-either
            (get-parses
               ((skip (get-imm #\e))
                (pow (get-integer base)))
               (expt base pow))
            (get-epsilon 1)))

      (define get-signer
         (get-parses ((char get-sign))
            (if (eq? char #\+) self (H - 0))))

      ;; separate parser with explicitly given base for string->number
      (define (get-number-in-base base)
         (get-parses
            ((sign get-signer) ;; default + <- could allow also an optional base here
             (num (get-natural base))
             (tail ;; optional after dot part be added
               (get-either
                  (get-parses
                     ((skip (get-imm #\.))
                      (digits (get-greedy-star (get-byte-if (digit-char? base)))))
                     (/ (bytes->number digits base)
                        (expt base (length digits))))
                  (get-epsilon 0)))
             (pow (get-exponent base)))
            (sign (* (+ num tail) pow))))

      ;; a sub-rational (other than as decimal notation) number
      (define get-number-unit
         (get-parses
            ((base get-exactness-base) ;; default 10
             (val (get-number-in-base base)))
            val))

      ;; anything up to a rational
      (define get-rational
         (get-parses
            ((n get-number-unit)
             (m (get-either
                  (get-parses
                     ((skip (get-imm #\/))
                      (m get-number-unit)
                      (verify (not (eq? 0 m)) "zero denominator"))
                     m)
                  (get-epsilon 1))))
            (/ n m)))

      (define get-imaginary-part
         (get-parses
            ((sign (get-either (get-imm #\+) (get-imm #\-)))
             (imag (get-either get-rational (get-epsilon 1))) ; we also want 0+i
             (skip (get-imm #\i)))
            (if (eq? sign #\+)
               imag
               (- 0 imag))))

      (define get-number
         (get-parses
            ((real get-rational) ;; typically this is it
             (imag (get-either get-imaginary-part (get-epsilon 0))))
            (if (eq? imag 0)
               real
               (complex real imag))))

      (define get-rest-of-line
         (get-parses
            ((chars (get-greedy-star (get-byte-if (B not (C eq? 10)))))
             (skip (get-imm 10))) ;; <- note that this won't match if line ends to eof
            chars))

      ;; skip everything up to |#
      (define (get-block-comment parser)
         (get-one-of
            (get-parses
               ((skip get-pipe)
                (skip get-hash)
                (comment parser))
               comment)
            (get-parses
               ((skip get-hash)
                (skip get-pipe)
                (comment (get-block-comment (get-block-comment parser))))
               comment)
            (get-parses
               ((skip get-byte)
                (comment (get-block-comment parser)))
               comment)))

      (define get-a-whitespace
         (get-one-of!
            (get-byte-if is-space?)
            (get-parses
               ((skip (get-imm #\;))
                (skip get-rest-of-line))
               #\space)
            (get-parses
               ((skip get-hash)
                (skip get-pipe)
                (comment (get-block-comment (get-epsilon #\space))))
               comment)))

      (define maybe-whitespace (get-star! get-a-whitespace))

      ; (
      (define (get-list-of parser)
         (get-parses
            ((lp (get-imm #\())
             (things
               (get-star! parser))
             (skip maybe-whitespace)
             (tail
               (get-either
                  (get-parses ((rp (get-imm #\)))) #n)
                  (get-parses
                     ((dot (get-imm #\.))
                      (fini parser)
                      (skip maybe-whitespace)
                      (skip (get-imm #\))))
                     fini))))
            (if (null? tail)
               things
               (append things tail))))

      (define mnemonic-escape
         (list->ff
            '((#\a . #\alarm)
              (#\b . #\backspace)
              (#\t . #\tab)
              (#\n . #\newline)
              (#\r . #\return))))

      (define (get-sequence-char delimiter)
         (get-either
            (get-parses
               ((skip (get-imm #\\))
                (char
                  (get-either
                     (get-parses
                        ((char (get-byte-if is-escaped?)))
                        (get mnemonic-escape char char))
                     (get-parses
                        ((skip (get-imm #\x))
                         (hexes (get-greedy-plus (get-byte-if is-xdigit?)))
                         (skip (get-imm #\;)))
                        (bytes->number hexes 16)))))
               char)
            (get-rune-if (λ (x) (eq? (fxior (eq? x delimiter) (eq? x #\\)) #f)))))

      (define (get-transparent-break parser)
         (get-parses
            ((skip
               (get-greedy-star
                  (get-parses
                     ((skip (get-imm #\\))
                      (skip (get-greedy-plus (get-byte-if is-space?))))
                     #n)))
             (data parser))
            data))

      (define get-string
         (get-parses
            ((skip (get-imm #\"))
             (chars
               (get-greedy-star
                  (get-transparent-break (get-sequence-char #\"))))
             (skip (get-transparent-break (get-imm #\"))))
            (runes->string chars)))

      (define get-identifier
         (get-one-of
            (get-parses
               ((head (get-rune-if is-initial?))
                (tail (get-star! (get-rune-if is-subsequent?))))
               (string->uninterned-symbol (runes->string (cons head tail))))
            (get-parses
               ((head (get-imm #\.))
                (tail (get-greedy-plus (get-rune-if is-subsequent?))))
               (let ((str (runes->string (cons head tail))))
                  (if (string=? str "...")
                     '...
                     (string->uninterned-symbol str))))
            (get-parses
               ((skip get-pipe)
                (chars (get-greedy-star (get-sequence-char #\|)))
                (skip get-pipe))
               (string->uninterned-symbol (runes->string chars)))))

      (define quotations
         (list->ff '((#\' . quote) (#\, . unquote) (#\` . quasiquote) (splice . unquote-splicing))))

      (define (get-quoted parser)
         (get-parses
            ((type
               (get-either
                  (get-parses ((_ (get-imm #\,)) (_ (get-imm #\@))) 'splice)
                  (get-byte-if (λ (x) (get quotations x)))))
             (value parser))
            (list (get quotations type) value)))

      (define get-named-char
         (get-one-of
            (get-word "null" #\null)
            (get-word "alarm" #\alarm)
            (get-word "backspace" #\backspace)
            (get-word "tab" #\tab)
            (get-word "newline" #\newline)
            (get-word "return" #\return)
            (get-word "escape" #\escape)
            (get-word "space" #\space)
            (get-word "delete" #\delete)))

      (define (get-letter-word l w val)
         (get-parses
            ((skip (get-imm l))
             (res
               (get-either
                  (get-word w val)
                  (get-epsilon val))))
            res))

      (define (get-hash-prefixed parser)
         (get-parses
            ((skip get-hash)
             (val
               (get-one-of
                  (get-letter-word #\f "alse" #false)
                  (get-letter-word #\n "ull" #null)
                  (get-letter-word #\t "rue" #true)
                  (get-parses ;; character
                     ((skip (get-imm #\\))
                      (codepoint (get-either get-named-char get-rune)))
                     codepoint)
                  (get-parses ;; #(...)
                     ((fields (get-list-of parser)))
                     (let ((fields (intern-symbols fields)))
                        (if (any pair? fields)
                           ;; vector may have unquoted stuff, so convert it to a sexp constructing a vector, which the macro handler can deal with
                           (cons '_sharp_vector fields) ; <- quasiquote macro expects to see this in vectors
                           (list->vector fields))))
                  (get-parses ;; #u8(...)
                     ((skip (get-imm #\u))
                      (skip (get-imm #\8))
                      (fields
                        (get-list-of
                           (get-parses
                              ((skip maybe-whitespace)
                               (base get-exactness-base)
                               (val (get-natural base))
                               (verify (lesser? val 256) '(bad u8)))
                              val))))
                     (raw fields type-bytevector))
                  (get-parses
                     ((bang (get-imm #\!))
                      (line get-rest-of-line))
                     (list 'quote (list 'hashbang (list->string line)))))))
            val))

      (define (get-sexp)
         (get-parses
            ((skip maybe-whitespace)
             (val
               (get-one-of
                  (get-list-of (get-sexp))
                  get-number         ;; more than a simple integer
                  get-identifier
                  (get-hash-prefixed (get-sexp))
                  get-string
                  (get-quoted (get-sexp))
                  (get-byte-if eof-object?))))
            val))

      (define sexp-parser
         ;; do not read trailing white-space to avoid blocking, when parsing a stream
         (get-parses ((sexp (get-sexp)))
            (intern-symbols sexp)))

      (define get-sexps
         (get-star! sexp-parser))

      ;; whitespace at either end
      (define get-padded-sexps
         (get-parses
            ((data get-sexps)
             (skip maybe-whitespace))
            data))

      ;; fixme: new error message info ignored, and this is used for loading causing the associated issue
      (define (read-exps-from data done fail)
         (lets/cc ret  ;; <- not needed if fail is already a cont
            ((data
               (utf8-decoder data
                  (λ (self line data)
                     (ret (fail (list "Bad UTF-8 data on line " line ": " (ltake line 10))))))))
            (sexp-parser data
               (λ (data drop val pos)
                  (cond
                     ((eof-object? val) (reverse done))
                     ((null? data) (reverse (cons val done))) ;; only for non-files
                     (else (read-exps-from data (cons val done) fail))))
               (λ (pos reason)
                  (if (null? done)
                     (fail "syntax error in first expression")
                     (fail (list 'syntax 'error 'after (car done) 'at pos))))
               0)))

      (define (list->number lst base)
         (get-try-parse (get-number-in-base base) lst #false #false #false))

      (define (string->sexp str fail)
         (get-try-parse sexp-parser
            (str-iter str)
            #false
            #false
            #false))

      ;; parse all contents of vector to a list of sexps, or fail with
      ;; fail-val and print error message with further info if errmsg
      ;; is non-false

      (define (vector->sexps vec fail errmsg)
         ; try-parse parser data maybe-path maybe-error-msg fail-val
         (let ((lst (vector->list vec)))
            (get-try-parse get-padded-sexps lst #false errmsg #false)))

      (define (list->sexps lst fail errmsg)
         ; try-parse parser data maybe-path maybe-error-msg fail-val
         (get-try-parse get-padded-sexps lst #false errmsg #false))

      (define (read-port port)
         (get-fd->exp-stream port sexp-parser (get-silent-syntax-fail null)))

      (define read-ll
         (case-lambda
            (()     (read-port stdin))
            ((thing)
               (cond
                  ((port? thing)
                     (read-port thing))
                  ((string? thing)
                     (get-try-parse get-padded-sexps (str-iter thing) #false #false #false))
                  (else
                     (error "read needs a port or a string, but got " thing))))))

      (define (read thing . rest)
         (let ((ll (read-ll thing)))
            (cond
               (ll (lcar ll))
               ((null? rest) (error "read: bad data in " thing))
               (else (car rest)))))
))
