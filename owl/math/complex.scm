#| doc
Complex Numbers

This library defines complex arbitrary precision math functions.
|#

(define-library (owl math complex)

   (export + - * / = complex negate conjugate)

   (import

      (owl core)
      (owl list)
      (owl syscall)
      (owl proof)

      (prefix ;; prefix integer operations with i
         (only (owl math integer) + - * = << >> rem mod negate)
         i)

      (prefix ;; prefix some rational operations with r
         (only (owl math rational) + - negate)
         r)

      (only (owl math rational)
         ;; some rational versions are used as such
         mk-rational-add
         mk-rational-sub
         divide gcd gcdl
         <
         rational
         numerator denominator)

      (only (owl math integer)
         ncons ncar ncdr big-bad-args
         big-digits-equal?
         quotient ediv
         to-int- to-int+ to-fix+ to-fix-
         fx-greatest fx-width truncate/
         zero? positive? even? odd?
         integer? fixnum?
         right-out
         << >> band bior bxor))


   (begin

      (define (= a b)
         (case (type a)
            (type-fix+ (eq? a b))
            (type-fix- (eq? a b))
            (type-int+
               (case (type b)
                  (type-int+ (big-digits-equal? a b))
                  (else #false)))
            (type-int-
               (case (type b)
                  (type-int- (big-digits-equal? a b))
                  (else #false)))
            (type-rational
               (case (type b)
                  (type-rational
                     ;; todo: add eq-simple to avoid this becoming recursive
                     (if (= (ncar a) (ncar b))
                        (= (ncdr a) (ncdr b))
                        #false))
                  (else #false)))
            (type-complex
               (if (eq? (type b) type-complex)
                  (and (= (ref a 1) (ref b 1))
                       (= (ref a 2) (ref b 2)))
                  #false))
            (else
               (big-bad-args '= a b))))

      (define (<= a b)
         (or (< a b) (= a b)))

      (define (> a b) (< b a))
      (define (>= a b) (<= b a))

      (define-syntax complex
         (syntax-rules ()
            ((complex a b) (mkt type-complex a b))))

      (define +
         (mk-rational-add
            (λ (a b) ;; a is complex
               (if (eq? (type b) type-complex)
                  ;; A+ai + B+bi = A+B + (a+b)i
                  (lets
                     ((ar ai a)
                      (br bi b)
                      (r (r+ ar br))
                      (i (r+ ai bi)))
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a))
                     (complex (r+ ar b) ai))))))

      (define (complex-sub ar ai br bi)
         (let ((i (r- ai bi)))
            (if (eq? i 0)
               (r- ar br)
               (complex (r- ar br) i))))

      (define -
         (mk-rational-sub
            (λ (a b)
               (if (eq? (type a) type-complex)
                  ;; ar+ai - ?
                  (if (eq? (type b) type-complex)
                     (lets ((ar ai a)
                            (br bi b))
                           (complex-sub ar ai br bi))
                     (lets ((ar ai a))
                        (complex-sub ar ai b 0)))
                  (lets ((br bi b))
                     (complex-sub a 0 br bi))))))

      (define (mul a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-rational  (divide (mul a (ncar b)) (ncdr b)))
                  (type-complex
                     (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                        (if (eq? i 0) r (complex r i))))
                  (else (i* a b))))
            (type-fix-
               (case (type b)
                  (type-rational  (divide (mul a (ncar b)) (ncdr b)))
                  (type-complex
                     (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                        (if (eq? i 0) r (complex r i))))
                  (else (i* a b))))
            (type-int+
               (case (type b)
                  (type-rational  (divide (mul a (ncar b)) (ncdr b)))
                  (type-complex
                     (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                        (if (eq? i 0) r (complex r i))))
                  (else (i* a b))))
            (type-int-
               (case (type b)
                  (type-rational  (divide (mul a (ncar b)) (ncdr b)))
                  (type-complex
                     (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                        (if (eq? i 0) r (complex r i))))
                  (else (i* a b))))
            (type-rational
               (case (type b)
                  (type-rational
                     (divide (mul (ncar a) (ncar b)) (mul (ncdr a) (ncdr b))))
                  (type-complex
                     (lets ((br bi b) (r (mul a br)) (i (mul a bi)))
                        (if (eq? i 0) r (complex r i))))
                  (else
                     (divide (mul (ncar a) b) (ncdr a)))))
            (type-complex
               (if (eq? (type b) type-complex)
                  (lets
                     ((ar ai a)
                      (br bi b)
                      (r (r- (mul ar br) (mul ai bi)))
                      (i (r+ (mul ai br) (mul ar bi))))
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a)
                      (r (mul ar b)) ;; fixme: use r* instead
                      (i (mul ai b)))
                     (if (eq? i 0) r (complex r i)))))
            (else
               (i* a b))))

      (define (div a b)
         (cond
            ((eq? b 0)
               (error "division by zero " (list '/ a b)))
            ((eq? (type a) type-complex)
               (if (eq? (type b) type-complex)
                  (lets
                     ((ar ai a)
                      (br bi b)
                      (x (+ (mul br br) (mul bi bi)))
                      (r (div (+ (mul ar br) (mul ai bi)) x))
                      (i (div (- (mul ai br) (mul ar bi)) x)))
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a)
                      (x (mul b b))
                      (r (div (mul ar b) x))
                      (i (div (mul ai b) x)))
                     (if (eq? i 0) r (complex r i)))))
            ((eq? (type b) type-complex)
               (lets
                  ((br bi b)
                   (x (+ (mul br br) (mul bi bi)))
                   (re (div (mul a br) x))
                  (im (div (- 0 (mul a bi)) x)))
                  (if (eq? im 0) re (complex re im))))
            ((eq? (type a) type-rational)
               (if (eq? (type b) type-rational)
                  ; a'/a" / b'/b" = a'b" / a"b'
                  (div
                     (mul (ncar a) (ncdr b))
                     (mul (ncdr a) (ncar b)))
                  ; a'/a" / b = a'/ba"
                  (divide (ncar a) (mul (ncdr a) b))))
            ((eq? (type b) type-rational)
               ; a / b'/b" = ab"/n
               (divide (mul a (ncdr b)) (ncar b)))
            (else
               (divide a b))))

   (define (conjugate a)
      (if (eq? (type a) type-complex)
         (complex
            (ref a 1)
            (mul -1 (ref a 2)))
         a))

   (define / div)

   (define * mul)

   (define (negate a)
      (or
         (inegate a)
         (rnegate a)
         (* a -1)))

   (example
      (conjugate 1+2/3i) = 1-2/3i
      (+ 0.5+0.1i 1/2+9/10i) = 1+i)

))
