#| doc
This library defines arbitrary precision rational arithmetic operations.
|#


(define-library (owl math rational)

   (export
      mk-rational-add
      mk-rational-sub
      + - < /
      ;; = should be here also
      gcd gcdl
      rational
      numerator denumerator
      divide)

   (import
      (owl defmac)
      (owl ff) ;; used in gcd
      (owl list)
      (only (owl syscall) error)
      (prefix (only (owl math integer) << >> < + - * = rem quotient ediv =) i)
      (only (owl math integer) ncar ncdr band negate negative?)
      (only (owl math integer)
         mk-add mk-sub right-out))

   (begin

      (define-syntax rational
         (syntax-rules ()
            ((rational a b) (mkt type-rational a b))))

      (define (< a b)
         (cond
            ; add short type paths here later
            ((eq? (type a) type-rational)
               (if (eq? (type b) type-rational)
                  ; a/a' < b/b' <=> ab' < ba'
                  (i< (i* (ncar a) (ncdr b)) (i* (ncar b) (ncdr a)))
                  ; a/a' < b <=> a < ba'
                  (i< (ncar a) (i* b (ncdr a)))))
            ((eq? (type b) type-rational)
               ; a < b/b' <=> ab' < b
               (i< (i* a (ncdr b)) (ncar b)))
            (else
               (i< a b))))

      (define (denominator n)
         (if (eq? (type n) type-rational)
            (ncdr n)  ;; always positive
            1))

      ;;;
      ;;; GCD (lazy binary new)
      ;;;

      ;; Euclid's gcd
      (define (gcd-euclid a b)
         (if (eq? b 0)
            a
            (gcd-euclid b (irem a b))))

      ;; lazy gcd

      ; O(1), shift focus bit
      (define (gcd-drop n)
         (let ((s (car n)))
            (cond
               ((eq? s #x800000)
                  (let ((n (cdr n)))
                     ; drop a digit or zero
                     (if (eq? (type n) type-fix+)
                        (cons 1 0)
                        (let ((tl (ncdr n)))
                           (if (null? (ncdr tl))
                              (cons 1 (ncar tl))
                              (cons 1 tl))))))
               (else
                  (lets ((lo _ (fx+ s s)))
                     (cons lo (cdr n)))))))

      ;; FIXME depends on fixnum size
      (define gcd-shifts
         (list->ff
            (map (lambda (x) (cons (i<< 1 x) x))
               '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))))

      (define (lazy-gcd a b n)
         (let ((av (cdr a)) (bv (cdr b)))
            (cond
               ((eq? av 0) (i<< bv n))
               ((eq? bv 0) (i<< av n))
               ((eq? (band av (car a)) 0) ; a even
                  (if (eq? (band bv (car b)) 0) ; a and b even
                     (lazy-gcd (gcd-drop a) (gcd-drop b) (i+ n 1))
                     (lazy-gcd (gcd-drop a) b n)))
               ((eq? (band bv (car b)) 0) ; a is odd, u is even
                  (lazy-gcd a (gcd-drop b) n))
               (else
                  (lets
                     ((av (i>> av (get gcd-shifts (car a) 0)))
                      (bv (i>> bv (get gcd-shifts (car b) 0)))
                      (x (i- av bv)))
                     (if (negative? x)
                        (lazy-gcd (cons 2 (negate x)) (cons 1 av) n)
                        (lazy-gcd (cons 2 x) (cons 1 bv) n)))))))

      ;; why are the bit values consed to head of numbers?
      (define (nat-gcd a b) (lazy-gcd (cons 1 a) (cons 1 b) 0)) ;; FIXME - does not yet work with variable fixnum size
      ;(define nat-gcd gcd-euclid)

      ;; signed wrapper for nat-gcd
      (define (gcd a b)
         (cond
            ((eq? (type a) type-fix-) (gcd (negate a) b))
            ((eq? (type a) type-int-) (gcd (negate a) b))
            ((eq? (type b) type-fix-) (gcd a (negate b)))
            ((eq? (type b) type-int-) (gcd a (negate b)))
            ((eq? (type a) type-fix+) (gcd-euclid a b))
            ((eq? (type b) type-fix+) (gcd-euclid a b))
            ((eq? a b) a)
            (else (nat-gcd a b))))

      (define (gcdl ls) (fold gcd (car ls) (cdr ls)))

      ; normalize, fix sign and construct rational
      (define (rationalize a b)
         (let ((f (gcd a b)))
            (if (eq? f 1)
               (cond
                  ((eq? (type b) type-fix-) (rational (negate a) (negate b)))
                  ((eq? (type b) type-int-) (rational (negate a) (negate b)))
                  (else (rational a b)))
               (rationalize (iquotient a f) (iquotient b f)))))

      ;; if dividing small fixnums, do it with primops
      (define (divide-simple a b)
         (if (eq? (type b) type-fix+) ; negative (if any) always at a
            (cond
               ((eq? (type a) type-fix+)
                  (lets ((_ q r (fxqr 0 a b)))
                     (if (eq? r 0)
                        q
                        #false)))
               (else #false))
            #false))

      (define (divide a b)
         (cond
            ((eq? (type b) type-fix-) (divide (negate a) (negate b)))
            ((eq? (type b) type-int-) (divide (negate a) (negate b)))
            ((divide-simple a b) => self)
            (else
               (let ((f (gcd a b)))
                  (cond
                     ((eq? f 1)
                        (if (eq? b 1)
                           a
                           (rational a b)))
                     ((i= f b)
                        (iediv a f))
                     (else
                        (rational
                           (iediv a f)
                           (iediv b f))))))))

      (define (mk-rational-add complex-no)
         (mk-add
            (λ (a b)
               (if (eq? (type a) type-rational)
                  (case (type b)
                     (type-rational
                        ; a'/a" + b'/b" = a'b" + b'a" / a"b"
                        (let ((ad (ncdr a)) (bd (ncdr b)))
                           (if (eq? ad bd)
                              ; a/x + b/x = (a+b)/x, x within fixnum range
                              (divide (i+ (ncar a) (ncar b)) ad)
                              (let ((an (ncar a)) (bn (ncar b)))
                                 (divide
                                    (i+ (i* an bd) (i* bn ad))
                                    (i* ad bd))))))
                     (type-complex
                        (complex-no b a))
                     (else
                        ; a'/a" + b = (a'+ba")/a"
                        (rational (i+ (ncar a) (i* b (ncdr a))) (ncdr a))))
                  (complex-no a b)))))

      (define +
         (mk-rational-add right-out))

      ;; substraction

      ; a/b - c = (a - bc)/b
      (define (rat-int ra c)
         (lets ((a b ra))
            (rational (i- a (i* b c)) b)))

      ; c - a/b  = (cb - a)/b
      (define (int-rat c r)
         (lets ((a b r))
            (rational (i- (i* b c) a) b)))

      (define (rational-sub-case no)
         (let
            ((rsub
               (λ (a b r)
                  (cond
                     ((eq? (type a) type-rational)
                        ;; a'/a" - ?
                        (case (type b)
                           (type-rational
                              ; a'/a" - b'/b" = a'b" - b'a" / a"b"
                              (let ((ad (ncdr a)) (bd (ncdr b)))
                                 (if (eq? ad bd)
                                    (divide (i- (ncar a) (ncar b)) ad)
                                    (let ((an (ncar a)) (bn (ncar b)))
                                       (divide
                                          (i- (i* an bd) (i* bn ad))
                                          (i* ad bd))))))
                           (type-fix+ (rat-int a b))
                           (type-fix- (rat-int a b))
                           (type-int+ (rat-int a b))
                           (type-int- (rat-int a b))
                           (else (no a b))))
                     ((eq? (type b) type-rational)
                        (case (type a)
                           (type-fix+ (int-rat a b))
                           (type-fix- (int-rat a b))
                           (type-int+ (int-rat a b))
                           (type-int- (int-rat a b))
                           (else (no a b))))
                     (else
                        (no a b))))))
            (λ (a b)
               (rsub a b rsub))))

      (define (mk-rational-sub complex-no)
         (mk-sub
            (rational-sub-case complex-no)))

      (define -
         (mk-rational-sub right-out))

      (define (numerator n)
         (case (type n)
            (type-rational (ncar n))
            (else n)))

      (define (denumerator n)
         (case (type n)
            (type-rational (ncdr n))
            (else 1)))

      (define / divide)

))
