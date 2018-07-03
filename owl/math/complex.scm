#| doc
This library defines complex arbitrary precision math functions.
|#


(define-library (owl math complex)

   (export
      number? fixnum? integer?
      + - * = /
      << < <= = >= > >>
      band bior bxor
      quotient ediv truncate/
      add sub mul big-bad-args negate
      even? odd?
      gcd gcdl lcm
      min max minl maxl
      floor ceiling abs
      sum product
      numerator denumerator
      log log2
      render-number
      zero?
      real? complex? rational?
      negative? positive?
      denominator numerator
      remainder modulo
      truncate round
      rational complex
      ncons ncar ncdr
      fx-greatest fx-width
      )

   (import

      (owl defmac)
      (owl list)
      (owl syscall)
      (owl ff)
      (only (owl primop) create-type)

      (prefix ;; prefix integer operations with i
         (only (owl math integer) + - * = < << >> rem mod)
         i)

      (only (owl math integer)
         ncons ncar ncdr big-bad-args
         big-digits-equal? negate
         negative? quotient ediv
         to-int- to-int+ to-fix+ to-fix-
         fx-greatest fx-width truncate/
         zero? positive? even? odd?
         integer? fixnum?
         make-+ make-- right-out
         << >> band bior bxor))


   (begin

      (define-syntax rational
         (syntax-rules ()
            ((rational a b) (mkt type-rational a b))))

      ;; todo: rational comparison is dumb.. first one should check the signs, then whether log_2(ab') < log_2(ba'), which is way faster than multiplication, and only as a last resort do the actual multiplication. also, more common comparisons should be inlined here.
      (define (< a b)
         (cond
            ; add short type paths here later
            ((eq? (type a) type-rational)
               (if (eq? (type b) type-rational)
                  ; a/a' < b/b' <=> ab' < ba'
                  (i< (i*(ncar a) (ncdr b)) (i* (ncar b) (ncdr a)))
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

      (define (min a b) (if (< a b) a b))
      (define (max a b) (if (< a b) b a))

      (define (minl as) (fold min (car as) (cdr as)))
      (define (maxl as) (fold max (car as) (cdr as)))

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

      ;; FIXME - consider carrying these instead
      ;; FIXME depends on fixnum size
      (define gcd-shifts
         (list->ff
            (map (lambda (x) (cons (<< 1 x) x))
               '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23))))


      (define (lazy-gcd a b n)
         (let ((av (cdr a)) (bv (cdr b)))
            (cond
               ((eq? av 0) (<< bv n))
               ((eq? bv 0) (<< av n))
               ((eq? (band av (car a)) 0) ; a even
                  (if (eq? (band bv (car b)) 0) ; a and b even
                     (lazy-gcd (gcd-drop a) (gcd-drop b) (i+ n 1))
                     (lazy-gcd (gcd-drop a) b n)))
               ((eq? (band bv (car b)) 0) ; a is odd, u is even
                  (lazy-gcd a (gcd-drop b) n))
               (else
                  (lets
                     ((av (>> av (get gcd-shifts (car a) 0)))
                      (bv (>> bv (get gcd-shifts (car b) 0)))
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
            ; negates should be inlined
            ((eq? (type a) type-fix-) (gcd (negate a) b))
            ((eq? (type a) type-int-) (gcd (negate a) b))
            ((eq? (type b) type-fix-) (gcd a (negate b)))
            ((eq? (type b) type-int-) (gcd a (negate b)))
            ((eq? (type a) type-fix+) (gcd-euclid a b))
            ((eq? (type b) type-fix+) (gcd-euclid a b))
            ((eq? a b) a)
            (else (nat-gcd a b))))

      (define (gcdl ls) (fold gcd (car ls) (cdr ls)))

      (define-syntax complex
         (syntax-rules ()
            ((complex a b) (mkt type-complex a b))))

      ; normalize, fix sign and construct rational
      (define (rationalize a b)
         (let ((f (gcd a b)))
            (if (eq? f 1)
               (cond
                  ((eq? (type b) type-fix-) (rational (negate a) (negate b)))
                  ((eq? (type b) type-int-) (rational (negate a) (negate b)))
                  (else (rational a b)))
               (rationalize (quotient a f) (quotient b f)))))

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
                     ((= f b)
                        (ediv a f))
                     (else
                        (rational
                           (ediv a f)
                           (ediv b f))))))))



      ;;;
      ;;; To-be (owl rational)
      ;;;

      ;; addition

      (define (mk-rational-add complex-no) ;; <- to be split to (owl rational)
         (make-+
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

      (define r+
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
         (make--
            (rational-sub-case complex-no)))

      (define r-
         (mk-rational-sub right-out))

      ;;; Complex code continues

      ;; we assume these are the last cases for now,
      ;; but this could also be generalized further to vectors and matrices later

      (define c+
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

      (define add c+)

      (define (complex-sub ar ai br bi)
         (let ((i (r- ai bi)))
            (if (eq? i 0)
               (r- ar br)
               (complex (r- ar br) i))))

      (define c-
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

      (define sub c-)

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
                      (r (sub (mul ar br) (mul ai bi)))
                      (i (add (mul ai br) (mul ar bi))))
                     (if (eq? i 0) r (complex r i)))
                  (lets
                     ((ar ai a)
                      (r (mul ar b))
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
                      (x (add (mul br br) (mul bi bi)))
                      (r (div (add (mul ar br) (mul ai bi)) x))
                      (i (div (sub (mul ai br) (mul ar bi)) x)))
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
                   (x (add (mul br br) (mul bi bi)))
                   (re (div (mul a br) x))
                   (im (div (sub 0 (mul a bi)) x)))
                  (if (eq? im 0) re (complex re im))))
            ((eq? (type a) type-rational)
               (if (eq? (type b) type-rational)
                  ; a'/a" / b'/b" = a'b" / a"b'
                  (divide
                     (mul (ncar a) (ncdr b))
                     (mul (ncdr a) (ncar b)))
                  ; a'/a" / b = a'/ba"
                  (divide (ncar a) (mul (ncdr a) b))))
            ((eq? (type b) type-rational)
               ; a / b'/b" = ab"/n
               (divide (mul a (ncdr b)) (ncar b)))
            (else
               (divide a b))))


      ;;;
      ;;; Basic math extra stuff
      ;;;

      (define (abs n)
         (case (type n)
            (type-fix+ n)
            (type-fix- (to-fix+ n))
            (type-int+ n)
            (type-int- (to-int+ n))
            (type-rational (if (negative? n) (sub 0 n) n))
            (else (error "bad math: " (list 'abs n)))))

      (define (floor n)
         (if (eq? (type n) type-rational)
            (lets ((a b n))
               (if (negative? a)
                  (negate (i+ (quotient (abs a) b) 1))
                  (quotient a b)))
            n))

      (define (ceiling n)
         (if (eq? (type n) type-rational)
            (lets ((a b n))
               (if (negative? a)
                  (quotient a b)
                  (i+ (floor n) 1)))
            n))

      (define (truncate n)
         (if (eq? (type n) type-rational)
            (lets ((a b n))
               (if (negative? a)
                  (negate (quotient (negate a) b))
                  (quotient a b)))
            n))

      (define (round n)
         (if (eq? (type n) type-rational)
            (lets ((a b n))
               (if (eq? b 2)
                  (if (negative? a)
                     (>> (sub a 1) 1)
                     (>> (i+ a 1) 1))
                  (quotient a b)))
            n))

      (define (sum l) (fold add (car l) (cdr l)))
      (define (product l) (fold mul (car l) (cdr l)))


      ; for all numbers n == (/ (numerator n) (denumerator n))

      (define (numerator n)
         (case (type n)
            (type-rational (ncar n))
            (else n)))

      (define (denumerator n)
         (case (type n)
            (type-rational (ncdr n))
            (else 1)))



      ;;;
      ;;; logarithms, here meaning (log n a) = m, being least natural number such that n^m >= a
      ;;;

      ;; naive version, multiply successively until >=
      (define (log-loop n a m i)
         (if (< m a)
            (log-loop n a (mul m n) (add i 1))
            i))

      ;; least m such that n^m >= a
      (define (log-naive n a)
         (log-loop n a 1 0))

      ;; same, but double initial steps (could recurse on remaining interval, cache steps etc for further speedup)
      (define (logd-loop n a m i)
         (if (< m a)
            (let ((mm (mul m m)))
               (if (< mm a)
                  (logd-loop n a mm (add i i))
                  (log-loop n a (mul m n) (add i 1))))
            i))

      (define (logn n a)
         (cond
            ((>= 1 a) 0)
            ((< a n) 1)
            (else (logd-loop n a n 1))))

      ;; special case of log2

      ; could do in 8 comparisons with a tree
      (define (log2-fixnum n)
         (let loop ((i 0))
            (if (< (<< 1 i) n)
               (loop (add i 1))
               i)))

      (define (log2-msd n)
         (let loop ((i 0))
            (if (<= (<< 1 i) n)
               (loop (add i 1))
               i)))

      (define (log2-big n digs)
         (let ((tl (ncdr n)))
            (if (null? tl)
               (add (log2-msd (ncar n)) (mul digs fx-width))
               (log2-big tl (add digs 1)))))

      (define (log2 n)
         (cond
            ((eq? (type n) type-int+) (log2-big (ncdr n) 1))
            ((eq? (type n) type-fix+)
               (if (< n 0) 1 (log2-fixnum n)))
            (else (logn 2 n))))

      (define (log n a)
         (cond
            ((eq? n 2) (log2 a))
            ((<= n 1) (big-bad-args 'log n a))
            (else (logn n a))))

      ;(import-old lib-test)
      ;(test
      ;   (lmap (λ (i) (lets ((rst n (rand i 10000000000000000000))) n)) (lnums 1))
      ;   (H log-naive 3)
      ;   (H log 3))
      ;(import-old lib-test)
      ;(test
      ;   (lmap (λ (i) (lets ((rst n (rand i #x20000))) n)) (lnums 1))
      ;   (H log 2)
      ;   log2)

      ; note: It is safe to use quotient, which is faster for bignums, because by definition
      ; the product is divisible by the gcd. Also, gcd 0 0 is not safe, but since (lcm
      ; a a) == a, handling this special case and a small optimization overlap nicely.

      (define (lcm a b)
         (if (eq? a b)
            a
            (quotient (abs (mul a b)) (gcd a b))))


      ;;;
      ;;; Rendering numbers
      ;;;

      (define (char-of digit)
         (add digit (if (< digit 10) 48 87)))

      (define (render-digits num tl base)
         (fold (λ (a b) (cons b a)) tl
            (unfold (λ (n) (lets ((q r (truncate/ n base))) (values (char-of r) q))) num zero?)))

      ;; move to math.scm

      (define (render-number num tl base)
         (cond
            ((eq? (type num) type-rational)
               (render-number (ref num 1)
                  (cons 47
                     (render-number (ref num 2) tl base))
                  base))
            ((eq? (type num) type-complex)
               ;; todo: imaginary number rendering looks silly, written in a hurry
               (lets ((real imag num))
                  (render-number real
                     (cond
                        ((eq? imag 1) (ilist #\+ #\i tl))
                        ((eq? imag -1) (ilist #\- #\i tl))
                        ((< imag 0) ;; already has sign
                           (render-number imag (cons #\i tl) base))
                        (else
                           (cons #\+
                              (render-number imag (cons #\i tl) base))))
                     base)))
            ((< num 0)
               (cons 45
                  (render-number (sub 0 num) tl base)))
            ((< num base)
               (cons (char-of num) tl))
            (else
               (render-digits num tl base))))




      ;;;
      ;;; Variable arity versions
      ;;;

      ;; FIXME: these need short circuiting

      ;; + → add
      (define +
         (case-lambda
            ((a b) (add a b))
            (xs (fold add 0 xs))))

      ;; - → sub
      (define -
         (case-lambda
            ((a b) (sub a b))
            ((a) (sub 0 a))
            ((a b . xs)
               (sub a (fold add b xs)))))

      ;; * → mul
      (define *
         (case-lambda
            ((a b) (mul a b))
            ((a b . xs) (mul a (fold mul b xs)))
            ((a) a)
            (() 1)))

      (define /
         (case-lambda
            ((a b) (div a b))
            ((a) (div 1 a))
            ((a . bs) (div a (product bs)))))

      ;; fold but stop on first false
      (define (each op x xs)
         (cond
            ((null? xs) #true)
            ((op x (car xs))
               (each op (car xs) (cdr xs)))
            (else #false)))

      ;; the rest are redefined against the old binary ones

      (define (vararg-predicate op) ;; turn into a macro
         (case-lambda
            ((a b) (op a b))
            ((a . bs) (each op a bs))))

      (define = (vararg-predicate =)) ;; short this later
      (define < (vararg-predicate <))
      (define > (vararg-predicate >))

      (define <= (vararg-predicate <=))
      (define >= (vararg-predicate >=))

      ;; ditto for foldables
      (define (vararg-fold op zero)
         (case-lambda
            ((a b) (op a b))
            ((a) a)
            ((a . bs) (fold op a bs))
            (() (or zero (error "No arguments for " op)))))

      (define min (vararg-fold min #false))
      (define max (vararg-fold max #false))
      (define gcd (vararg-fold gcd 0))
      (define lcm (vararg-fold lcm 1))

      (define remainder irem)
      (define modulo imod)

      (define (number? a)
         (case (type a)
            (type-fix+ #true)
            (type-fix- #true)
            (type-int+ #true)
            (type-int- #true)
            (type-rational #true)
            (type-complex #true)
            (else #false)))

      ;; RnRS compat
      (define real? number?)
      (define complex? number?)
      (define rational? number?)

      (define (negate x)
         (mul -1 x))
))
