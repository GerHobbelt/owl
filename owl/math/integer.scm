#| doc
Bignums

This library defines arbitrary precision integer arithmetic. Some of the
functions are only defined for integers, whereas others are typically
extended to handle also more complex kinds of numbers.

Operations to be extended typically export both the operation defined for
integers only, and a corresponding mk-* version which calls a given function
in case the types of inputs are not integers. This allows extending the
definitions in other modules while checking the most common cases of integer
inputs in first branches.

Owl currently has 24 bits available for encoding a value in an immediate
value, meaning a value that does not have to be stored in memory. A fixnum,
or a fixed size number, is a number which fits these bits. The sign of a
fixnum is stored the type of the immediate object.

When a number is bigger or smaller than the maximum fixnum, it is converted
to an allocated integer. In this case the representation of the number is a
linked sequence of base 2²⁴ digits of the number starting from the least
significant digits. Again the sign of the number is stored in the type.

A valid fixnum zero is always positive, and a valid negative bignum has the
negative type only at the root node.
|#

(define-library (owl math integer)

   (export
      fixnum? integer?
      mk-add mk-sub
      + - * = /
      << < <= = >= > >>
      band bior bxor
      quotient ediv truncate/
      nat-succ big-bad-args negate
      ncar ncdr
      even? odd?
      zero?
      negative? positive?
      rem mod
      ncons ncar ncdr
      big-digits-equal?
      fx-greatest fx-width
      to-int- to-int+
      to-fix+ to-fix-
      add-big sub-big
      right-out
      )

   (import
      (owl core)
      (owl list)
      (owl syscall)
      )

   (begin

      (define zero?
         (C eq? 0))

      ;; get the largest value the VM supports in fixnum
      ;; idea is to allow the vm to be compiled with different ranges, initially fixed to 24
      (define fx-greatest
         (lets ((m _ (fx- 0 1)))
            m))

      ;; biggest before highest bit is set (needed in some bignum ops)
      (define fx-greatest>>1
         (lets ((f _ (fx>> fx-greatest 1)))
            f))

      ;; count the number of bits the VM supports in fixnum
      (define fx-width
         (let loop ((f fx-greatest) (n 0))
            (if (eq? f 0)
               n
               (lets
                  ((f _ (fx>> f 1))
                   (n _ (fx+ n 1)))
                  (loop f n)))))

      (define-syntax ncons
         (syntax-rules ()
            ((ncons a d) (mkt type-int+ a d))))

      (define-syntax ncar
         (syntax-rules ()
            ((ncar n) (ref n 1))))

      (define-syntax ncdr
         (syntax-rules ()
            ((ncdr n) (ref n 2))))

      (define-syntax to-fix+
         (syntax-rules ()
            ((to-fix+ n) (fxxor 0 n))))

      (define to-fix-
         (H fxxor (create-type type-fix-)))

      (define (to-int+ n)
         (mkt type-int+ (ncar n) (ncdr n)))

      (define (to-int- n)
         (mkt type-int- (ncar n) (ncdr n)))

      (define *big-one*
         (ncons 1 #n))

      (define *first-bignum*
         (ncons 0 *big-one*))

      (define (fixnum? x)
         (eq? (fxior (type x) type-fix-) type-fix-))

      (define (big-bad-args op a b)
         (error "Bad math:" (list op a b)))

      (define (big-unimplemented op a b)
         (error "Math too high:" (list op a b)))

      ;; negate val → -val | #f
      (define (negate num)
         (case (type num)
            (type-fix+
               (if (eq? num 0)
                  0
                  (to-fix- num)))     ;; a -> -a
            (type-fix- (to-fix+ num)) ;; -a -> a
            (type-int+ (to-int- num)) ;; A -> -A
            (type-int- (to-int+ num)) ;; -A -> A
            (else #f)))

      (define (right-out a b)
         (error "bad math: " (list a b)))

      (define (nrev-walk num to)
         (if (null? num)
            to
            (nrev-walk
               (ncdr num)
               (ncons (ncar num) to))))

      (define-syntax nrev
         (syntax-rules ()
            ((nrev num)
               (nrev-walk num #n))))

      (define (negative? a)
         (case (type a)
            (type-fix+ #false)
            (type-fix- #true)
            (type-int+ #false)
            (type-int- #true)
            (type-rational ;; fixme - move elsewhere
               (case (type (ncar a))
                  (type-fix+ #false)
                  (type-fix- #true)
                  (type-int+ #false)
                  (type-int- #true)
                  (else (error "Bad number: " a))))
            (else (error 'negative? a))))

      (define positive?
         (B not negative?))

      (define (integer? a)
         (case (type a)
            (type-fix+ #true)
            (type-fix- #true)
            (type-int+ #true)
            (type-int- #true)
            (else #false)))


      ;;;
      ;;; COMPARISON
      ;;;

      (define (big-digits-equal? a b)
         (cond
            ((eq? a b) #true)      ; shared tail or both empty
            ((null? a) #false)
            ((null? b) #false)
            ((eq? (ncar a) (ncar b))
               (big-digits-equal? (ncdr a) (ncdr b)))
            (else #false)))

      (define (big-less a b lower)
         (cond
            ((eq? a b)    ; both ended or shared tail
               lower)
            ((null? a) #true)
            ((null? b) #false)
            (else
               (let ((ad (ncar a)) (bd (ncar b)))
                  (cond
                     ((lesser? ad bd)
                        (big-less (ncdr a) (ncdr b) #true))
                     ((eq? ad bd)
                        (big-less (ncdr a) (ncdr b) lower))
                     (else
                        (big-less (ncdr a) (ncdr b) #false)))))))

      ;; fixnum/integer <
      (define (< a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+   (lesser? a b))
                  (type-int+ #true)
                  (else #false)))
            (type-fix-
               (case (type b)
                  (type-fix+ #true)
                  (type-fix-
                     (if (eq? a b)
                        #false
                        (lesser? b a)))
                  (type-int+ #true)
                  (else #false)))
            (type-int+
               (case (type b)
                  (type-int+ (big-less a b #false))
                  (else #false)))
            (type-int-
               (case (type b)
                  (type-int-
                     (if (big-less a b #false) #false #true))
                  (else #true)))
            (else
               (big-bad-args '< a b))))

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
            (else
               (big-bad-args '= a b))))


      (define (> a b) (< b a))

      (define (<= a b)
         (or (< a b) (= a b)))

      (define (>= a b) (<= b a))



      ;;;
      ;;; ADDITION AND SUBSTRACTION
      ;;;

      (define (nat-succ n)
         (let ((t (type n)))
            (cond
               ((eq? t type-fix+)
                  (if (eq? n fx-greatest)
                     *first-bignum*
                     (lets ((n _ (fx+ n 1))) n)))
               ((eq? t type-int+)
                  (let ((lo (ncar n)))
                     (if (eq? lo fx-greatest)
                        (ncons 0 (nat-succ (ncdr n)))
                        (lets ((lo _ (fx+ lo 1)))
                           (ncons lo (ncdr n))))))
               ((null? n)
                  *big-one*)
               (else
                  (big-bad-args 'inc n n)))))

      (define (nlen n)
         (cond
            ((eq? (type n) type-fix+) 1)
            ((eq? (type n) type-fix-) 1)
            (else
               (let loop ((n n) (i 0))
                  (if (null? n)
                     i
                     (loop (ncdr n) (nat-succ i)))))))

      (define (add-number-big a big)
         (lets
            ((b bs big)
             (new overflow (fx+ a b)))
            (if (eq? overflow 0)
               (ncons new bs)
               (if (null? bs)
                  (ncons new *big-one*)
                  (ncons new (add-number-big overflow bs))))))

      (define (add-big a b carry)
         (cond
            ((null? a)
               (if (null? b)
                  (if (eq? carry 0) #n *big-one*)
                  (if (eq? carry 0) b (add-number-big carry b))))
            ((null? b)
               (if (eq? carry 0) a (add-number-big carry a)))
            (else
               (lets ((r o (fx+ (ncar a) (ncar b))))
                  (if (eq? carry 0)
                     (ncons r (add-big (ncdr a) (ncdr b) o))
                     (lets ((r o2 (fx+ r carry)))
                        (ncons r (add-big (ncdr a) (ncdr b) (fxior o o2)))))))))

      (define-syntax sub-small->pick-sign
         (syntax-rules ()
            ((sub-small->pick-sign a b)
               (lets ((r u (fx- a b)))
                  (if (eq? u 0)
                     r
                     (lets ((r _ (fx- b a))) ;; could also fix here by adding or bitwise
                        (to-fix- r)))))))

      ; bignum - fixnum -> either
      (define (sub-big-number a b leading?)
         (lets ((r underflow (fx- (ncar a) b)))
            (cond
               ((not (eq? underflow 0))
                  (let ((tail (sub-big-number (ncdr a) underflow #f)))
                     (cond
                        (tail (ncons r tail))   ; otherwise tail went to 0
                        (leading? r)
                        ((eq? r 0) #false)
                        (else (ncons r #n)))))
               ((eq? r 0)
                  (let ((tail (ncdr a)))
                     (if (null? tail)
                        (if leading? r #false)
                        (ncons r tail))))
               (else
                  (ncons r (ncdr a))))))

      ; a - B = a + -B = -(-a + B) = -(B - a)

      (define (sub-number-big a b first?)
         (let ((res (sub-big-number b a #true)))
            ; res is either fixnum or bignum
            (case (type res)
               (type-fix+ (to-fix- res))
               (else (to-int- res)))))


      ; substract from a, which must be bigger

      (define (sub-digits a b borrow leading?)
         (cond
            ((null? a)
               #false)
            ((null? b)
               (if (eq? borrow 0)
                  a
                  (sub-big-number a borrow leading?)))
            (else
               (lets ((r u (fx- (ncar a) (ncar b))))
                  (if (not (eq? borrow 0))
                     (lets ((r u2 (fx- r 1)))
                        (let ((tail (sub-digits (ncdr a) (ncdr b) (fxior u u2) #f)))
                           (cond
                              (tail (ncons r tail))
                              (leading? r)
                              ((eq? r 0) #false)
                              (else (ncons r #n)))))

                     (let ((tail (sub-digits (ncdr a) (ncdr b) u #f)))
                        (cond
                           (tail (ncons r tail))
                           (leading? r)
                           ((eq? r 0) #false)
                           (else (ncons r #n)))))))))


      ; A - B = -(B - A)

      (define (sub-big a b)
         (cond
            ((big-less a b #false)
               (let ((neg (sub-digits b a 0 #t)))
                  (cond
                     ((eq? neg 0) neg)
                     ((eq? (type neg) type-fix+) (to-fix- neg))
                     (else (to-int- neg)))))
            (else
               (sub-digits a b 0 #t))))

      ; add bits, output is negative

      (define (add-small->negative a b)
         (lets ((r overflow (fx+ a b)))
            (if (eq? overflow 0)
               (to-fix- r)
               (mkt type-int- r *big-one*))))

      ; for changing the (default positive) sign of unsigned operations
      (define-syntax negative
         (syntax-rules (if)
            ((negative (op . args))
               (let ((foo (op . args)))
                  (negative foo)))
            ((negative x)
               (if (eq? (type x) type-fix+)
                  (to-fix- x)
                  (to-int- x)))))

      (define-syntax add-small->positive
         (syntax-rules ()
            ((add-small->positive a b)
               (lets ((r overflow (fx+ a b)))
                  (if (eq? overflow 0) r (ncons r *big-one*))))))

      (define (mk-sub no)
         (λ (a b)
            (case (type a)
               (type-fix+ ; a signed fixnum
                  (case (type b)
                     (type-fix+ (sub-small->pick-sign a b))      ;; +a - +b -> as +a + -b
                     (type-fix- (add-small->positive a b))       ;; +a - -b -> as +a + +b
                     (type-int+ (sub-number-big a b #true))      ;; +a - +B -> as +a + -B
                     (type-int- (add-number-big a b))            ;; +a - -B -> as +a + +B
                     (else (no a b))))
               (type-fix-
                  (case (type b)
                     (type-fix+ (add-small->negative a b))       ;; -a - +b -> as -a + -b
                     (type-fix- (sub-small->pick-sign b a))      ;; -a - -b -> as -a + +b
                     (type-int+ (to-int- (add-number-big a b)))  ;; -a - +B -> as -a + -B
                     (type-int- (sub-big-number b a #true))      ;; -a - -B -> as -a + +B
                     (else (no a b))))
               (type-int+
                  (case (type b)
                     (type-fix+ (sub-big-number a b #true))      ;; +A - +b -> as +A + -b
                     (type-fix- (add-number-big b a))            ;; +A - -b -> as +A + +b
                     (type-int+ (sub-big a b))                   ;; +A - +B -> as +A + -B
                     (type-int- (add-big a b 0))                 ;; +A - -B -> as +A + +B
                     (else (no a b))))
               (type-int-
                  (case (type b)
                     (type-fix+ (to-int- (add-number-big b a))) ;; -A - +b -> as -A + -b
                     (type-fix- (sub-number-big b a #true))     ;; -A - -b -> as -A + +b
                     (type-int+ (to-int- (add-big a b 0)))      ;; -A - +B -> as -A + -B
                     (type-int- (sub-big b a))                  ;; -A - -B -> as -A + +B
                     (else (no a b))))
               (else
                  (no a b)))))

      (define - (mk-sub right-out))

      (define (mk-add no)
         (λ (a b)
            (case (type a)
               (type-fix+ ; a signed fixnum
                  (case (type b)
                     (type-fix+ (add-small->positive a b))            ;; +a + +b -> c | C
                     (type-fix- (sub-small->pick-sign a b))         ;; +a + -b -> +c | -c, underflow determines sign
                     (type-int+ (add-number-big a b))               ;; +a + +B -> +C
                     (type-int- (sub-number-big a b #true))         ;; +a + -B -> -c | -C
                     (else (no b a))))
               (type-fix-
                  (case (type b)
                     (type-fix+ (sub-small->pick-sign b a))         ;; -a + +b == +b + -a -> as above (no need to recurse)
                     (type-fix- (add-small->negative a b))         ;; -a + -b -> -c | -C
                     (type-int+ (sub-big-number b a #true))            ;; -a + +B == +B - +a -> sub-big-number
                     (type-int- (to-int- (add-number-big a b))) ;; -a + -B == -C == -(a + B)
                     (else (no b a))))
               (type-int+
                  (case (type b)
                     (type-fix+ (add-number-big b a))               ;; +A + +b -> +C
                     (type-fix- (sub-big-number a b #true))            ;; +A + -b == -b + +A -> as above
                     (type-int+ (add-big a b 0))                  ;; +A + +B == +C
                     (type-int- (sub-big a b))                     ;; +A + -B == +c | -c | +C | -C
                     (else (no b a))))
               (type-int-
                  (case (type b)
                     (type-fix+ (sub-number-big b a #true))            ;; -A + +b == +b + -A -> as above
                     (type-fix- (to-int- (add-number-big b a))) ;; -A + -b == -b + -A = -C -> as above
                     (type-int+ (sub-big b a))                     ;; -A + +B == +B + -A -> as above
                     (type-int- (to-int- (add-big a b 0))) ;; -A + -B == -(A + B)
                     (else (no b a))))
               (else
                  (no a b)))))

      (define + (mk-add right-out))




      ;;;
      ;;; BITWISE OPERATIONS
      ;;;

      ; fxand, fxior, fxxor -> result
      ; fx>> -> hi + lo

      (define (shift-right-walk this rest n first?)
         (if (null? rest)
            (cond
               (first?  this)
               ((eq? this 0) #false)
               (else
                  (ncons this #n)))
            (let ((next (ncar rest)))
               (lets ((hi lo (fx>> next n)))
                  (lets
                     ((this (fxior this lo))
                      (tail (shift-right-walk hi (ncdr rest) n #false)))
                     (cond
                        (tail (ncons this tail))
                        ((eq? this 0)
                           (if first? 0 #false))
                        (else
                           (if first? this
                              (ncons this #n)))))))))

      (define (shift-right a n)
         (if (null? a)
            0
            (lets ((hi lo (fx>> (ncar a) n)))
               (shift-right-walk hi (ncdr a) n #true))))

      ; words known to be fixnum
      (define (drop-digits a words)
         (cond
            ((eq? words 0) a)
            ((null? a) a)
            (else
               (lets ((words _ (fx- words 1)))
                  (drop-digits (ncdr a) words)))))

      ; optimize << and >> since they will be heavily used in subsequent ops

      (define (>> a b)
         (case (type b)
            (type-fix+
               (lets ((_ wor bits (fxqr 0 b fx-width)))
                  (if (eq? wor 0)
                     (case (type a)
                        (type-fix+ (receive (fx>> a bits) (lambda (hi lo) hi)))
                        (type-fix- (receive (fx>> a bits) (lambda (hi lo) (if (eq? hi 0) 0 (negate hi)))))
                        (type-int+ (shift-right a bits))
                        (type-int- (negative (shift-right a bits)))
                        (else (big-bad-args '>> a b)))
                     (case (type a)
                        (type-fix+ 0)
                        (type-fix- 0)
                        (type-int+ (shift-right (drop-digits a wor) bits))
                        (type-int-
                           (negative
                              (shift-right (drop-digits a wor) bits)))
                        (else (big-bad-args '>> a b))))))
            (type-int+
               ;; todo, use digit multiples instead or drop each digit
               (if (eq? a 0)
                  0 ;; terminate early if out of bits
                  (>> (ncdr a) (- b fx-width))))
            (else
               (big-bad-args '>> a b))))

      ; make a digit with last as low bits
      (define (shift-left num n last)
         (if (null? num)
            (if (eq? last 0)
               #n
               (ncons last #n))
            (lets ((hi lo (fx>> (ncar num) n)))
               (ncons (fxior last lo)
                  (shift-left (ncdr num) n hi)))))

      ; << quarantees n is a fixnum
      (define (extend-digits num n)
         (if (eq? n 0)
            num
            (lets ((n _ (fx- n 1)))
               (extend-digits (ncons 0 num) n))))

      ; fixme: a << b = a * 2^b for other numbers
      ; thus #b0.0001 << 4 = 1

      (define (<< a b)
         (cond
            ((eq? a 0) 0)
            ((eq? (type b) type-fix+)
               (lets
                  ((_ words bits (fxqr 0 b fx-width))
                   (bits _ (fx- fx-width bits))) ; convert shift width for fx>>
                  (case (type a)
                     (type-fix+
                        (lets ((hi lo (fx>> a bits)))
                           (if (eq? hi 0)
                              (if (eq? words 0)
                                 lo
                                 (extend-digits (ncons lo #n) words))
                              (if (eq? words 0)
                                 (ncons lo (ncons hi #n))
                                 (extend-digits
                                    (ncons lo (ncons hi #n))
                                    words)))))
                     (type-fix-
                        (lets ((hi lo (fx>> a bits)))
                           (if (eq? hi 0)
                              (if (eq? words 0)
                                 (to-fix- lo)
                                 (to-int- (extend-digits (ncons lo #n) words)))
                              (to-int-
                                 (extend-digits
                                    (ncons lo (ncons hi #n)) words)))))
                     (type-int+
                        (extend-digits (shift-left a bits 0) words))
                     (type-int-
                        (to-int- (extend-digits (shift-left a bits 0) words)))
                     (else
                        (big-bad-args '<< a b)))))
            ((eq? (type b) type-int+)
               ;; not likely to happen though
               (<< (<< a fx-greatest) (- b fx-greatest)))
            (else
               ;; could allow negative shift left to mean a shift right, but that is
               ;; probably more likely an accident than desired behavior, so failing here
               (big-bad-args '<< a b))))

      (define (big-band a b)
         (cond
            ((null? a) 0)
            ((null? b) 0)
            (else
               (lets
                  ((this (fxand (ncar a) (ncar b)))
                   (tail (big-band (ncdr a) (ncdr b))))
                  (cond
                     ((eq? tail 0) this)
                     ((eq? (type tail) type-fix+)
                        (ncons this (ncons tail #n)))
                     (else
                        (ncons this tail)))))))

      ;; answer is quaranteed to be a bignum
      (define (big-bior a b)
         (cond
            ((null? a) b)
            ((null? b) a)
            (else
               (lets
                  ((this (fxior (ncar a) (ncar b)))
                   (tail (big-bior (ncdr a) (ncdr b))))
                  (ncons this tail)))))

      ;; → null | bignum
      (define (big-bxor-digits a b)
         (cond
            ((null? a) b)
            ((null? b) a)
            (else
               (lets
                  ((this (fxxor (ncar a) (ncar b)))
                   (tail (big-bxor-digits (ncdr a) (ncdr b))))
                  (if (null? tail)
                     (if (eq? this 0)
                        #n
                        (ncons this tail))
                     (ncons this tail))))))

      (define (big-bxor a b)
         (let ((r (big-bxor-digits a b)))
            (cond
               ;; maybe demote to fixnum
               ((null? r) 0)
               ((null? (ncdr r)) (ncar r))
               (else r))))

      ; not yet defined for negative
      (define (band a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+ (fxand a b))
                  (type-int+ (fxand a (ncar b)))
                  (else
                     (big-bad-args 'band a b))))
            (type-int+
               (case (type b)
                  (type-fix+
                     (fxand (ncar a) b))
                  (type-int+
                     (big-band a b))
                  (else
                     (big-bad-args 'band a b))))
            (else
               (big-bad-args 'band a b))))

      (define (even? n) (eq? 0 (band n 1)))
      (define (odd?  n) (eq? 1 (band n 1)))

      (define (bior a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+ (fxior a b))
                  (type-int+
                     (ncons (fxior a (ncar b))
                        (ncdr b)))
                  (else
                     (big-bad-args 'bior a b))))
            (type-int+
               (case (type b)
                  (type-fix+
                     (ncons (fxior b (ncar a))
                        (ncdr a)))
                  (type-int+
                     (big-bior a b))
                  (else
                     (big-bad-args 'bior a b))))
            (else
               (big-bad-args 'bior a b))))

      (define (bxor a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+ (fxxor a b))
                  (type-int+
                     (ncons (fxxor a (ncar b)) (ncdr b)))
                  (else
                     (big-bad-args 'bxor a b))))
            (type-int+
               (case (type b)
                  (type-fix+
                     (ncons (fxxor b (ncar a)) (ncdr a)))
                  (type-int+
                     (big-bxor a b))
                  (else
                     (big-bad-args 'bxor a b))))
            (else
               (big-bad-args 'bxor a b))))



      ;;;
      ;;; MULTIPLICATION
      ;;;

      ; O(n), basic multiply bignum b by fixnum a with carry

      (define (mult-num-big a b carry)
         (cond
            ((null? b)
               (if (eq? carry 0)
                  #n
                  (ncons carry #n)))
            ((eq? carry 0)
               (lets ((lo hi (fx* a (ncar b))))
                  (ncons lo
                     (mult-num-big a (ncdr b) hi))))
            (else
               (lets
                  ((lo hi (fx* a (ncar b)))
                   (lo o (fx+ lo carry))
                   (hi _ (fx+ hi o)))
                     (ncons lo (mult-num-big a (ncdr b) hi))))))

      ; O(1), fixnum multiply overflowing to bignum

      ;(define (mult-fixnums a b)
      ;   (receive (fx* a b)
      ;      (lambda (lo hi)
      ;         (if (eq? hi 0)
      ;            lo
      ;            (ncons lo (ncons hi #n))))))

      (define-syntax mult-fixnums
         (syntax-rules ()
            ((mult-fixnums a b)
               (lets ((lo hi (fx* a b)))
                  (if (eq? hi 0)
                     lo
                     (ncons lo (ncons hi #n)))))))


      ;;;
      ;;; Big multiplication
      ;;;

      ; current approach: karatsuba + schoolboy algorithm for small numbers

      ; ensure bigness
      (define (bigen x)
         (if (eq? (type x) type-fix+)
            (ncons x #n)
            x))

      ; a + (b << ex*16)
      (define (add-ext a b ex)
         (cond
            ((eq? ex 0) (if (null? a) (bigen b) (+ a b)))
            ((null? a)
               (ncons 0
                  (add-ext #n b (- ex 1))))
            ((eq? (type a) type-fix+) (add-ext (ncons a #n) b ex))
            ((eq? (type ex) type-fix+)
               (lets
                  ((ex _ (fx- ex 1))
                   (d ds a))
                  (ncons d (add-ext ds b ex))))
            (else
               (ncons (ncar a)
                  (add-ext (ncdr a) b (- ex 1))))))

      ; fixme, should just keep jumbo digits for for added versions and
      ;        perform the carrying just once in a final pass. add merges
      ;        and high parts (if any) of the digits are the carriables.
      ; can be used for small bignums
      (define (mul-simple a b)
         (if (null? a)
            #n
            (lets
               ((digit (ncar a))
                (head (ncons 0 (mul-simple (ncdr a) b)))
                (this (mult-num-big digit b 0)))
               (+ head this))))

      ; downgrade to fixnum if length 1
      (define (fix n)
         (if (null? (ncdr n)) (ncar n) n))

      ; drop leading zeros, reverse digits and downgrade to fixnum if possible
      (define (fixr n)
         (if (null? n)
            0
            (lets ((d ds n))
               (cond
                  ((null? ds) d)
                  ((eq? d 0) (fixr ds))
                  (else (nrev n))))))

      ; cut numbers from the midpoint of smaller while counting length (tortoise and hare akimbo)
      (define (splice-nums ah bh at bt rat rbt s? l)
         (cond
            ((null? ah)
               (values (fix at) (fixr rat) (fix bt) (fixr rbt) l))
            ((null? bh)
               (values (fix at) (fixr rat) (fix bt) (fixr rbt) l))
            (s?
               (splice-nums (ncdr ah) (ncdr bh) at bt rat rbt #false l))
            (else
               (lets
                  ((a at at)
                   (b bt bt)
                   (l _ (fx+ l 1))) ; fixme, no bignum len
                  (splice-nums (ncdr ah) (ncdr bh)
                     at bt (ncons a rat) (ncons b rbt) #true l)))))

      (define (kara a b)
         (cond
            ;; O(1) leaf cases
            ((eq? a 0) 0)
            ((eq? b 0) 0)
            ((eq? a 1) b)
            ((eq? b 1) a)

            ;; O(n) or O(1) leaf cases
            ((eq? (type a) type-fix+) (if (eq? (type b) type-fix+) (mult-fixnums a b) (mult-num-big a b 0)))
            ((eq? (type b) type-fix+) (mult-num-big b a 0))
            ((null? (ncdr a))
               (if (null? (ncdr b))
                  (mult-fixnums (ncar a) (ncar b))
                  (mult-num-big (ncar a) b 0)))
            ((null? (ncdr b)) (mult-num-big (ncar b) a 0))

            ;; otherwise divide et imperial troopers
            (else
               (lets
                  ; 3O(n)
                  ((ah at bh bt atl
                     (splice-nums a b a b #n #n #t 0)))
                  (if (lesser? atl 30)
                     (mul-simple a b)
                     (lets
                         ; 3F(O(n/2)) + 2O(n/2)
                        ((z2 (kara ah bh))
                         (z0 (kara at bt))
                         (z1a
                           (lets ((a (+ ah at)) (b (+ bh bt)))
                              (kara a b)))
                         ; 2O(n)
                         (z1 (- z1a (+ z2 z0)))
                         ; two more below
                         (x (if (eq? z1 0) z0 (add-ext z0 z1 atl))))
                        (if (eq? z2 0)
                           x
                           (add-ext x z2 (<< atl 1)))))))))

      ;(define mult-big mul-simple)   ; for debugging only!
      (define mult-big kara)

      (define (muli a b)
         (cond
            ; are these actually useful?
            ((eq? a 0) 0)
            ;((eq? a 1) b)
            ((eq? b 0) 0)
            ;((eq? b 1) a)
            (else
               (case (type a)
                  (type-fix+
                     (case (type b)
                        (type-fix+ (mult-fixnums a b))                  ; +a * +b
                        (type-fix- (negative (mult-fixnums a b)))      ; +a * -b
                        (type-int+ (mult-num-big a b 0))               ; +a * +B
                        (type-int- (negative (mult-num-big a b 0)))   ; +a * -b
                        (else (big-bad-args 'mul a b))))
                  (type-fix-
                     (case (type b)
                        (type-fix+ (negative (mult-fixnums a b)))      ; -a * +b -> -c | -C
                        (type-fix- (mult-fixnums a b))                  ; -a * -b -> +c | +C
                        (type-int+ (to-int- (mult-num-big a b 0))) ; -a * +B -> -C
                        (type-int- (mult-num-big a b 0))            ; -a * -B -> +C
                        (else (big-bad-args 'mul a b))))
                  (type-int+
                     (case (type b)
                        (type-fix+ (mult-num-big b a 0))            ; +A * +b -> +C
                        (type-fix- (to-int- (mult-num-big b a 0))) ; +A * -b -> -C
                        (type-int+ (mult-big a b))               ; +A * +B -> +C
                        (type-int- (to-int- (mult-big a b)))       ; +A * -B -> -C
                        (else (big-bad-args 'mul a b))))
                  (type-int-
                     (case (type b)
                        (type-fix+ (to-int- (mult-num-big b a 0))) ; -A * +b -> -C
                        (type-fix- (mult-num-big b a 0))               ; -A * -b -> +C
                        (type-int+ (to-int- (mult-big a b)))       ; -A * +B -> -C
                        (type-int- (mult-big a b))                  ; -A * -B -> +C
                        (else (big-bad-args 'mul a b))))
                  (type-rational
                     (case (type b)
                        (type-rational  (big-bad-args 'mul a b))         ; handle this before mul for now
                        (else (muli b a))))                  ; otherwise use other branches
                  (else (big-bad-args 'mul a b))))))



      ;;;
      ;;; REMAINDER
      ;;;


      (define (rsub a b) ; -> a' borrow|null
         (cond
            ((null? b) (values a #false)) ; ok if borrowable
            ((null? a) (values a #n)) ; fail
            (else
               (lets
                  ((d (- (ncar a) (ncar b))) ; fix+ or fix-
                   (tl dr (rsub (ncdr a) (ncdr b))))
                  (cond
                     ((null? dr) (values tl dr)) ; failed below
                     (dr
                        (let ((d (- d 1))) ; int- (of was -fx-greatest), fix- or fix+
                           (if (negative? d)
                              (values (ncons (+ d *first-bignum*) tl) #true) ; borrow
                              (values (ncons d tl) #false))))
                     ((eq? (type d) type-fix-) ; borrow
                        (values (ncons (+ d *first-bignum*) tl) #true))
                     (else
                        (values (ncons d tl) #false)))))))

      (define (drop-zeros n)
         (cond
            ((null? n) n)
            ((eq? 0 (ncar n)) (drop-zeros (ncdr n)))
            (else n)))

      (define (rev-sub a b) ; bignum format a' | #false
         (lets ((val fail? (rsub a b)))
            (if fail?
               #false
               (drop-zeros val))))

      ;; reverse number multiplication by digit

      (define (rmul n d) ; tl x carry-up
         (if (null? n)
            (values #n 0)
            (lets
               ((x tl n)
                (lo hi (fx* x d))
                (tl carry (rmul (ncdr n) d))
                (lo over (fx+ lo carry))
                (hi _ (fx+ hi over)))
                  (values (ncons lo tl) hi))))

      (define (rmul-digit n d) ; -> bignum
         (cond
            ((eq? d 0) (ncons 0 #n))
            ((eq? d 1) n)
            (else
               (lets ((tl carry (rmul n d)))
                  (if (eq? carry 0)
                     tl
                     (ncons carry tl))))))

      (define (rrem a b) ; both should be scaled to get a good head for b
         (cond
            ((null? a) a)
            ((null? (ncdr a)) a)
            ((lesser? (ncar b) (ncar a))
               (lets
                  ((h _ (fx+ (ncar b) 1))
                   (_ f r (fxqr 0 (ncar a) h))
                   (bp (rmul-digit b f))
                   (ap (rev-sub a bp)))
                  (if ap (rrem ap b) a)))
            ((rev-sub a b) => (C rrem b)) ; <- check later
            (else
               (lets
                  ((h _ (fx+ (ncar b) 1))
                   (fh fl r (fxqr (ncar a) (ncar (ncdr a)) h))
                   )
                  (if (eq? fh 0)
                     (lets
                        ((f fl)
                         (bp (rmul-digit b f))
                         (ap (rev-sub a bp))
                         (ap (or ap (rev-sub a (ncons 0 bp))))
                         )
                        (if ap (rrem ap b) a))
                     (lets
                        ((f fh)
                         (bp (rmul-digit b f))
                         (ap (rev-sub a (ncons 0 bp))))
                        (if ap (rrem ap b) a)))))))

      (define (nat-rem-reverse a b)
         (if (< a b)
            a
            (lets ((rb (nrev b)))
               (if (lesser? #b000000111111111111111111 (ncar rb))
                  ; scale them to get a more fitting head for b
                  ; and also get rid of the special case where it is fx-greatest
                  (>> (nat-rem-reverse (<< a 12) (<< b 12)) 12)
                  (let ((r (rrem (nrev a) rb)))
                     (cond
                        ((null? r) 0)
                        ((null? (ncdr r)) (ncar r))
                        (else (nrev r))))))))


      (define nat-rem nat-rem-reverse)    ; better when b is smaller ;; FIXME - not yet for variable sized fixnums


      ;;;
      ;;; EXACT DIVISION
      ;;;

      ;; this algorithm is based on the observation that the lowest digit of
      ;; the quotient in division, when the remainder will be 0, depends only
      ;; on the lowest bits of the divisor and quotient, which allows the
      ;; quotient to be built bottom up using only shifts and substractions.

      ; bottom up exact division, base 2

      (define (div-rev st out)
         (if (null? st) out
            (div-rev (ncdr st) (ncons (ncar st) out))))

      (define (div-finish n)
         (cond
            ((null? (ncdr n)) (ncar n))
            ((eq? (ncar n) 0) (div-finish (ncdr n)))
            (else (div-rev n #n))))

      ; fixme, postpone and merge shifts and substraction

      ; later (sub-shifted a b s) in 1-bit positions
      ; b is usually shorter, so shift b right and then substract instead
      ; of moving a by s

      (define last-bit (- fx-width 1))

      (define (divex bit bp a b out)
         (cond
            ((eq? (type a) type-fix-) #false) ;; not divisible
            ((eq? (type a) type-int-) #false) ;; not divisible
            ((eq? a 0) (div-finish out))
            ((eq? (band a bit) 0) ; O(1)
               (if (eq? bp last-bit)
                  (lets
                     ((a (ncdr a))
                      (a (if (null? (ncdr a)) (ncar a) a)))
                     (divex 1 0 a b (ncons 0 out)))
                  (lets
                     ((bit _ (fx+ bit bit))
                      (bp _  (fx+ bp 1)))
                     (divex bit bp a b out))))
            (else ; shift + substract = amortized O(2b) + O(log a)
               (divex bit bp (- a (<< b bp))
                  b (ncons (fxior bit (ncar out)) (ncdr out))))))

      (define divex-start (ncons 0 #n))

      ; FIXME: shifts are O(a+b), switch to bit walking later to get O(1)

      (define (nat-divide-exact a b)
         (if (eq? (band b 1) 0)
            (if (eq? (band a 1) 0)
               ;; drop a powers of two from both and get 1 bit to bottom of b
               (nat-divide-exact (>> a 1) (>> b 1))
               #false) ;; not divisible
            (divex 1 0 a b divex-start)))

      (define (maybe-negate a)
         (if a (negate a) a))

      ; int nat -> int | #false
      (define (divide-exact a b)
         (case (type a)
            (type-fix- (maybe-negate (divide-exact (negate a) b)))
            (type-int- (maybe-negate (divide-exact (negate a) b)))
            (else (nat-divide-exact a b))))

      (define ediv divide-exact)



      ;;;
      ;;; INTEGER DIVISION
      ;;;

      (define (nat-quotrem a b)
         (let ((r (nat-rem a b)))
            (values (ediv (- a r) b) r)))

      (define (div-big->negative a b)
         (lets ((q r (nat-quotrem a b)))
            (negate q)))

      ; walk down a and compute each digit of quotient using the top 2 digits of a
      (define (qr-bs-loop a1 as b out)
         (if (null? as)
            (if (null? (ncdr out))
               (values (ncar out) a1)
               (values out a1))
            (lets
               ((a2 as as)
                (q1 q2 r (fxqr a1 a2 b)))
               (if (eq? q1 0)
                  (qr-bs-loop r as b (ncons q2 out))
                  (qr-bs-loop r as b (ncons q2 (ncons q1 out)))))))

      (define (qr-big-small a b) ; -> q r
         (cond
            ((eq? b 0)
               (big-bad-args 'qr-big-small a b))
            (else
               (lets
                  ((ra (nrev a))
                   (a as ra))
                  (qr-bs-loop a as b #n)))))

      (define (truncate/ a b)
         (if (eq? b 0)
            (big-bad-args 'truncate/ a b)
            (case (type a)
               (type-fix+
                  (case (type b)
                     (type-fix+ (receive (fxqr 0 a b) (lambda (_ q r) (values q r))))
                     (type-int+ (values 0 a))
                     (type-fix- (receive (fxqr 0 a b) (lambda (_ q r) (values (negate q) r))))
                     (type-int- (values 0 a))
                     (else (big-bad-args 'truncate/ a b))))
               (type-int+
                  (case (type b)
                     (type-fix+ (receive (qr-big-small a b) (lambda (q r) (values q r))))
                     (type-int+ (nat-quotrem a b))
                     (type-fix- (receive (qr-big-small a b) (lambda (q r) (values (negate q) r))))
                     (type-int- (receive (nat-quotrem a (negate b))
                              (lambda (q r) (values (negate q) r))))
                     (else (big-bad-args 'truncate/ a b))))
               (type-fix-
                  (case (type b)
                     (type-fix+
                        (receive (fxqr 0 a b) (lambda (_ q r) (values (negate q) (negate r)))))
                     (type-fix- (receive (fxqr 0 a b) (lambda (_ q r) (values q (negate r)))))
                     (type-int+ (values 0 a))
                     (type-int- (values 0 a))
                     (else (big-bad-args 'truncate/ a b))))
               (type-int-
                  (case (type b)
                     (type-fix+
                        (lets ((q r (qr-big-small a b)))
                           (values (negate q) (negate r))))
                     (type-fix- (receive (qr-big-small a b) (lambda (q r) (values q (negate r)))))
                     (type-int+ (receive (nat-quotrem (negate a) b)
                              (lambda (q r) (values (negate q) (negate r)))))
                     (type-int- (receive (nat-quotrem (negate a) (negate b))
                              (lambda (q r) (values q (negate r)))))
                     (else (big-bad-args 'truncate/ a b))))
               (else
                  (big-bad-args 'truncate/ a b)))))


      (define (quotient a b)
         (lets ((q r (truncate/ a b))) q))

      (define-syntax fx%
         (syntax-rules ()
            ((fx% a b)
               (lets ((q1 a2 r (fxqr 0 a b))) r))))

      (define (rem a b)
         (case (type a)
            (type-fix+
               (case (type b)
                  (type-fix+ (fx% a b))
                  (type-fix- (fx% a b))
                  (type-int+ a)
                  (type-int- a)
                  (else (big-bad-args 'remainder a b))))
            (type-fix-
               (case (type b)
                  (type-fix+ (negate (fx% a b)))
                  (type-fix- (negate (fx% a b)))
                  (type-int+ a)
                  (type-int- a)
                  (else (big-bad-args 'remainder a b))))
            (type-int+
               (case (type b)
                  (type-fix+ (receive (qr-big-small a b) (lambda (q r) r)))
                  (type-fix- (receive (qr-big-small a b) (lambda (q r) r)))
                  (type-int+ (nat-rem a b))
                  (type-int- (nat-rem a (negate b)))
                  (else (big-bad-args 'remainder a b))))
            (type-int-
               (case (type b)
                  (type-fix+
                     (receive (qr-big-small a b)
                        (lambda (q r) (negate r))))
                  (type-fix-
                     (receive (qr-big-small a b)
                        (lambda (q r) (negate r))))
                  (type-int+ (negate (nat-rem (negate a) b)))
                  (type-int- (negate (nat-rem (negate a) (negate b))))
                  (else (big-bad-args 'remainder a b))))
            (else (big-bad-args 'remainder a b))))

      ; required when (truncate/ a b) -> q,r and b != 0
      ;   a = q*b + r
      ;    |r| < |b|
      ;    -a/b = a/-b = -(a/b)

      ; note: remainder has sign of a, modulo that of b

      (define (mod a b)
         (if (negative? a)
            (if (negative? b)
               (rem a b)
               (let ((r (rem a b)))
                  (if (eq? r 0)
                     r
                     (+ b r))))
            (if (negative? b)
               (let ((r (rem a b)))
                  (if (eq? r 0)
                     r
                     (+ b r)))
               (rem a b))))

      (define * muli)

      (define / quotient)
))
