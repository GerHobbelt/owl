
(import (owl lcd))

;; ---------------------- 8< ----------------------

(define-sum-type lizt
   (nil)
   (kons a b))

(define (kar x)
   (lizt x
      ((nil) (car 'nil))
      ((kons a b) a)))

(define (kdr x)
   (lizt x
      ((nil) (cdr 'nil))
      ((kons a b) b)))

(define (kfold op st lst)
   (lizt lst
      ((nil) st)
      ((kons a as)
         (kfold op (op st a) as))))

(define (kiota a n b)
   (if (= a b)
      (nil)
      (kons a (kiota (+ a n) n b))))

(print (kfold + 0 (kiota 0 1 100)))

(print (fold  + 0 (iota  0 1 100)))

;; ---------------------- 8< ----------------------

(define-sum-type arith
   (sum a b)
   (sub a b)
   (val a))

(define (eval e)
   (arith e
      ((sum a b) (+ (eval a) (eval b)))
      ((sub a b) (- (eval a) (eval b)))
      ((val a) a)))

(print (eval (sum (val 40) (sub (val 3) (val 1)))))

