,load "owl/sum.scm"

(import (owl sum))

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

