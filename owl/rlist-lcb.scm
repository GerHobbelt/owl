;;; Random access lists are a data structure like lists, which offer an efficient cons, car and
;;; cdr, but they also allow referring and setting values from the middle of the the list without
;;; having to walk all the way to the position.
;;;
;;; This library is a test implementation of random access lists implemented in part as
;;; executable functions. The implementation is written so that it does not depend on
;;; bignum arithmentic. As a consequence the maximum length of a value that can be referred
;;; from the middle of the list is bound by fixnum size to 16777215. The data structure itself
;;; does not impose any limits on number of elements.
;;;
;;; The idea of the data structure consists of two parts. First in order to maintain efficient
;;; access to elements in the list, the data structure grows a forest of complete binary trees
;;; of the values. The first tree is always of height 0, allowing an O(1) car, a following tree
;;; is always either of same height or *one* level taller, and twice as large, than the previous one,
;;; and there are always at most *two* trees of the same height next to each other.
;;;
;;; Given these invariants, it is easy to see that it takes O(log n) steps to find the tree in
;;; which some particular value is held, and then another O(log n) steps to walk the tree to
;;; a given position. Because the elements of the tree are kept in order, it is possible to
;;; use the bits of the index to choose tree branches when finding a particular value from
;;; a tree in which it is known to be.
;;;
;;; The initial version mainly has rcons, rnull and rget, which act like cons, null and lref.
;
(define-library (owl rlist-lcb)

   (import
      (owl defmac)
      (owl list))

   (export
      rnull
      rcons
      rget
      rlist
      list->rlist)

   (begin

      (define (same x t)
         (λ (s d e) (s x t)))

      (define (double x t)
         (λ (s d e) (d x t)))

      (define (rnull s d e) e)

      (define (rcons x a)
         (a (λ (y b)
               (b (λ (z c)
                     (c #f
                        (λ (v d)
                            ((rcons (cons y z) (same v d))
                               (λ (yz b) (same x (double yz b)))
                               #f #f))
                        (same x (double (cons y z) rnull))))
                  (λ (z c) (same x a))
                  (same x (same y rnull))))
            #f (same x rnull)))

      (define (pick tree path depth)
         (if (eq? depth 1)
            tree
            (lets ((depth _ (fx>> depth 1)))
               (if (eq? (fxband path depth) 0)
                  (pick (car tree) path depth)
                  (pick (cdr tree) path depth)))))

      (define (rget rl pos def)
         (let loop ((rl rl) (depth 1) (pos pos))
            (rl
               (λ (tree rl)
                  (lets ((posp u (fx- pos depth)))
                     (if u
                        (pick tree pos depth)
                        (loop rl depth posp))))
               (λ (tree rl)
                  (lets
                     ((depth o (fx+ depth depth))
                      (posp u (fx- pos depth)))
                     (if u
                        (pick tree pos depth)
                        (loop rl depth posp))))
               def)))

      (define (list->rlist x)
         (foldr rcons rnull x))

      (define (rlist . args)
         (list->rlist args))
))

