;;; Random access lists are a data structure similar to lists,
;;;;   but with very different efficiency characteristics.
;;; A regular list built out of cons cells is an optimal solution,
;;;    if one needs to work mainly with the initial elements or the whole list at a time.
;;; However, if you need to frequently find and maybe update values in the middle of the list,
;;;    you have to perform operations taking time proportional to the length of the list.
;;; In other words, those list operations are linear time, or have complexity O(n).
;;; Cons, car and cdr on the other hand are very efficient for regular lists.
;;; Regardless of the size of a list, it will always take a fixed amount of time to add, take or remove a value from it.
;;; In other words, these operations are constant time, or have complexity O(1).
;;;
;;; A random access list is a data structure,
;;;    which unsurprisingly attempts to make random access and update efficient.
;;;
;;; The performance characteristics of this random access list library are:
;;; ```
;;;   car → O(1)
;;;   cdr → O(log n)
;;;   cons → O(log n)
;;;   get → O(log n)
;;;   set → O(log n)
;;;   append → O(n)
;;; ```
;;;
;;; The operation is based on two ideas.
;;; Firstly, a random access list consists of a sequence of complete binary trees.
;;; The binary trees are built out of cons cells when needed.
;;; The first tree is always of height 1, meaning it just holds the value, much like a regular cons cell.
;;; The next node always holds a binary tree either of the same or next height.
;;; There can be at most two trees of the same height next to eachother.
;;; Therefore, tree heights `(1 1)`, `(1 2 4)` and `(1 1 2 4 4)` are valid,
;;;    whereas `(1 1 1)`, `(2 2 4)` and `(1 2 2 8)` are not.
;;; `(5)` is right out.
;;;
;;; Secondly, trees can be addressed directly with bits.
;;; It takes a n-bit number address each node of a complete binary tree of height n.
;;; Finding a value from a list works by first finding the tree in which the value is held,
;;;   and then using the remaining bits to find the correct leaf node in the tree.
;;;
;;; It is easy to see that it takes O(log n) steps to find the tree in which some particular value is held,
;;;    and then another O(log n) steps to walk the tree to a given position,
;;; Threfore we have a total complexity of O(log n) for access and update.
;;;
;;; ```
;;;   (rcar (rcons 11 rnull)) → 11
;;;   (rnull? (rcons 11 rnull)) → #false
;;;   (rlist->list (rcons 1 (rcons 2 rnull))) → (1 2))
;;;   (rget (list->rlist (iota 0 1 1000)) 123 #f) → 123
;;;   (rget (list->rlist (iota 0 1 1000)) 1234 #f) → #false
;;; ```

(define-library (owl rlist-lcb)

   (import
      (owl defmac)
      (owl list))

   (export
      rnull
      rcons
      rget
      rcar
      rcdr
      rlist
      rfold
      rnull?
      node-case
      list->rlist
      rlist->list)

   (begin

      (define (snd x t)
         (λ (s d e) (s x t)))

      (define (fst x t)
         (λ (s d e) (d x t)))

      (define (rnull s d e) (e))

      ;; these could be autogenerated later
      (define-syntax node-case
         (syntax-rules (snd fst null _bind)
            ((node-case _bind elem () (s d n) ())
               (elem s d n))

            ;; allow passing a precomputed thunk, since it's often fixed
            ((node-case _bind elem (null fst snd) () (pre ... (null nthunk) post ...))
             (node-case _bind elem (fst snd) (nthunk) (pre ... post ...)))
            ((node-case _bind elem (null fst snd) () (pre ... ((null) nval) post ...))
             (node-case _bind elem (fst snd) ((λ () nval)) (pre ... post ...)))
            ((node-case _bind elem (null fst snd) () bindings)
             (node-case _bind elem (fst snd) (#f) bindings))

            ((node-case _bind elem (fst snd) (nval) (pre ... ((fst a b) . val) post ...))
             (node-case _bind elem (snd) ((λ (a b) . val) nval) (pre ... post ...)))
            ((node-case _bind elem (fst snd) (nval) bindings)
             (node-case _bind elem (snd) (#f nval) bindings))

            ((node-case _bind elem (snd) (dval nval) (((snd a b) . val)))
             (node-case _bind elem () ((λ (a b) . val) dval nval) ()))
            ((node-case _bind elem (snd) (dval nval) ())
             (node-case _bind elem () (#f dval nval) ()))

            ((node-case (op . args) . stuff)
               (let ((node (op . args)))
                  (node-case node . stuff)))
            ((node-case _bind . stuff)
               (syntax-error "bad node-case situation:" (node-case _bind . stuff)))
            ((node-case elem . bindings)
               ;; start finding cases, fill empty ones with elem itsel
               (node-case _bind elem (null fst snd) () bindings))))

      (define (rcons x as)
         (node-case as
            ((fst a bs)
               (node-case bs
                  ((snd b cs)
                     (node-case cs
                        ((fst v d)
                           (fst x (rcons (cons a b) cs)))
                        ((null) (fst x (fst (cons a b) rnull)))))
                  ((fst z cs) (fst x (snd a bs)))
                  ((null) (fst x (snd a rnull)))))
            ((null) (fst x rnull))))

      (define (rcar rl def)
         (node-case rl
            ((fst a rl) a)
            ((null) def)))

      (define (tof) #f)

      (define rcar
         (case-lambda
            ((rl)
               (node-case rl
                  ((fst a rl) a)
                  (null tof)))
            ((rl def)
               (node-case rl
                  ((fst a rl) a)
                  ((null) def)))))

      (define rnull?
         (let ((y (λ () #t))
               (n (λ (a b) #f)))
            (λ (rl) (rl n n y))))

      (define (drop as)
         (node-case as
            ((null)
               rnull)
            ((snd a bs)
               (fst a bs))
            ((fst ab cs)
               (fst (car ab)
                  (snd (cdr ab)
                     (drop cs))))))

      (define (rcdr as)
         (node-case as
            ((fst a bs) (drop bs))
            ((null) rnull)))

      (define (pick tree path depth)
         (if (eq? depth 1)
            tree
            (lets ((depth _ (fx>> depth 1)))
               (if (eq? (fxband path depth) 0)
                  (pick (car tree) path depth)
                  (pick (cdr tree) path depth)))))

      (define (rget rl pos def)
         (let loop ((rl rl) (d 0) (dp 1) (pos pos))
            (node-case rl
               ((snd tree rl)
                  (lets ((posp u (fx- pos d)))
                     (if u
                        (pick tree pos d)
                        (loop rl d dp posp))))
               ((fst tree rl)
                  (lets
                     ((d dp)
                      (dp _ (fx+ dp dp))
                      (posp u (fx- pos d)))
                     (if u
                        (pick tree pos d)
                        (loop rl d dp posp))))
               ((null) def))))

      (define (rfold-node op st n d)
         (if (eq? d 1)
            (op st n)
            (lets ((d _ (fx>> d 1)))
               (rfold-node op (rfold-node op st (car n) d) (cdr n) d))))

      (define (rfold op st rl)
         (let loop ((rl rl) (st st) (depth 0))
            (node-case rl
               ((snd a rl)
                  (loop rl (rfold-node op st a depth) depth))
               ((fst a rl)
                  (if (eq? depth 0)
                     (loop rl (op st a) 1)
                     (lets ((depth _ (fx+ depth depth)))
                        (loop rl
                           (rfold-node op st a depth)
                           depth))))
               ((null) st))))

      (define (list->rlist x)
         (foldr rcons rnull x))

      (define (rlist->list rl)
         (reverse (rfold (λ (pre val) (cons val pre)) '() rl)))

      (define (rlist . args)
         (list->rlist args))

))
