#| doc
Random access lists are a data structure similar to lists,
but with very different efficiency characteristics.
A regular list built out of cons cells is an optimal solution,
   if one needs to work mainly with the initial elements or the whole list at a time.
However, if you need to frequently find and maybe update values in the middle of the list,
   you have to perform operations taking time proportional to the length of the list.
In other words, those list operations are linear time, or have complexity O(n).
Cons, car and cdr on the other hand are very efficient for regular lists.
Regardless of the size of a list, it will always take a fixed amount of time to add, take or remove a value from it.
In other words, these operations are constant time, or have complexity O(1).

A random access list is a data structure,
   which unsurprisingly attempts to make random access and update efficient.

The performance characteristics of this random access list library are:
```
  car → O(1)
  cdr → O(log n)
  cons → O(log n)
  get → O(log n)
  set → O(log n)
  len → O(log n)
  fold → O(n)
  append → O(n)
  list->rlist → O(n log n)
  rlist->list → O(n)
```

The operation is based on two ideas.
Firstly, a random access list consists of a sequence of complete binary trees.
The binary trees are built out of cons cells when needed.
The first tree is always of height 1, meaning it just holds the value, much like a regular cons cell.
The next node always holds a binary tree either of the same or next height.
There can be at most two trees of the same height next to eachother.
Therefore, tree heights `(1 1)`, `(1 2 4)` and `(1 1 2 4 4)` are valid,
   whereas `(1 1 1)`, `(2 2 4)` and `(1 2 2 8)` are not.
`(5)` is right out.

Secondly, trees can be addressed directly with bits.
It takes a n-bit number address each node of a complete binary tree of height n.
Finding a value from a list works by first finding the tree in which the value is held,
  and then using the remaining bits to find the correct leaf node in the tree.

It is easy to see that it takes O(log n) steps to find the tree in which some particular value is held,
   and then another O(log n) steps to walk the tree to a given position,
Threfore we have a total complexity of O(log n) for access and update.

```
  (rcar (rcons 11 rnull)) → 11
  (rnull? (rcons 11 rnull)) → #false
  (rlist->list (rcons 1 (rcons 2 rnull))) → (1 2))
  (rget (list->rlist (iota 0 1 1000)) 123 #f) → 123
  (rget (list->rlist (iota 0 1 1000)) 1234 #f) → #false
```

|#

(define-library (owl lcd rlist)

   (import
      (owl core)
      (only (owl syscall) error)
      (owl sum)
      (owl list))

   (export
      rnull
      rcons
      rget
      rcar
      rcdr
      rset
      rlen
      rlist
      rfold
      rfoldr
      riter
      riterr
      rmap
      rnull?
      rpair?
      list->rlist
      rlist->list)

   (begin

      ;; note that for now the pattern match must come in the same order
      ;; as the data type definition
      (define-sum-type rl-case
         (snd x t)
         (fst x t)
         (nil))

      (define rnull (nil))

      (define (nope op)
         (error "invalid rlist arguments to " op))

      (define (rcons x as)
         (rl-case as
            ((snd a bs) (nope 'rcons))
            ((fst a bs)
               (rl-case bs
                  ((snd b cs)
                     (rl-case cs
                        ((snd v d) (nope 'rcons))
                        ((fst v d)
                           (fst x (rcons (cons a b) cs)))
                        ((nil) (fst x (fst (cons a b) rnull)))))
                  ((fst b cs) (fst x (snd a bs)))
                  ((nil) (fst x (snd a rnull)))))
            ((nil) (fst x rnull))))

      (define (rcar rl . def)
         (rl-case rl
            ((snd a bs) (error 'rcar rl))
            ((fst a bs) a)
            ((nil)
               (if (null? def)
                  (error 'rcar 'null)
                  (car def)))))

      (define (tof) #f)

      (define rnull?
         (let ((y (λ () #t))
               (n (λ (a b) #f)))
            (λ (rl) (rl n n y))))

      (define rpair?
         (let ((y (λ (a b) #t))
               (n (λ () #f)))
            (λ (rl) (rl y y n))))

      (define (drop as)
         (rl-case as
            ((snd a bs)
               (fst a bs))
            ((fst ab cs)
               (fst (car ab)
                  (snd (cdr ab)
                     (drop cs))))
            ((nil)
               rnull)))

      (define (rcdr as)
         (rl-case as
            ((snd a as) (nope 'rcdr))
            ((fst a as) (drop as))
            ((nil) rnull)))

      (define (pick tree path depth)
         (if (eq? depth 1)
            tree
            (lets ((depth _ (fx>> depth 1)))
               (if (eq? (fxand path depth) 0)
                  (pick (car tree) path depth)
                  (pick (cdr tree) path depth)))))

      (define (rlen rl)
         (let loop ((rl rl) (d 0) (dp 1) (n 0))
            (rl-case rl
               ((snd tree rl)
                  (lets ((n _ (fx+ n d)))
                     (loop rl d dp n)))
               ((fst tree rl)
                  (lets
                     ((d dp)
                      (dp _ (fx+ dp dp))
                      (n _ (fx+ n d)))
                     (loop rl d dp n)))
               ((nil) n))))

      (define (rget rl pos def)
         (let loop ((rl rl) (d 0) (dp 1) (pos pos))
            (rl-case rl
               ((snd tree rl)
                  (lets ((posp u (fx- pos d)))
                     (if (eq? u 0)
                        (loop rl d dp posp)
                        (pick tree pos d))))
               ((fst tree rl)
                  (lets
                     ((d dp)
                      (dp _ (fx+ dp dp))
                      (posp u (fx- pos d)))
                     (if (eq? u 0)
                        (loop rl d dp posp)
                        (pick tree pos d))))
               ((nil) def))))

      ;; rset is rget + path copying

      (define (set tree path depth val)
         (if (eq? depth 1)
            val
            (lets ((depth _ (fx>> depth 1)))
               (if (eq? (fxand path depth) 0)
                  (cons (set (car tree) path depth val) (cdr tree))
                  (cons (car tree) (set (cdr tree) path depth val))))))

      (define (rset rl pos val)
         (let loop ((rl rl) (d 0) (dp 1) (pos pos))
            (rl-case rl
               ((snd tree rl)
                  (lets ((posp u (fx- pos d)))
                     (if (eq? u 0)
                        (snd tree (loop rl d dp posp))
                        (snd (set tree pos d val) rl))))
               ((fst tree rl)
                  (lets
                     ((d dp)
                      (dp _ (fx+ dp dp))
                      (posp u (fx- pos d)))
                     (if (eq? u 0)
                        (fst tree (loop rl d dp posp))
                        (fst (set tree pos d val) rl))))
               ((nil) rl))))

      ;;; fold from left

      (define (rfold-node op st n d)
         (if (eq? d 1)
            (op st n)
            (lets ((d _ (fx>> d 1)))
               (rfold-node op (rfold-node op st (car n) d) (cdr n) d))))

      (define (rfold op st rl)
         (let loop ((rl rl) (st st) (depth 0))
            (rl-case rl
               ((snd a rl)
                  (loop rl (rfold-node op st a depth) depth))
               ((fst a rl)
                  (if (eq? depth 0)
                     (loop rl (op st a) 1)
                     (lets ((depth _ (fx+ depth depth)))
                        (loop rl
                           (rfold-node op st a depth)
                           depth))))
               ((nil) st))))

      (define (list->rlist x)
         (foldr rcons rnull x))

      (define (rlist->list rl)
         (reverse (rfold (λ (pre val) (cons val pre)) '() rl)))

      (define (rlist . args)
         (list->rlist args))

      (define rmap    #f)
      (define rfoldr  #f)
      (define riter   #f)
      (define riterr  #f)

))