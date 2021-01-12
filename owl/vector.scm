#| doc

Vectors

Vectors are one-dimensional data structures indexable by natural numbers,
having O(n log_256 n) access and memory use (effectively O(1)). They are mainly
intended to be used for static data requiring relatively efficient iteration
and random access.

Vectors are implemented as complete 256-ary trees. Small vectors
fitting to one node of the tree are of raw or allocated type 11, meaning
they usually take 8+4n or 4+n bytes of memory, depending on whether the
values are fixnums in the range 0-255 or other values.

Large vectors are trees. Each node in the tree handles one byte of an index,
and nodes starting from root each dispatch the highest byte of an index. When
only one byte is left, one reads the reached leaf node, or the leaf node stored
to the dispatch node.

Reading the vector in order corresponds to breadth-first walk of the tree. No
nonzero number has 0 as the highest byte, so the first dispatch position of the
root is always free. This position contains the size of the vector, so that it
is accessable in O(1) without space overhead or special case handling. Leaf
nodes have the size as part of the normal object header.

Order example using binary trees:

             (0 1)                 bits 0 and 1, only 1 can have children
                |                  dispatch the top bit
              (2 3)                bits from top, 10 11, numbers ending here 2 and 3
              /   \                dispatch top and second bit
             /     \
         (4 5)     (6 7)           bits from top, (100 101) (110 111)
         /  |       |  \
        /   |       |   \
   (9 8) (10 11) (12 13) (14 15)   etc

Vectors use the same order, but with up to 256 links per node.
|#

(define-library (owl vector)

  (export
      vector              ; v0, .., vn → vector
      vector?             ; x → bool
      vector-length       ; v → n
      vector-ref          ; v x p → v[p] | error
      list->vector
      vector->list
      vec-iter
      vec-iterr
      vec-fold
      vec-foldr
      vec-range           ; vec x start x end -> vec'
      vec-iter-range      ; vec x start x end -> ll
      vector-map          ; (val → val') x vec → vec'

      ; these assume a sorted vector (as used by pred) having matches in one continuous range
      ;vec-match-range         ; vec x val-pred -> lo x hi | #false x #false
      ;vec-match-range-between ; vec x pred x hi x lo -> lo x hi | #false x #false

      ;vec-equal?         ; v x v → bool
      ;vec-render         ; v x tail → tail'

      merge-chunks          ; exported for use in lib-io (may be moved later)
      make-vector           ; n elem → #(elem ...)
      leaf-data vec-leaf-of
      vec-leaves
      vec-cat             ;  vec x vec → vec
      vec-rev
      *vec-leaf-size*)     ; needed for vector IO

   (import
      (owl core)
      (owl lazy)
      (owl bytevector)
      (owl list)
      (owl list-extra)
      (owl tuple)
      (owl proof)
      (only (owl syscall) error)
      (owl math))

   (begin

      ;; number of bits each vector tree node dispatches from index
      (define *vec-bits* (>> fx-width 1))
      ; (define *vec-bits* 8) ;; legacy

      (define *vec-leaf-size* (<< 1 *vec-bits*))
      (define *vec-leaf-max* (- *vec-leaf-size* 1))

      ;;;
      ;;; Vector search
      ;;;

      ;; dispatch low 8 bits of a fixnum, returning the subnode
      (define (vec-dispatch-1 v n)
         (case (type v)
            (type-vector-dispatch ; vector dispatch node with #[Leaf D0 ... D255]
               (lets ((n _ (fx+ (fxand n *vec-leaf-max*) 2))) ;; jump over header and leaf
                  (ref v n)))
            (else
               (error "Bad vector node in dispatch-1: type " (type v)))))

      ; dispatch the high half bits of a fixnum, returning the subnode
      (define (vec-dispatch-2 v d) ; -> v'
         (case (type v)
            (type-vector-dispatch
               (lets
                  ((p _ (fx>> d *vec-bits*))
                   (p _ (fx+ p 2)))
                  (ref v p)))
            (type-vector-leaf
               (error "Leaf vector in dispatch-2: " v))
            (else
               (error "Bad vector node in dispatch-2: obj " (type v)))))

      ; dispatch 8-bit parts (256-way tree)
      ; note, the highest one must know whether it must dispatch one or two bytes

      (define (vec-seek v ds)
         (lets ((d ds ds))
            (if (null? ds)
               (if (lesser? d *vec-leaf-size*) ; just one byte at top digit?
                  (vec-dispatch-1 v d)
                  (vec-dispatch-1 (vec-dispatch-2 v d) d))
               (vec-dispatch-1 (vec-seek v ds) d))))

      ; vec x fixnum -> local value
      (define (vec-ref-digit v n)
         (case (type v)
            (type-bytevector
               (ref v (fxand n *vec-leaf-max*)))
            (type-vector-dispatch
                (vec-ref-digit (ref v 1) n)) ; read the leaf of the node
            (type-vector-leaf
                (if (eq? n *vec-leaf-max*)
                   (ref v *vec-leaf-size*)
                   (lets ((n _ (fx+ (fxand n *vec-leaf-max*) 1)))
                     (ref v n))))
            (else
               (error "bad vector node in vec-ref-digit: type " (type v)))))

      ; find the node holding the last digit and read it
      (define (vec-ref-big v n)
         (vec-ref-digit
            (vec-dispatch-2
               (vec-seek v (ncdr n))
               (ncar n))
            (ncar n)))

      ; vec x n -> vec[n] or fail
      (define (vector-ref v n)
         (case (type n)
            (type-fix+
               (cond
                  ((eq? (type v) type-bytevector)
                     (ref v n))
                  ((lesser? n *vec-leaf-size*)
                     (vec-ref-digit v n))
                  (else
                     (vec-ref-digit (vec-dispatch-2 v n) (fxand n *vec-leaf-max*)))))
            (type-int+
               (vec-ref-big v n))
            (else
               (error "vector-ref: bad index: " n))))

      ;;; searching the leaves containing a pos

      ;; todo: switch vector-ref to use vec-leaf-of for int+ indices

      (define (vec-leaf-big v n)
         (vec-dispatch-2 (vec-seek v (ncdr n)) (ncar n)))

      (define (vec-leaf-of v n)
         (case (type n)
            (type-fix+
               (cond
                  ((eq? (type v) type-bytevector) v)
                  ((lesser? n *vec-leaf-size*) v)
                  (else (vec-dispatch-2 v n))))
            (type-int+
               (vec-leaf-big v n))
            (else
               (error "vec-leaf-of: bad index: " n))))

      ;; others

      (define (vector-length vec)
         (case (type vec)
            (type-bytevector
               (sizeb vec))
            (type-vector-dispatch
               (ref vec 2))
            (type-vector-leaf
               (tuple-length vec))
            (else
               (error "vector-length: not a vector: " (list vec 'of 'type (type vec))))))


      ;;;
      ;;; Vector construction
      ;;;

      ; note, a blank vector must use a raw one, since there are no such things as 0-tuples

      (define empty-vector
         (raw #n type-bytevector))

      (define (make-leaf rvals n raw?)
         (if raw?
            ;; the leaf contains only fixnums 0-255, so make a compact leaf
           (list->bytevector (reverse rvals)) ;; make node and reverse
           ;; the leaf contains other values, so need full 4/8-byte descriptors
           (listuple type-vector-leaf n (reverse rvals))))

      (define (byte? val)
         (and
            (eq? (type val) type-fix+)
            (eq? val (fxand val 255))))

      ;; list -> list of leaf nodes
      (define (chunk-list lst out leaves n raw? len)
         (cond
            ((eq? n *vec-leaf-size*) ; flush out to leaves
               (let ((leaf (make-leaf out n raw?)))
                  (chunk-list lst #n (cons (make-leaf out n raw?) leaves) 0 #true (+ len n))))
            ((null? lst) ; partial (last) leaf
               (if (null? out)
                  (values (reverse leaves) len)
                  (values (reverse (cons (make-leaf out n raw?) leaves)) (+ len n))))
            ((pair? lst)
               (if raw?
                  (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) (byte? (car lst)) len)
                  (chunk-list (cdr lst) (cons (car lst) out) leaves (+ n 1) #false len)))
            (else (chunk-list (lst) out leaves n raw? len))))

      (define (grab l n)
         (let loop ((l l) (n n) (taken #n))
            (cond
               ((null? l) (values (reverse taken) l))
               ((eq? n 0) (values (reverse taken) l))
               (else
                  (loop (cdr l) (- n 1) (cons (car l) taken))))))

      (define (merge-each l s)
         (cond
            ((null? l) l)
            ((null? s) l)
            ((number? (car l))
               (cons (car l)
                  (merge-each (cdr l) s)))
            (else
               (lets ((these s (grab s *vec-leaf-size*)))
                  (cons
                     (listuple type-vector-dispatch (+ 1 (length these)) (cons (car l) these))
                     (merge-each (cdr l) s))))))

      (define (merger l n)
         (if (null? l)
            #n
            (lets ((these l (grab l n)))
               (if (null? l)
                  these
                  (merge-each these (merger l (* n n)))))))

      (define (cut-at lst pos out)
         (cond
            ((null? lst)
               (values (reverse out) #n))
            ((eq? pos 0)
               (values (reverse out) lst))
            (else
               (cut-at (cdr lst) (- pos 1) (cons (car lst) out)))))

      (define (levels lst width)
         (lets ((here below (cut-at lst width #n)))
            (if (null? below)
               (list here)
               (cons here (levels below (* width *vec-leaf-size*)))))) ; everything below the first level branches 256-ways

      (define (merge-levels lst)
         (foldr
            (λ (this below)
               ;; this = list of leaves which will be as such or in dispatch nodes
               ;;        on this level of the tree
               ;; below = possible list of nodes up to 256 of which will be attached
               ;;         as subtrees to each leaf of this level, starting from left
               (let loop ((below below) (this this))
                  (cond
                     ((null? below) this)
                     ((null? this)
                        (error "out of leaves before out of data: " (length below)))
                     ;((number? (car this)) ;; skip size field at roo
                     ;   (cons (car this) (loop below (cdr this))))
                     (else
                        (lets ((here below (cut-at below *vec-leaf-size* #n)))
                           ;; attach up to 256 subtrees to this leaf
                           (cons
                              (listuple type-vector-dispatch (+ 1 (length here)) (cons (car this) here))
                              (loop below (cdr this))))))))
            #n (levels lst *vec-leaf-max*)))

      ; handle root here, since it is special in having 255 subtrees only (0-slot is empty and has size)
      (define (merge-chunks ll len)
         (cond
            ((null? ll)
               ;; no leaves, no data
               empty-vector)
            ((null? (cdr ll))
               ;; just one leaf, so it is also the vector
               (car ll))
            (else
               ;; the top node is special in that it has the size field
               ;; others can be computed easily recursively
               (lets
                  ((low (car ll))                  ;; first leaf data, places 0-255
                   (fields (cdr ll))    ;; fill in the length of the vector at dispatch position 0
                   (subtrees (merge-levels fields))) ;; construct the subtrees
                  (listuple type-vector-dispatch (+ 2 (length subtrees)) (ilist low len subtrees))))))


      (define (list->vector l)
         (if (null? l)
            empty-vector
            ;; leaves are chunked specially, so do that in a separate pass. also
            ;; compute length to avoid possibly forcing a computation twice.
            (lets ((chunks len (chunk-list l #n #n 0 #t 0)))
               ;; convert the list of leaf vectors to a tree
               (merge-chunks chunks len))))

      (define (vector? x) ; == raw or a variant of major type 11?
         (case (type x)
            (type-bytevector #true)
            (type-vector-leaf #true)
            (type-vector-dispatch #true)
            (else #false)))


      ;;;
      ;;; Vector iterators
      ;;;

      ;; iter - iterate forwards (leaves from left to right, tree breadth first left to right)

      (define (iter-raw-leaf v p tl)
         (if (eq? p 0)
            (cons (ref v p) tl)
            (lets ((n _ (fx- p 1)))
               (iter-raw-leaf v n (cons (ref v p) tl)))))

      (define (iter-leaf v p tl)
         (if (eq? p 0)
            tl
            (lets ((n _ (fx- p 1)))
               (iter-leaf v n (cons (ref v p) tl)))))

      (define (iter-leaf-of v tl)
         (case (type v)
            (type-vector-dispatch (iter-leaf-of (ref v 1) tl))
            (type-bytevector
               (let ((s (sizeb v)))
                  (if (eq? s 0)
                     tl
                     (iter-raw-leaf v (- s 1) tl))))
            (else tl))) ; size field -> number

      (define (vec-iter v)
         (let loop ((end (vector-length v)) (pos 0))
            (let ((this (vec-leaf-of v pos)))
               (iter-leaf-of this
                  (λ () (let ((pos (+ pos *vec-leaf-size*))) (if (< pos end) (loop end pos) #n)))))))

      (define (iter-leaf-range v p n t)
         (if (eq? n 0)
            t
            (pair (vector-ref v p)
               (iter-leaf-range v (+ p 1) (- n 1) t))))

      (define (iter-range-really v p n)
         (let ((start (band p *vec-leaf-max*)))
            (cond
               ((eq? start 0)
                  ;; read leaf from beginning
                  (if (> n *vec-leaf-max*)
                     ;; iter a full leaf (usual suspect)
                     (iter-leaf-of (vec-leaf-of v p)
                        (λ () (iter-range-really v (+ p *vec-leaf-size*) (- n *vec-leaf-size*))))
                     ;; last leaf reached, iter prefix and stop
                     (iter-leaf-range (vec-leaf-of v p) 0 n #n)))
               ((eq? n 0) #n)
               ((lesser? n (- *vec-leaf-size* start))
                  ;; the whole range is in a part of this leaf
                  (iter-leaf-range (vec-leaf-of v p) start n #n))
               (else
                  ;; this is the first leaf. iter a suffix of it.
                  (lets
                     ((n-here (- *vec-leaf-size* start))
                      (n-left (- n n-here)))
                     (iter-leaf-range (vec-leaf-of v p) start n-here
                        (λ () (iter-range-really v (+ p n-here) n-left))))))))

      (define (vec-iter-range v p e)
         (if (<= e (vector-length v))
            (cond
               ((< p e)
                  (iter-range-really v p (- e p)))
               ((= p e) #n)
               (else (error "vec-iter-range: bad range " (cons p e))))
            (error "vec-iter-range: end outside of vector: " e)))

      ;; iterate back to front

      ;; todo: vec-iterr could also chunk whole leaves directly with fixnums like vec-iterr
      (define (iterr-raw-leaf v last tl)
         (if (eq? last 0)
            tl
            (lets ((last (- last 1)))
               (cons (ref v last)
                  (λ () (iterr-raw-leaf v last tl))))))

      (define (iterr-leaf v p tl)
         (if (eq? p 1)
            (cons (ref v p) tl)
            (cons (ref v p) (λ () (iterr-leaf v (- p 1) tl)))))

      (define (iterr-any-leaf v tl)
         (case (type v)
            (type-vector-dispatch (iterr-any-leaf (ref v 1) tl))
            (type-bytevector (iterr-raw-leaf v (sizeb v) tl))
            (type-vector-leaf (iterr-leaf v (tuple-length v) tl))
            (else
               tl))) ; size field in root is a number → skip

      (define (vec-iterr-loop v p)
         (if (eq? type-fix- (type p))
            #n
            (iterr-any-leaf (vec-leaf-of v p)
               (λ () (vec-iterr-loop v (- p *vec-leaf-size*))))))

      (define (vec-iterr v)
         (lets
            ((end (vector-length v))
             (last (band end *vec-leaf-max*)))
            (if (eq? last 0) ; vec is empty or ends to a full leaf
               (if (eq? end 0) ; blank vector
                  #n
                  (vec-iterr-loop v (- end 1))) ; start from previous leaf
               (vec-iterr-loop v (- end 1)))))

      ;; vector folds

      (define (vec-fold  op st vec) (lfold  op st (vec-iter  vec)))
      (define (vec-foldr op st vec) (lfoldr op st (vec-iterr vec)))

      ;; list conversions

      (define (vector->list vec)
         (if (eq? (type vec) type-bytevector)
            ;; convert raw vectors directly to allow this to be used also for large chunks
            ;; which are often seen near IO code
            (bytevector->list vec)
            (vec-foldr cons #n vec)))

      (define (leaf-data leaf)
         (if (eq? (type leaf) type-bytevector)
            leaf
            (ref leaf 1)))

      ;;;
      ;;; vector map
      ;;;

      ;; fixme: vector-map <- placeholder
      (define (vector-map fn vec)
          (list->vector (lmap fn (vec-iter vec))))

      ;;;
      ;;; Vector ranges
      ;;;

      ;; fixme: proper vec-range not implemented
      (define (vec-range-naive vec from to) ; O(m log n)
         (list->vector (map (H vector-ref vec) (iota from 1 to))))

      (define vec-range vec-range-naive)


      ;;;
      ;;; Vector leaf data stream (mainly useful for IO)
      ;;;

      ;; vec → a stream of leaves
      (define (vec-leaves vec)
         (let ((end (vector-length vec)))
            (let loop ((pos 0))
               (if (< pos end)
                  (let ((data (leaf-data (vec-leaf-of vec pos))))
                     (pair data (loop (+ pos *vec-leaf-size*))))
                  #n))))

      ;; fixme: temporary vector append!
      (define (vec-cat a b)
         (list->vector
            (append
               (vector->list a)
               (vector->list b))))

      (define vec-rev
         (B list->vector vec-iterr))

      ;; fixme: make-vector does not share the nodes despite most being equal
      (define (make-vector n . elem?)
         (list->vector (make-list n (if (null? elem?) #f (car elem?)))))
      
      ;;;
      ;;; Vector construction
      ;;;

      (define (vector . lst)
         (list->vector lst))

      (example
         (vector-ref (make-vector 100 42) 99) = 42
         (vector-map (lambda (x) (+ x 10)) (vector 1 2 3)) = (vector 11 12 13)
         (vec-fold  - 0 (vector 1 2 3 4)) = (fold  - 0 (list 1 2 3 4))
         (vec-foldr - 0 (vector 1 2 3 4)) = (foldr - 0 (list 1 2 3 4))
       )                  
))
