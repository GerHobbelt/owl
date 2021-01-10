#| doc
Finite Functions

This library defines finite functions. They are commonly used in Owl to
construct efficient key-value mappings. A finite function is much like
an association list, which are frequently used in Lisp. The main difference
is that finite functions are represented as red-black trees internally,
so the performance is much better when there are many keys.

The value `empty` is an empty finite function. It contains no mappings.
You can read the value of of a mapping with `get`, which accepts a
finite function, a key to be fetched and optionally a default value to
return if the key is not mapped. `put` can be used to extend a ff and
`del` removes a mapping.

  > (define f (put (put empty 'foo 100) 'bar 42))
  > f
  #<function>
  > (get f 'foo #f)
  100
  > (get f 'x #f)
  #f
  > (get (del f 'foo) 'foo #f)
  #f
This data structure is made possible by the fact, that Owl has an
order-preserving garbage collector. Therefore we have a total order on objects,
which makes it possible to build balanced trees by comparison.

|#

(define-library (owl ff)

   (import
      (owl core)
      (owl list)
      (owl proof)
      (owl function))

   (export
      put
      get
      del
      empty
      empty?
      fupd
      ff-fold
      ff-foldr
      ff-union
      ff-has?
      list->ff
      ff->list
      ff-iter
      ff-min
      ff-max
      ff-map
      ff
      keys)

   (begin

      ;; note: it is possible to define ff? by checking the bytecode

      ;;;
      ;;; Construction
      ;;;

      (define black
         (λ (l k v r)
            (if l
               (if r
                  (λ (R B) (B l k v r))
                  (λ (R B) (B l k v #f)))
               (if r
                  (λ (R B) (B #f k v r))
                  (λ (R B) (B  #f k v #f))))))

      (define red
         (λ (l k v r)
            (if l
               (if r
                  (λ (R B) (R l k v r))
                  (λ (R B) (R l k v #f)))
               (if r
                  (λ (R B) (R #f k v r))
                  (λ (R B) (R  #f k v #f))))))

      ;;;
      ;;; Utils
      ;;;

      (define empty #f)

      (define empty? (C eq? empty))

      (define (color-black node)
         (node black black))

      (define (color-red node)
         (node red red))

      (define (red? node)
         (if node
            (node
               (λ (l k v r) #t)
               (λ (l k v r) #f))
            #f))

      ;;;
      ;;; Insertion
      ;;;

      (define (black-left left key val right)
         (left
            (λ (ll lk lv lr)
               (if (red? right)
                  (red (color-black left) key val (color-black right))
                  (cond
                     ((red? ll)
                        (ll
                           (λ (a xk xv b)
                              (let ((yk lk) (yv lv) (c lr))
                                 (red (black a xk xv b) yk yv (black c key val right))))
                           #f))
                     ((red? lr)
                        (lr
                           (λ (b yk yv c)
                              (let ((a ll) (xk lk) (xv lv))
                                 (red (black a xk xv b) yk yv (black c key val right))))
                           #f))
                     (else
                        (black left key val right)))))
            (λ (A B C D)
               (black left key val right))))

      (define (black-right left key val right)
         (right
            (λ (rl rk rv rr)
               (cond
                  ((red? rl)
                     (rl
                        (λ (b yk yv c)
                           (lets ((zk rk) (zv rv) (d rr))
                              (red
                                 (black left key val b)
                                 yk yv
                                 (black c zk zv d))))
                        #f))
                  ((red? rr)
                     (rr
                        (λ (c zk zv d)
                           (let ((b rl) (yk rk) (yv rv))
                              (red
                                 (black left key val b)
                                 yk yv
                                 (black c zk zv d))))
                        #f))
                  (else
                     (black left key val right))))
            (λ (A B C D)
               (black left key val right))))

      (define (putn node key val)
         (if node
            (node
               (λ (left this this-val right) ;; red
                  (cond
                     ((lesser? key this)
                        (red (putn left key val) this this-val right))
                     ((eq? key this)
                        (red left key val right))
                     (else
                        (red left this this-val (putn right key val)))))
               (λ (left this this-val right) ;; black
                  (cond
                     ((lesser? key this)
                        (black-left (putn left key val) this this-val right))
                     ((eq? key this)
                        (black left key val right))
                     (else
                        (black-right left this this-val (putn right key val))))))
            (red #f key val #f)))

      ;; key known to occur
      (define (ff-update node key val)
         (node
            (λ (left this this-val right) ;; red
               (cond
                  ((lesser? key this)
                     (red (ff-update left key val) this this-val right))
                  ((eq? key this)
                     (red left key val right))
                  (else
                     (red left this this-val (ff-update right key val)))))
            (λ (left this this-val right) ;; black
               (cond
                  ((lesser? key this)
                     (black (ff-update left key val) this this-val right))
                  ((eq? key this)
                     (black left key val right))
                  (else
                     (black left this this-val (ff-update right key val)))))))

      (define fupd ff-update)


      ;;;
      ;;; Derived Operations
      ;;;

      (define (ff-min ff def)
         (if ff
            (ff
               (λ (l k v r) (ff-min l v))
               (λ (l k v r) (ff-min l v)))
            def))

      (define (ff-max ff def)
         (if ff
            (ff
               (λ (l k v r) (ff-max r v))
               (λ (l k v r) (ff-max r v)))
            def))

      ;; (k v -> v') ff -> ff'
      (define (ff-map fn ff)
         (if ff
            (ff
               (lambda (l k v r)
                  (red (ff-map fn l) k (fn k v) (ff-map fn r)))
               (lambda (l k v r)
                  (black (ff-map fn l) k (fn k v) (ff-map fn r))))
            empty))

      (define (ff-get ff key def self)
         (if ff
            (ff
               (λ (l k v r)
                  (cond
                     ((eq? key k) v)
                     ((lesser? key k) (self l key def self))
                     (else (self r key def self))))
                (λ (l k v r)
                  (cond
                     ((eq? key k) v)
                     ((lesser? key k) (self l key def self))
                     (else (self r key def self)))))
            def))

      (define (get ff key . def)
         (if (eq? def #null)
            (ff-get ff key #f ff-get)
            (ff-get ff key (car def) ff-get)))

      (define (ff-has? ff key)
         (if ff
            (ff
               (λ (l k v r)
                  (cond
                     ((eq? key k) #t)
                     ((lesser? key k) (ff-has? l key))
                     (else (ff-has? r key))))
               (λ (l k v r)
                  (cond
                     ((eq? key k) #t)
                     ((lesser? key k) (ff-has? l key))
                     (else (ff-has? r key)))))
            #f))

      (define (put ff k v)
         (color-black (putn ff k v)))

      ;;; utilities

      (define (ff-foldr o s t)
         (if t
            (let
               ((step
                  (λ (l k v r)
                     (if l
                        (if r
                           (ff-foldr o
                              (o (ff-foldr o s r) k v)
                              l)
                           (ff-foldr o (o s k v) l))
                        (if r
                           (o (ff-foldr o s r) k v)
                           (o s k v))))))
               (t step step))
            s))

      (define (ff-fold o s t)
         (if t
            (let
               ((step
                  (λ (l k v r)
                     (if l
                        (if r
                           (ff-fold o
                              (o (ff-fold o s l) k v)
                              r)
                           (o (ff-fold o s l) k v))
                        (if r
                           (ff-fold o (o s k v) r)
                           (o s k v))))))
               (t step step))
            s))

      (define (keys ff)
         (ff-foldr
            (λ (out k v) (cons k out))
            '()
            ff))

      (define (ff-iter ff)
         (let loop ((ff ff) (tail null))
            (if ff
               (ff
                  (λ (l k v r)
                     (loop l
                        (cons (cons k v)
                           (λ () (loop r tail)))))
                  (λ (l k v r)
                     (loop l
                        (cons (cons k v)
                           (λ () (loop r tail))))))
               tail)))

      (define (list->ff lst)
         (fold
            (λ (ff elem)
               (put ff (car elem) (cdr elem)))
            #f lst))

      (define (ff->list ff)
         (ff-foldr
            (λ (out k v)
               (cons (cons k v) out))
            #null ff))

      (define not-there '(x))

      ;; preferably small b
      (define (ff-union a b collide)
         (ff-fold
            (λ (a bk bv)
               (let ((x (get a bk not-there)))
                  (if (eq? x not-there)
                     (put a bk bv)
                     (fupd a bk
                        (collide x bv)))))
            a b))

      ;;;
      ;;; Deletion
      ;;;

      (define (ball-left left key val right)
         (cond
            ((red? left)
               (red (color-black left) key val right))
            ((red? right)
               (right
                  (λ (r zk zv c)
                     (r
                        (λ (a yk yv b)
                           (red
                              (black left key val a)
                              yk yv
                              (black-right b zk zv (color-red c))))
                        (λ (a yk yv b)
                           (red
                              (black left key val a)
                              yk yv
                              (black-right b zk zv (color-red c))))))
                  #f))
            (else
               (black-right left key val (color-red right)))))

      (define (ball-right left key val right)
         (cond
            ((red? right)
               (red left key val (color-black right)))
            ((red? left)
               (left
                  (λ (a xk xv b)
                     (b
                        (λ (b yk yv c)
                           (red
                              (black-left (color-red a) xk xv b)
                              yk yv
                              (black c key val right)))
                        (λ (b yk yv c)
                           (red
                              (black-left (color-red a) xk xv b)
                              yk yv
                              (black c key val right)))))
                  #f))
            (else
               (black-left (color-red left) key val right))))

      (define (ffcar node)
         (node
            (λ (l k v r) l)
            (λ (l k v r) l)))

      (define (ffcdr node)
         (node
            (λ (l k v r) r)
            (λ (l k v r) r)))

      (define (either node cont) (node cont cont))

      (define (app left right)
         (cond

            ;;; if other branch is empty
            ((eq? left  empty) right)
            ((eq? right empty) left)

            ;;; otherwise full nodes
            ((red? left)
               (if (red? right)
                  (let ((middle (app (ffcdr left) (ffcar right))))
                     (if (red? middle)
                        (middle
                           (λ (ml mk mv mr)
                              (left
                                 (λ (ll lk lv lr)
                                    (right
                                       (λ (rl rk rv rr)
                                          (red
                                             (red ll lk lv ml)
                                             mk mv
                                             (red mr rk rv rr)))
                                       #f))
                                 #f))
                           #f)
                        (left
                           (λ (a xk xv b)
                              (right
                                 (λ (c yk yv d)
                                    (red a xk xv
                                       (red middle yk yv d)))
                                 #f))
                           #f)))
                  (left
                     (λ (a xk xv b)
                        (red a xk xv (app b right)))
                     #f)))

            ((red? right)
               (right
                  (λ (rl rk rv rr)
                     (red (app left rl) rk rv rr))
                  #f))
            (else
               ;;; both are black
               (let ((middle (app (ffcdr left) (ffcar right))))
                  (if (red? middle)
                     (middle
                        (λ (ml mk mv mr)
                           (left #f
                              (λ (ll lk lv lr)
                                 (right #f
                                    (λ (rl rk rv rr)
                                       (red
                                          (black ll lk lv ml)
                                          mk mv
                                          (black mr rk rv rr)))))))
                        #f)
                     (left #f
                        (λ (ll lk lv lr)
                           (right #f
                              (λ (rl rk rv rr)
                                 (ball-left
                                    ll lk lv
                                    (black middle rk rv rr)))))))))))

      (define (deln ff key)
         (if (eq? ff empty)
            ff
            (either ff
               (λ (left this-key val right)
                  (cond
                     ((lesser? key this-key)
                        (let ((sub (deln left key)))
                           (cond
                              ((eq? sub left)  ;; ← check usefulness, intended to avoid rebalancing if not there
                                 ff)
                              ((red? left)
                                 (red sub this-key val right))
                              (else
                                 (ball-left sub this-key val right)))))
                     ((eq? key this-key)
                        (app left right))
                     (else
                        (let ((sub (deln right key)))
                           (cond
                              ((eq? sub right) ;; ← check usefulness
                                 ff)
                              ((red? right)
                                 (red left this-key val sub))
                              (else
                                 (ball-right left this-key val sub))))))))))

      (define (del ff key)
         (let ((ff (deln ff key)))
            (if (red? ff)
               (color-black ff)
               ff)))

      (define-syntax ff
         (syntax-rules ()
            ((ff a b . cs)
               (put (ff . cs) a b))
            ((ff) empty)
            ((ff a)
               (syntax-error "Uneven ff construction. Last argument: " a))))

      (example
         let test = (list->ff '((a . 1) (b . 2) (c . 3)))
         (get test 'a 0) = 1
         (get test 'x 0) = 0
         (keys test) = '(a b c)
         (ff->list empty) = ()
         (ff->list (put empty 'a 42)) = '((a . 42))
         (ff->list (put test 'a 42)) = '((a . 42) (b . 2) (c . 3))
         (ff->list (put test 'd 42)) = '((a . 1) (b . 2) (c . 3) (d . 42))
         (ff->list (del test 'a)) = '((b . 2) (c . 3))
         (ff->list (del test 'x)) = '((a . 1) (b . 2) (c . 3))
         (ff-fold (λ (out k v) (cons v out)) #n test) = '(3 2 1)
         (ff-foldr (λ (out k v) (cons v out)) #n test) = '(1 2 3)
         (ff-map (lambda (k v) (null? v)) (ff 'a 1 'b '())) = (ff 'a #f 'b #t)
         )


))




