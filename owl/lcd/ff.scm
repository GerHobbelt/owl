
(define-library (owl lcd ff)

   (import
      (owl core)
      (owl list)
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

      (define get
         (case-lambda
            ((ff key)
               (ff-get ff key #f ff-get))
            ((ff key def)
               (ff-get ff key def ff-get))))

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
         (ff-fold
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

      (define (del-nlogn ff to-del) ;; TEMPORARY
         (ff-fold
            (λ (ff k v)
               (if (eq? k to-del)
                  ff
                  (put ff k v)))
            empty ff))

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

      ;; todo: optimize, since many colors are known
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
                        (either middle
                           (λ (ml mk mv mr)
                              (either left
                                 (λ (ll lk lv lr)
                                    (either right
                                       (λ (rl rk rv rr)
                                          (red
                                             (red ll lk lv ml)
                                             mk mv
                                             (red mr rk rv rr))))))))
                        (either left
                           (λ (a xk xv b)
                              (either right
                                 (λ (c yk yv d)
                                    (red a xk xv
                                       (red middle yk yv d))))))))
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
                           (either left
                              (λ (ll lk lv lr)
                                 (either right
                                    (λ (rl rk rv rr)
                                       (red
                                          (black ll lk lv ml)
                                          mk mv
                                          (black mr rk rv rr)))))))
                        #f)
                     (either left
                        (λ (ll lk lv lr)
                           (either right
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
         (if (ff-has? ff key)
            (let ((ff (deln ff key)))
               (if (red? ff)
                  (color-black ff)
                  ff))
            ff))

))

'(import (owl lcd ff))
'(define x (fold (λ (ff x) (put ff x (+ x 1000))) empty (iota 0 1 10)))
'(print (ff->list x))

'(print (-> x (del 5) (del 8) (del 0) ff->list))

'(define x
   (lfold
      (λ (ff x)
         (put ff x (+ x 1000)))
      #f
      (liota 0 1 1000000)))

'(print (ff-fold (λ (sum k v) (+ sum v)) 0 x))




