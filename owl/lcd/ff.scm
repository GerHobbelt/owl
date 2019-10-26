
(define-library (owl lcd ff)

   (import
      (owl core)
      (owl list)
      (owl function)
      ;(owl io) ;; debug
      (prefix (owl ff) old-)
      )

   (export
      put
      get
      getf ;; COMPAT REMOVE
      del
      empty
      empty?
      fupd
      ff-fold
      ff-foldr
      ff-union
      list->ff
      ff->list
      ff-iter
      ff-min
      ff-max
      keys

      upgrade     ;; old ff -> new COMPAT REMOVE
      downgrade   ;; new ff -> old COMPAT REMOVE
      )

   (begin

      (define empty #f)

      (define empty? (C eq? empty))

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

      (define (color-black node)
         (node black black))

      (define (color-red node)
         (node red red))

      (define (black-left left key val right)
         ;(print " - black-left")
         (left
            (λ (ll lk lv lr)
               ;(print "x")
               (if right
                  (right
                     (λ (A B C D)
                        ;(print "xa")
                        (red (color-black left) key val (color-black right)))
                     (λ (A B C D)
                        ;(print "xb")
                        (ll
                           (λ (a xk xv b)
                              (let ((yk lk) (yv lv) (c lr))
                                 (red (black a xk xv b) yk yv (black c key val right))))
                           (λ (A B C D)
                              (lr
                                 (λ (b yk yv c)
                                    (let ((a ll) (xk lk) (xv lv))
                                       (red (black a xk xv b) yk yv (black c key val right))))
                                 (λ (A B C D)
                                    (black left key val right)))))))
                  (black left key val right)))
            (λ (A B C D)
               ;(print "xx")
               (black left key val right))))

      (define (black-right left key val right)
         ;(print " - black-right")
         (right
            (λ (rl rk rv rr)
               (if rl
                  (rl
                     (λ (b yk yv c)
                        (lets ((zk rk) (zv rv) (d rr))
                           (red
                              (black left key val b)
                              yk yv
                              (black c zk zv d))))
                     (λ (A B C D)
                        (if rr
                           (rr
                              (λ (c zk zv d)
                                 (let ((b rl) (yk rk) (yv rv))
                                    (red
                                       (black left key val b)
                                       yk yv
                                       (black c zk zv d))))
                              (λ (A B C D)
                                 (black left key val right)))
                           (black left key val right))))
                  (if rr
                     (rr
                        (λ (c zk zv d)
                           (let ((b rl) (yk rk) (yv rv))
                              (red
                                 (black left key val b)
                                 yk yv
                                 (black c zk zv d))))
                        (λ (A B C D)
                           (black left key val right)))
                     (black left key val right))))
            (λ (A B C D)
               (black left key val right))))

      (define (putn node key val)
         ;(print " - putn " key " = " val ", " node)
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
         ;(print "ff-get " key " from " ff)
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

      (define getf get) ;; COMPAT REMOVE

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
         ;(print "putting " k " = " v " to " ff)
         (color-black (putn ff k v)))

      ;;; utilities

      (define fupd put) ;; can optimized, because can assume key is there

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

      (define (del ff to-del) ;; TEMPORARY
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

      (define (ff-union a b collide)
         (ff-fold
            (λ (a bk bv)
               (let ((x (get a bk not-there)))
                  (if (eq? x not-there)
                     (put a bk bv)
                     (fupd a bk
                        (collide x bv)))))
            a b))

      (define (upgrade x)
         (if (old-ff? x)
            (list->ff (old-ff->list x))
            x))

      (define (downgrade x)
         (cond
            ((eq? x empty) old-empty)
            ((function? x) (old-list->ff (ff->list x)))
            ((old-ff? x) x)
            (else
               (car 'downgrade-not-ff))))

      ))

; (import (owl lcd ff))

'(define x
   (lfold
      (λ (ff x)
         (put ff x (+ x 1000)))
      #f
      (liota 0 1 1000000)))

'(print (ff-fold (λ (sum k v) (+ sum v)) 0 x))




