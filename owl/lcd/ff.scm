
(define-library (owl lcd ff)

   (import
      (owl core))

   (export
      put
      get
      ff-fold
      ff-foldr)

   (begin

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
         (left
            (λ (ll lk lv lr)
               (right
                  (λ (A B C D)
                     (red (color-black left) key val (color-black right)))
                  (λ (A B C D)
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
                                 (black left key val right))))))))
            (λ (A B C D)
               (black left key val right))))

      (define (black-right left key val right)
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
            s))))

(import (owl lcd ff))

(define x
   (fold
      (λ (ff x)
         (put ff x (+ x 1000)))
      #f
      (iota 0 1 10000)))

(ff-fold (λ (_ k v) (print k " = " v)) 0 x)




