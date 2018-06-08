(define-library (owl equal-prim)

   (export
      equal-prim?
      simple-equal?)

   (import
      (owl defmac))

   (begin

      (define (eq-fields a b eq pos)
         (cond
            ((eq? pos 0)
               #true)
            ((eq (ref a pos) (ref b pos))
               (lets ((pos _ (fx- pos 1)))
                  (eq-fields a b eq pos)))
            (else #false)))

      (define (eq-bytes a b pos)
         (if (eq? (ref a pos) (ref b pos))
            (if (eq? pos 0)
               #true
               (lets ((pos _ (fx- pos 1)))
                  (eq-bytes a b pos)))
            #false))

      (define (equal-prim? self a b)
         (if (eq? a b)
            #true
            (let ((ta (type a)))
               (if (eq? ta type-symbol)
                  #false ; would have been eq?, because they are interned
                  (let ((sa (size a)))
                     (cond
                        ; a is immediate -> would have been eq?
                        ((not sa) #false)
                        ; same size
                        ((eq? sa (size b))
                           ; check equal types
                           (if (eq? ta (type b))
                              (if (raw? a)
                                 ; equal raw objects, check bytes
                                 (lets
                                    ((ea (sizeb a)) ; raw objects may have padding bytes, so recheck the sizes
                                     (eb (sizeb b)))
                                    (if (eq? ea eb)
                                       (if (eq? ea 0)
                                          #true
                                          (lets ((ea _ (fx- ea 1)))
                                             (eq-bytes a b ea)))
                                       #false))
                                 ; equal ntuples, check fields
                                 (eq-fields a b self sa))
                              #false))
                        (else #false)))))))

      ;; equality (mainly for theorem checks) which does not depend on
      ;; any libraries one would like to be able to test
      (define (simple-equal? a b)
         (equal-prim? simple-equal? a b))
))
