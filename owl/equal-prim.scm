(define-library (owl equal-prim)

   (export
      equal-prim?
      simple-equal?)

   (import
      (owl core))

   (begin

      (define (eq-fields a b cmp end)
         (or
            (eq? end 1)
            (lets ((end _ (fx- end 1)))
               (and
                  (cmp (ref a end) (ref b end))
                  (eq-fields a b cmp end)))))

      (define (eq-bytes a b end)
         (or
            (eq? end 0)
            (lets ((end _ (fx- end 1)))
               (and
                  (eq? (ref a end) (ref b end))
                  (eq-bytes a b end)))))

      (define (equal-prim? self a b)
         (if (eq? a b)
            #true
            (let ((ta (type a)))
               (if (eq? ta type-symbol)
                  #false ; would have been eq?, because they are interned
                  (let ((sa (object-size a)))
                     (cond
                        ; a is immediate -> would have been eq?
                        ((eq? sa 0) #f)
                        ; same size
                        ((eq? sa (object-size b))
                           ; check equal types
                           (if (eq? ta (type b))
                              (if-lets ((ea (sizeb a)))
                                 ; equal raw objects, check bytes
                                 (and
                                    (eq? ea (sizeb b)) ; raw objects may have padding bytes, so recheck the sizes
                                    (eq-bytes a b ea))
                                 ; equal ntuples, check fields
                                 (eq-fields a b self sa))
                              #false))
                        (else #false)))))))

      ;; equality (mainly for theorem checks) which does not depend on
      ;; any libraries one would like to be able to test
      (define (simple-equal? a b)
         (equal-prim? simple-equal? a b))
))
