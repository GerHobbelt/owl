(define-library (owl bytevector)
   (export
      bytevector
      bytevector?
      bytevector-length
      bytevector-u8-ref
      bytevector-append
      bytevector-copy
      bytevector->list
      list->bytevector)

   (import
      (owl defmac))

   (begin

      (define list->bytevector
         (C raw type-bytevector))

      (define (bytevector . lst)
         (list->bytevector lst))

      (define (bytevector? obj)
         (eq? (type obj) type-bytevector))

      (define bytevector-length sizeb)

      (define bytevector-u8-ref ref)

      (define (bytevector-copy->list vec top end tail)
         (if (lesser? top end)
            (lets ((end _ (fx- end 1)))
               (bytevector-copy->list vec top end (cons (ref vec end) tail)))
            tail))

      (define (bytevectors->list lst)
         (if (null? lst)
            lst
            (lets ((vec lst lst))
               (bytevector-copy->list vec 0 (sizeb vec) (bytevectors->list lst)))))

      (define (bytevector-append . lst)
         (list->bytevector (bytevectors->list lst)))

      (define bytevector-copy
         (case-lambda
            ((vec)
               vec)
            ((vec top)
               (list->bytevector (bytevector-copy->list vec top (sizeb vec) '())))
            ((vec top end)
               (list->bytevector (bytevector-copy->list vec top end '())))))

      (define (bytevector->list . lst)
         (bytevectors->list lst))
))
