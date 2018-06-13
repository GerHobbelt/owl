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

      (define (bytevector . lst)
         (raw lst type-bytevector))

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
         (raw (bytevectors->list lst) type-bytevector))

      (define bytevector-copy
         (case-lambda
            ((vec)
               vec)
            ((vec top)
               (raw (bytevector-copy->list vec top (sizeb vec) '()) type-bytevector))
            ((vec top end)
               (raw (bytevector-copy->list vec top end '()) type-bytevector))))

      (define (bytevector->list vec)
         (bytevector-copy->list vec 0 (sizeb vec) '()))

      (define list->bytevector
         (C raw type-bytevector))
))
