#| doc
Byte Vectors

Byte vectors are vectors holding only numbers in the range 0-255. They are
internally represented as chunks of memory in the garbage collected heap.
Typical use cases for them is packing data for an operating system call, or
receiving data from an external source.

Vectors and byte vectors differ from Scheme in that they are not disjoint
types. Regular vectors can be, or can contain, byte vectors if the content
happens to fit in the range representable by bytes. This makes it possible
to use a more compact representation of data where possible. Representation
changes to regular vectors where necessary, much like numbers are converted
from fixnums to bignums where the values would no longer be representable
by fixnums.

|#

(define-library (owl bytevector)
   (export
      bytevector
      bytevector?
      bytevector-length
      bytevector-u8-ref
      bytevector-append
      bytevector-concatenate
      bytevector-concatenate->list
      bytevector-copy
      bytevector->list
      list->bytevector)

   (import
      (owl core))

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

      (define (bytevector-concatenate->list lst)
         (if (null? lst)
            lst
            (lets ((vec lst lst))
               (bytevector-copy->list vec 0 (sizeb vec) (bytevector-concatenate->list lst)))))

      (define (bytevector-concatenate lst)
         (list->bytevector (bytevector-concatenate->list lst)))

      (define (bytevector-append . lst)
         (bytevector-concatenate lst))

      (define bytevector-copy
         (case-lambda
            ((vec)
               vec)
            ((vec top)
               (list->bytevector (bytevector-copy->list vec top (sizeb vec) #n)))
            ((vec top end)
               (list->bytevector (bytevector-copy->list vec top end #n)))))

      (define (bytevector->list . lst)
         (bytevector-concatenate->list lst))
))
