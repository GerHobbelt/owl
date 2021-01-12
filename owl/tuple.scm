#| doc
Tuples

Tuples are an early simple data structure for holding multiple values. Values
are indexed from 1 and there is little error detection apart from range checks.

  > (define x (list->tuple '(a b c)))
  > (ref x 1)
  'a
  > (size x)
  3
  > (equal? x (tuple 'a 'b 'c))
  #true

|#

(define-library (owl tuple)

   (export
      tuple?
      tuple-length
      list->tuple
      tuple->list)

   (import
      (owl core)
      (owl list)
      (only (owl syscall) error))

   (begin
      (define (tuple? x)
         (eq? (type x) type-tuple))

      (define (tuple-length x)
         (lets ((len u (fx- (object-size x) 1)))
            (and (eq? u 0) len)))

      (define (list->tuple lst)
         (let ((l (len lst)))
            (if l
               (listuple 2 l lst)
               (error "list does not fit a tuple: length " l))))

      (define tuple->list
         object->list)
      
))
