#| doc
Boolean Values

This trivial library defines boolean values.
|#

(define-library (owl boolean)

   (export
      boolean?
      boolean=?

      true      ;; these look more natural in code
      false
   )

   (import
      (owl core)
      (only (owl list) every))

   (begin

      (define true #true)
      (define false #false)

      (define (boolean? x)
         (or (eq? x #true) (eq? x #false)))

      (define (boolean=? x . lst)
         (and (boolean? x) (every (C eq? x) lst)))

))
