
(define-library (owl function)

   (export
      bytecode?
      function?
      procedure?)

   (import
      (owl core)
      (only (owl syscall) interact))

   (begin

      (define (bytecode? x)
         (eq? type-bytecode (type x)))

      ;; raw bytecode vector, 1-level (proc) or 2-level (clos) function
      (define (function? x)
         (let ((t (type x)))
            (or (eq? t type-bytecode)
                (eq? t type-proc)
                (eq? t type-clos))))

      (define procedure? function?)))
