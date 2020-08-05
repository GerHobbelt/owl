#| doc
Functional Data Types

This library allows defining algebraic data types as functions.
|#
(define-library (owl sum)

   (import
      (owl core)
      (owl syntax-rules))

   (export define-sum-type)

   (begin

      (define-syntax-ng define-sum-type
         (syntax-rules (__repl_begin __options __names __expected define define-syntax syntax-rules syntax-error)
            ((define-sum-type name
               __options ((body option (arg fresh) ...) ...)
               __names names
               __expected what)
             (__repl_begin
                (quote
                   ((define (option fresh ...)
                         (lambda names (option fresh ...)))
                    ...
                    (define-syntax name
                         (syntax-rules (arg ... . names) ;; <- not allowed yet, arg ellipsis lifting
                            ;; valid case
                            ((name value ((option fresh ...) . body) ...)
                               (value
                                  (lambda (fresh ...) . body) ...))
                            ((name . rest)
                               (syntax-error name "expects" (option ...)))
                            ))))))

            ((define-sum-type name (option arg ...) ...)
               (define-sum-type name
                  __options ((body-var option (arg fresh) ...) ...)
                  __names (option ...)
                  __expected ((option arg ...) ...)))

            ))))



