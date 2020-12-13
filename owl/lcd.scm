#| doc
Lambda Calculus Data

This library defines macros for using functions (which are lambda expressions) as data
structures.

|#

(define-library (owl lcd)

   (import
      (owl core))

   (export
      define-sum-type
      prod
      trod)

   (begin

      (define-syntax define-sum-type
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

            ))

      ;; apply to a function or use (lets ((a b c <- (prod 1 2 3))) ...)
      (define-syntax prod
         (syntax-rules ()
            ((prod val ...)
               (lambda (c) (c val ...)))))


      (define-syntax trod
         (syntax-rules ()
            ((trod tag val ...)
               (lambda (c)
                  ((c tag) val ...)))))


))



