(define-library (owl sum)

   (import
      (owl core)
      (owl syntax-rules))

   (export define-sum-type)

   (begin

      (define-syntax-ng define-sum-type
         (syntax-rules (__repl_begin __options __walk __names define define-syntax syntax-rules)
            ((define-sum-type name
               __walk ()
               __options ((body option . args) ...)
               __names names)
             (__repl_begin
                (quote
                   ((define (option . args)
                         (lambda names (option . args)))
                    ...
                    (define-syntax name
                         (syntax-rules names
                            ((name value ((option . args) . body) ...)
                               (value
                                  (lambda args . body) ...))))))))

            ((define-sum-type name __walk ((option . args) . rest) __options (o ...)  __names (n ...))
               (define-sum-type name
                  __walk rest
                  __options (o ... (body option . args))
                  __names (n ... option)))

            ((define-sum-type name . options)
               (define-sum-type name __walk options __options () __names ()))))))
