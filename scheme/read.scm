(define-library (scheme read)

   (import
      (owl core)
      (owl lazy)
      (owl list)
      (owl port)
      (owl sexp))

   (export
      read
      read-all)

   (begin

      (define owl-read read)

      (define (read . args)
         (owl-read
            (if (null? args)
               stdin
               (car args))))

      (define (read-all . args)
         (pipe
            (if (null? args) stdin (car args))
            read-ll
            force-ll))))



