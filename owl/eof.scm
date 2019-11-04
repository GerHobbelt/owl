(define-library (owl eof)

   (export
      eof-object
      eof-object?)

   (import
      (owl core))

   (begin

      (define *eof-object*
         (fxxor '() 4))

      (define (eof-object)
         *eof-object*)

      (define eof-object?
         (C eq? *eof-object*))
))
