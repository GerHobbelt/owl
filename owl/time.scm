(define-library (owl time)

   (export
      elapsed-real-time
      time
      time-ms
      time-ns)

   (import
      (owl core)
      (owl io)
      (owl syscall)
      (owl math)
      (only (owl sys) clock_gettime CLOCK_REALTIME)
      (scheme write))

   (begin

      (define (time-ns)
         (clock_gettime (CLOCK_REALTIME)))

      (define (elapsed-real-time thunk)
         (display "timing: ")
         (lets
            ((ns (time-ns))
             (res (thunk)))
            (print (quotient (- (time-ns) ns) 1000000) "ms")
            res))

      (define (time)
         (quotient (time-ns) 1000000000))

      (define (time-ms)
         (quotient (time-ns) 1000000))
))
