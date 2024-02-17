(define-library (owl time)

   (export
      time
      time-ms
      time-ns)

   (import
      (owl core)
      (owl syscall)
      (owl math)
      (only (owl sys) clock_gettime CLOCK_REALTIME))

   (begin

      (define (time-ns)
         (clock_gettime (CLOCK_REALTIME)))

      (define (time)
         (quotient (time-ns) 1000000000))

      (define (time-ms)
         (quotient (time-ns) 1000000))
))
