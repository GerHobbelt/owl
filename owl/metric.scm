#| doc
Formatting Numbers

This library is for converting potentially large integers to more readable and
less accurate form. Mainly used for formatting output of ,time repl command.
|#

(define-library (owl metric)

   (import
      (owl core)
      (only (owl render) str)
      (owl list)
      (owl proof)
      (owl math)
      (owl time))

   (export
      format-time            ;; ns -> str
      format-number          ;; n -> string
      format-number-base2    ;, n -> string, base 1024
      )

   (begin

      ; 2 digits, for decimal
      (define (decimal-pad n base)
         (let ((n (quotient n 10)))
            (cond
               ((< n 10) (str "0" n))
               (else (str n)))))

      (define (metric-format n sub units base)
         (cond
            ((< n 0)
               (str "-" (metric-format (- 0 n) sub units base)))
            ((or (null? (cdr units)) (< n base))
               (str n "." (decimal-pad sub base) (car units)))
            (else
               (metric-format (quotient n base) (remainder n base) (cdr units) base))))

      (define (format-time ns)
         (metric-format ns 0
            '("ns" "µs" "ms" "s") 1000))

      (define (format-number ns)
         (metric-format ns 0
            '("" "k" "M" "G" "T" "P") 1000))

      (define (format-number-base2 bytes)
         (metric-format bytes 0
            '("" "K" "M" "G" "T" "P" "E" "Z" "Y") 1024))

      (example
         (format-time 123456789) = "123.45ms"
         (format-number 4096)    = "4.09k"
         (format-number-base2 4096) = "4.00K")
))
