
;; remove these after next fasl update
   ,load "owl/io.scm" ;; temporarily for copy-file
   (import (owl io))

(import (owl sys))

(define (build-stat args)
   (lets
      ((start (time-ns))
       (ret (system args))
       (final (time-ns)))
      (print "bootstrap: " (quotient (- final start) 1000000) " ms")
      (print "fasl: " (file-size (last args "")) " b")
      ret))

(define (fixed-point args)
   (if (not (file? "fasl/boot.fasl"))
      ;; use supplied init.fasl from repository as starting point
      (if (copy-file "fasl/init.fasl" "fasl/boot.fasl")
         (begin
            (print "Using init.fasl as the initial step")
            (fixed-point args))
         (begin
            (print "Failed to copy fasl/init.fasl -> fasl/boot.fasl")
            #false)))
   (and
      ; compile bootp.fasl
      (build-stat args)
      (if (system '("cmp" "-s" "fasl/boot.fasl" "fasl/bootp.fasl"))
         (begin
            (print "fasl fixed point reached")
            (rename "fasl/bootp.fasl" "fasl/ol.fasl"))
         (and
            ; check that the compiling image passes tests
            (system '("sh" "tests/run" "all" "bin/vm" "fasl/bootp.fasl"))
            (rename "fasl/bootp.fasl" "fasl/boot.fasl")
            (fixed-point args)))))

(λ (args)
   (let ((args (cdr args)))
      (if
         (if (string=? (car args) "-f")
            (fixed-point (cdr args))
            (build-stat args))
         0 1)))
