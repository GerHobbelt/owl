(import (owl sys))

(define (execvp args)
   (let ((cmd (car args)))
      (if (m/\// cmd)
         (exec cmd args)
         (fold
            (λ (r path)
               (if (m/^\// path)
                  (exec (string-append path "/" cmd) args))
               r)
            #f
            (c/:/ (getenv "PATH"))))))

(define (run args)
   (if-lets ((pid (fork)))
      (if (eq? pid #t)
         (or (execvp args) (halt 1))
         (equal? (wait pid) '(1 . 0)))
      #f))

(define (build-stat args)
   (lets
      ((start (time-ns))
       (ret (run args))
       (final (time-ns)))
      (print "bootstrap: " (quotient (- final start) 1000000) " ms")
      (print "fasl: " (file-size (last args "")) " b")
      ret))

(define (fixed-point args)
   (and
      ; check that the compiling image passes tests
      (run '("sh" "tests/run" "all" "bin/vm" "fasl/boot.fasl"))
      ; compile fasl
      (build-stat args)
      (if (run '("cmp" "-s" "fasl/boot.fasl" "fasl/bootp.fasl"))
         ; move the new image to ol.fasl, if it is a fixed point
         (rename "fasl/bootp.fasl" "fasl/ol.fasl")
         ; otherwise recompile
         (and (rename "fasl/bootp.fasl" "fasl/boot.fasl") (fixed-point args)))))

(λ (args)
   (let ((args (cdr args)))
      (if
         (if (string=? (car args) "-f")
            (fixed-point (cdr args))
            (build-stat args))
         0 1)))
