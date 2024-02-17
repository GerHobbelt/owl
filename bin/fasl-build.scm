
;; Only if core update is necessary
;,load "owl/core.scm" ;; temporarily for new core
;(import (owl core))

(import (prefix (owl sys) sys-))
(import (owl metric))

;; info -> fasl-size, initial-fasl-size, boot-ms, initial-boot-ms

(define (add-info info ms bytes)
   (pipe info
      ;(put 'boot-ms ms)
      (put 'fasl-size bytes)
      (put 'round (+ 1 (get info 'round 0)))
      (put
         (if (get info 'initial-boot-ms) 'boot-ms 'initial-boot-ms)
         ms)))

(define (report-deltas info)
   (lets ((bs (get info 'fasl-size 0))
          (obs (get info 'initial-fasl-size 0))
          (ms (get info 'boot-ms))
          (oms (get info 'initial-boot-ms)))
      (print "Build round " (get info 'round))
      (print " - heap " (format-number-base2 bs) "B (delta to initial "
         (format-number-base2 (- bs obs)) "B)")
      (if ms
         (print " - time " (format-time ms) " (delta to first " (format-time (- ms oms)) ")")
         (print " - time " (format-time oms) " (first boot)")
         )))


(define (fixed-point args info)
   (lets
      ((start (time-ns))
       (res (sys-system args))
       (elapsed (- (time-ns) start))
       (fasl-size (file-size "fasl/bootp.fasl"))
       (info (add-info info elapsed fasl-size)))
      (report-deltas info)
      (if (sys-system '("cmp" "-s" "fasl/boot.fasl" "fasl/bootp.fasl"))
         (begin
            (print "Fasl build complete. Selfcompilation fixed point reached.")
            (sys-rename "fasl/bootp.fasl" "fasl/ol.fasl"))
         (and
            ; check that the compiling image passes tests
            (sys-system '("sh" "tests/run" "all" "bin/vm" "fasl/bootp.fasl"))
            (sys-rename "fasl/bootp.fasl" "fasl/boot.fasl")
            (fixed-point args info)))))

(λ (args)
   (print)
   (print "Building compiler fixed point to fasl/ol.fasl.")
   (if (not (sys-file? "fasl/boot.fasl"))
      (begin
         ;; use supplied init.fasl from repository as starting point
         (print "Using init.fasl as the initial step.")
         (copy-file "fasl/init.fasl" "fasl/boot.fasl")))
   (let ((args (cdr args)))
      (if
         (if (string=? (car args) "-f")
            (fixed-point (cdr args) (put empty 'initial-fasl-size (file-size "fasl/boot.fasl")))
            (sys-system args))
         0 1)))
