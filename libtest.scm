
(import 
   (only (owl sys) peek-byte))

(define (read-memory ptr len)
   (if (eq? len 0)
      #null
      (cons (peek-byte ptr)
         (read-memory (+ ptr 1) (- len 1)))))

(define (echo nth)
   (λ (arg)
      (lets ((ptr len max seed arg))
         (values
            (read-memory ptr len)
            (echo (+ nth 1))))))

(echo 0)
