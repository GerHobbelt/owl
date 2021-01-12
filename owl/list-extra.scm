#| doc
Extra List Functions

These common list processing operations require numbers, so they are
defined in a library loaded after math code.
|#

(define-library (owl list-extra)

   ;; A note on naming: There are del, get, set and ref functions for
   ;; various data structures, where the prefix (or lack of it) denotes
   ;; the type of data structure being operated on. L is the prefix for
   ;; regular list operations. Lack of generic functions for these
   ;; similar operations dispatching by the type of the data structure
   ;; is a feature.

   (export
      lset ldel lget length
      led ledn lins lref
      take drop iota
      list-ref
      list-tail
      make-list
      split-at
      has? hasq? ;; <- here temporarily
      )

   (import
      (owl core)
      (owl math integer)
      (owl list)
      (owl proof)
      (owl equal-prim)
      (owl syscall))

   (begin

      (define (list-ref lst pos)
         (cond
            ((null? lst) (error "list-ref: out of list" pos))
            ((eq? pos 0) (car lst))
            (else (list-ref (cdr lst) (- pos 1)))))

      (define lref list-ref)

      (define (lget lst pos def)
         (cond
            ((null? lst) def)
            ((eq? pos 0) (car lst))
            (else
               (lget (cdr lst) (- pos 1) def))))

      (define (lset lst pos val)
         (cond
            ((null? lst) (error "list-set: out of list setting " val))
            ((eq? pos 0) (cons val (cdr lst)))
            (else
               (cons (car lst)
                  (lset (cdr lst) (- pos 1) val)))))

      (define (ldel lst pos)
         (cond
            ((null? lst) (error "list-del: out of list, left " pos))
            ((eq? pos 0) (cdr lst))
            (else (cons (car lst) (ldel (cdr lst) (- pos 1))))))

      ;; list edit node - apply op to list (not element) at pos pos, allowing last null
      (define (ledn lst pos op)
         (cond
            ((eq? pos 0) (op lst))
            ((null? lst) (error "ledn: out of list, remaining " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (ledn tl (- pos 1) op))))))

      ;; list edit - apply op to value at given pos
      (define (led lst pos op)
         (cond
            ((null? lst) (error "led: out of list, remaining " pos))
            ((eq? pos 0) (cons (op (car lst)) (cdr lst)))
            (else
               (lets ((hd tl lst))
                  (cons hd (led tl (- pos 1) op))))))

      ;; insert value to list at given position
      (define (lins lst pos val)
         (cond
            ((eq? pos 0) (cons val lst))
            ((null? lst) (error "lins: out of list, left " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (lins tl (- pos 1) val))))))

      (example
         (lref '(a b c) 1) = 'b
         (lset '(a b c) 1 'x) = '(a x c)
         (ldel '(a b c) 1) = '(a c)
         (led '(1 2 3) 1 (C * 10)) = '(1 20 3)
         (ledn '(1 2 3) 1 (H cons 'x)) = '(1 x 2 3)
         (lins '(a b c) 1 'x) = '(a x b c))

      (define (length lst)
         (fold (λ (n v) (+ n 1)) 0 lst))

      ; take at n (or less) elemts from list l

      (define (take l n)
         (cond
            ((eq? n 0) #n)
            ((null? l) l)
            (else (cons (car l) (take (cdr l) (- n 1))))))

      (define (drop l n)
         (cond
            ((eq? n 0) l)
            ((null? l) l)
            (else (drop (cdr l) (- n 1)))))

      (example
         (length '(a b c)) = 3
         (take '(a b c) 2) = '(a b)
         (take '(a) 100) = '(a)
         (drop '(a b c) 2) = '(c)
         (drop '(a) 100) = '())

      (define (iota from step to)
         (unfold
            (lambda (pos)
               (values (+ pos step) pos))
            from
            (if (< from to)
               (lambda (x) (>= x to))
               (lambda (x) (<= x to)))))

      (example
         (iota 0 1 5) = '(0 1 2 3 4)
         (iota 10 -2 0) = '(10 8 6 4 2))

      (define (list-tail lst n)
         (if (eq? n 0)
            lst
            (list-tail (cdr lst) (- n 1))))

      (define (make-list n thing)
         (let loop ((n n) (out #n))
            (if (eq? n 0)
               out
               (loop (- n 1) (cons thing out)))))

      ;; lst n -> head tail, SRFI-1
      (define (split-at l n)
         (let loop ((l l) (o #n) (n n))
            (cond
               ((null? l)
                  (values (reverse o) l))
               ((eq? n 0)
                  (values (reverse o) l))
               (else
                  (loop (cdr l) (cons (car l) o) (- n 1))))))

      (example
         (list-tail '(a b c) 1) = '(b c)
         (make-list 3 'x) = '(x x x)
         (split-at '(a b c d) 2) = (values '(a b) '(c d)))

      ;; temporary versions
      (define (has? lst elem)
         (cond
            ((null? lst) #f)
            ((simple-equal? (car lst) elem)
               #t)
            (else
               (has? (cdr lst) elem))))

      (define (hasq? lst elem)
         (cond
            ((null? lst) #f)
            ((eq? (car lst) elem)
               #t)
            (else
               (hasq? (cdr lst) elem))))))
