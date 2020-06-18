
(import (owl proof))

;; unique keys
(define literal "lit")
(define failure "fail")
(define ellipsis "...")

;; (X ... . ?)
(define (ellipsis? exp)
   (and
      (pair? exp)
      (pair? (cdr exp))
      (eq? (cadr exp) '...)))

(define match
   (begin

      (define (match-ellipsis pat exps env)
         (let loop ((exps exps) (out null))
            (cond
               ((null? exps)
                  (values exps (put env ellipsis (reverse out))))
               ((match pat (car exps) env) =>
                  (lambda (envp)
                     (loop (cdr exps) (cons envp out))))
               (else
                  (values exps (put env ellipsis (reverse out)))))))

      (define (match pat exp env)
         (cond
            ((eq? env failure)
               env)
            ((pair? pat)
               (cond
                  ((ellipsis? pat)
                     (if (list? exp)
                        (lets ((exps env (match-ellipsis (car pat) exp env)))
                           (match (cddr pat) exps env))
                        failure))
                  ((pair? exp)
                     (match (car pat) (car exp)
                        (match (cdr pat) (cdr exp) env)))
                  (else failure)))
            ((symbol? pat)
               (let ((val (get env pat #f)))
                  (cond
                     ((eq? val literal) ;; allow ... and _ as literals
                        (if (eq? pat exp)
                           env
                           failure))
                     ((eq? pat '_)
                        env)
                     ((not val)
                        (put env pat exp))
                     ((eq? val exp)
                        env)
                     (else failure))))
            ((equal? pat exp)
               env)
            (else
               failure)))
      (lambda (pat exp)
         (match pat exp empty))))

(define (match->list match)
   (if (eq? match failure)
      failure
      (ff-foldr
         (lambda (out k v)
            (if (eq? k ellipsis)
               (let ((matches (map match->list v)))
                  (cons (cons 'ellipsis matches) out))
               (cons (cons k v) out)))
         null
         match)))

(define (match-al pat exp)
   (let ((res (match->list (match pat exp))))
      ;(print pat " + " exp " = " res)
      res))

(example
   (match-al '(_ a b) '(1 2 3)) = '((a . 2) (b . 3))
   (match-al '(_ a b) '(1 2 3)) = '((a . 2) (b . 3))
   (match-al '(a b ...) '(10 1 2 3)) = '((a . 10) (ellipsis ((b . 1)) ((b . 2)) ((b . 3))))
   (match-al '(x (a b) ...) '(10 (1 10) (2 20) (3 30))) = '((x . 10) (ellipsis ((a . 1) (b . 10)) ((a . 2) (b . 20)) ((a . 3) (b . 30))))
   (match-al '(x (a b ...) ...) '(X (A 1 2 3) (B 10 20 30))) = '((x . X) (ellipsis ((a . A) (ellipsis ((b . 1)) ((b . 2)) ((b . 3)))) ((a . B) (ellipsis ((b . 10)) ((b . 20)) ((b . 30))))))
   )

(define symbols-of

   (define (walk exp found)
      (cond
         ((pair? exp)
            (walk (cdr exp)
               (walk (car exp) found)))
         ((and (symbol? exp) (not (memq exp found)))
            (cons exp found))
         (else found)))

   (C walk #n))


(define rewrite
   (begin

      (define (rewrite pat match)
         (cond
            ((ellipsis? pat)
               (rewrite-ellipsis (car pat) match
                  (lambda (tail)
                     (append
                        (rewrite (cddr pat) match)
                        tail))))
            ((pair? pat)
               (cons
                  (rewrite (car pat) match)
                  (rewrite (cdr pat) match)))
            ((symbol? pat)
               (let ((val (get match pat #f)))
                  (cond
                     ((not val)
                        'gensym)
                     ((eq? val literal)
                        val)
                     (else val))))
            (else
               pat)))

      (define (rewrite-ellipsis pat env cont)
         (cont
            (map
               (lambda (env)
                  (rewrite pat env))
               (get env ellipsis null))))

      rewrite))


(example
   (rewrite '(a (b c)) (match '((a b) c) '((11 22) 33))) = '(11 (22 33))
   (rewrite '(a . b) (match '(a b a b) '(11 22 11 22))) = '(11 . 22)
   (rewrite '(a ...) (match '(a ...) '(11 22 33 44))) = '(11 22 33 44)
   (rewrite '((b a) ...) (match '((a b) ...) '((1 2) (3 4) (5 6)))) = '((2 1) (4 3) (6 5))
   (rewrite '(a ...) (match '((a b ...) ...) '((1 2 3) (10 20 30)))) = '(1 10)
   (rewrite '((a a b ...) ... ) (match '((a b ...) ...) '((1 2 3) (10 20 30)))) = '((1 1 2 3) (10 10 20 30))
   )















