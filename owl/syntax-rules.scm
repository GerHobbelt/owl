
(define-library (owl syntax-rules)

   (import
      (owl core)
      (owl list)
      (owl lcd ff)
      (owl equal)
      (owl symbol)
      (owl gensym)
      (owl io) ;; temp
      (owl proof))

   (export
      ;define-syntax         ;; macro to construct hygienic syntax-rules macro expander definitions
      ;define-syntax-ng      ;; same as above. for use during migration.
      define-syntax-transformer ;; the actual transformer to be included in an environment
      syntax-transformer)   ;; function to construct the macro expanders generated via define-syntax

   (begin
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
         match))

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
         (let ((res (match->list (match pat exp empty))))
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

            ;; pat match -> exp match', possibly filling in gensyms
            (define (rewrite pat match gen)
               (cond
                  ((ellipsis? pat)
                     (lets
                        ((these matchp gen (rewrite-ellipsis (car pat) match gen))
                         (tail  match  gen (rewrite (cddr pat) match gen)))
                        (values
                           (append these tail)
                           match gen)))
                  ((pair? pat)
                     (lets
                        ((hd match gen (rewrite (car pat) match gen))
                         (tl match gen (rewrite (cdr pat) match gen)))
                        (values (cons hd tl) match gen)))
                  ((symbol? pat)
                     (let ((val (get match pat #f)))
                        (cond
                           ((not val)
                              (values
                                 gen
                                 (put match pat gen)
                                 (gensym gen)))
                           ((eq? val literal)
                              (values pat match gen))
                           (else
                              (values val match gen)))))
                  (else
                     (values pat match gen))))

            (define (rewrite-ellipsis pat env gen)
               (let loop ((opts (get env ellipsis null)))
                  (if (null? opts)
                     (values null env gen)
                     (lets
                        ((this matchp gen (rewrite pat (car opts) gen))
                         (rest matchp gen (loop (cdr opts))))
                        (values (cons this rest) matchp gen)))))

            (lambda (exp env gensym)
               (lets ((exp env gensym (rewrite exp env gensym)))
                  (tuple exp gensym)))))

      (define (rewrite-t exp env)
         (let ((resp (rewrite exp env (gensym exp))))
            (ref resp 1)))

      (example
         ;; simple
         (rewrite-t '(a (b c)) (match '((a b) c) '((11 22) 33) empty)) = '(11 (22 33))
         (rewrite-t '(a . b) (match '(a b a b) '(11 22 11 22) empty)) = '(11 . 22)

         ;; ellipsis
         (rewrite-t '(a ...) (match '(a ...) '(11 22 33 44) empty)) = '(11 22 33 44)
         (rewrite-t '((b a) ...) (match '((a b) ...) '((1 2) (3 4) (5 6)) empty)) = '((2 1) (4 3) (6 5))
         (rewrite-t '(a ...) (match '((a b ...) ...) '((1 2 3) (10 20 30)) empty)) = '(1 10)
         (rewrite-t '((a a b ...) ... ) (match '((a b ...) ...) '((1 2 3) (10 20 30)) empty)) = '((1 1 2 3) (10 10 20 30))

         ;; gensyms
         (rewrite-t '(a x b x c) (match '(a b c) '(11 22 33) empty)) = '(11 g1 22 g1 33)
         (rewrite-t '(a x b y c) (match '(a b c) '(11 22 33) empty)) = '(11 g1 22 g2 33)

         ;; gensym ellipsis
         ; (rewrite-t '((x a x) ...) (match '(a ...) '(11 22 33) empty)) = '((1 g1) (2 g2) (3 g3))
         )

      (define (syntax-transformer literals rules env)
         ;(print "creating transformer:")
         ;(print " - literals " literals)
         ;(print " - rules: " rules)
         ;(print " - env " env)
         (let
            ((g (gensym (list rules literals)))
             (e (fold (lambda (env x) (put env x literal)) empty literals))) ;; temporary
            (lambda (exp free)
               ;(print "transforming " exp)
               (let ((g (gensym (list free g))))
                  (let loop ((rules rules))
                     (if (null? rules)
                        #false
                        (let ((match-env (match (caar rules) exp e))) ;; temp env
                           ;(print (car rules) " -> " match-env)
                           (if (eq? match-env failure)
                              (loop (cdr rules))
                              (begin
                                 ;(print "will rewrite " (car (cdar rules)) " in ")
                                 ;(for-each print (match->list match-env))
                                 (rewrite (car (cdar rules)) match-env g))))))))))


      ;; value (= transformer) of define-syntax -macro, which is used to define other macros
      (define define-syntax-transformer
         (syntax-transformer
            '(quote lambda define-syntax define-syntax-ng syntax-rules _define-macro syntax-transformer *toplevel*)
            '(
               ((_ keyword (syntax-rules literals (pattern template) ...))
                  (_define-macro keyword
                     (syntax-transformer
                        (quote (quote lambda keyword . literals))  ;; <- implicit
                        (quote ((pattern template) ...))
                        *toplevel*
                        ; empty
                        )))
            )
            ; *tabula-rasa*
            *toplevel*
            ))

      ;(_define-macro define-syntax     define-syntax-transformer)
      ;(_define-macro define-syntax-ng  define-syntax-transformer) ;; during migration

))

