
(define-library (owl syntax-rules)

   (import
      (owl core)
      (owl list)
      (owl lcd ff)
      (owl equal)
      (owl symbol)
      (owl gensym)
      (owl list-extra)
      (owl math)
      (only (owl render) str)       ;; temp
      (only (owl syscall) error)
      (owl io) ;; temp
      (owl proof))

   (export 
      define-syntax-ng)
      
   (begin

      (define empty-env 
         (put empty 'syntax 'env))
      
      ;;;
      ;;; UTILS
      ;;;
      
      (define implicit-literals
         '(lambda quote))
      
      
      ;; env = ff of symbol ->
      ;;          (literal . value)
      ;;          (bound . value)
      ;;          ([depth] . values)
      
      ;; to be sum values later
      (define (literal? binding)
         (and (pair? binding) (eq? (car binding) 'literal)))
      
      (define (bound? binding)
         (and (pair? binding) (eq? (car binding) 'bound)))
      
      (define (ellipsis? binding)
         (and (pair? binding) 
         (number? (car binding))))
      
      (define (env-bind env key val)
         (put env key (cons 'bound val)))
      
      (define (ellipsis-pattern? exp)
         (and (pair? exp)
            (pair? (cdr exp))
            (eq? (cadr exp) '...)))
      
      (define gensym-key (list 'gensym))
      
      (define get-gensym
         ;; special key under which a fresh gensym is kept
         (lambda (env)
            (let ((val (get env gensym-key)))
               (if val
                  (values
                     (put env gensym-key (gensym val))
                     val)
                  (let ((first (gensym 'x)))
                     (values
                        (put env gensym-key (gensym first))
                        first))))))
      
      
      
      ;;;
      ;;; MATCHING: pattern + expression -> environment
      ;;;
      
      ;; start a new list at given depth constructing the path to it if necessary
      (define (new-ellipsis lst depth)
         (cond
            ((eq? depth 1)
               (append lst '(())))
            ((null? lst)
               (new-ellipsis '(()) depth))
            (else
               (led lst
                  (- (length lst) 1)
                  (lambda (elem)
                     (new-ellipsis elem (- depth 1)))))))
      
      (define (push-ellipsis lst depth val)
         (cond
            ((eq? depth 1)
               (append lst (list val)))
            ((null? lst)
               (error "bug: push-ellipsis null at " depth))
            (else
               (led lst (- (length lst) 1)
                  (lambda (elem)
                     (push-ellipsis elem (- depth 1) val))))))
            
      (example
         (new-ellipsis '() 1) = '(())
         (new-ellipsis '(a) 1) = '(a ())
         (new-ellipsis '(a (b c)) 2) = '(a (b c ()))
         (push-ellipsis '(a b c) 1 'd) = '(a b c d)
         (push-ellipsis '((a b) (c d)) 2 'e) = '((a b) (c d e))  
         )
      
      (define (env-store env key ellipsis-depth val)
         (let ((stored (get env key #f)))
            (cond
               ((not stored)
                  (if (eq? ellipsis-depth 0)
                     (env-bind env key val)
                     (error "env-store: depth " ellipsis-depth))) 
               ((ellipsis? stored)
                  (if (= ellipsis-depth (car stored)) ;; matching ellipsis
                     (put env key
                        (cons ellipsis-depth
                           (push-ellipsis (cdr stored) ellipsis-depth val)))
                     (error "mismatching ellipsis depth" ellipsis-depth)))
               (else
                  (error "env-store: not an ellipsis: " val)))))
      
      (define (symbols-of exp)
         (cond
            ((pair? exp)
               (union (symbols-of (car exp))
                      (symbols-of (cdr exp))))
            ((symbol? exp)
               (list exp))
            (else null)))
      
      ;; TEMPORARY
      (define (syntax-variables exp env)
         (diff 
            (symbols-of exp)
            '(... quote lambda)))      
      
      ;; -> env | #f (cannot be empty env)
      (define (new-ellipsis-variables env vars depth)
         (fold
            (lambda (env var)
               (let ((val (get env var)))
                  (cond
                     ((not env)
                        #f)
                     ((not val)
                        ; (put env var (cons depth (new-ellipsis null depth)))
                        (put env var (list depth))
                        )
                     ((eq? depth (car val))
                        (put env var
                           (cons depth (new-ellipsis (cdr val) (- depth 1)))))
                     ((equal? (car val) (- depth 1))
                        ;; override with a deeper ellipsis. should also check emptiness.
                        (put env var
                           (cons depth (new-ellipsis (cdr val) (- depth 1)))))
                     ;(#t
                     ;   (put env var
                     ;      (cons (car val)
                     ;         (new-ellipsis (cdr val) (- depth 1)))))
                     (else
                        (print "new-ellipsis-vars: failed to make " var " at depth " depth " vs " val)
                        #f))))
             env vars))
                 
      (define (match exp pattern env depth ok fail)
         (cond
            ((ellipsis-pattern? pattern)
               (if (list? exp)
                  (lets ((vars (syntax-variables (car pattern) env))
                         (env (new-ellipsis-variables env vars (+ depth 1))))
                     (if env
                        (let loop ((env env) (exp exp) (fail fail))
                           (if (null? exp)
                              (match exp (cddr pattern) env depth ok fail)
                              (let ((backtrack (lambda () (match exp (cddr pattern) env depth ok fail))))
                                 (match (car exp) (car pattern) env (+ depth 1)
                                    (lambda (env fail)
                                       (loop env (cdr exp) fail))
                                    backtrack))))
                        (fail)))
                  (fail)))
            ((pair? pattern)
               (if (pair? exp)
                  ;; car and cdr must match, or call fail
                  (match (car exp) (car pattern) env depth
                     (lambda (env fail)
                        ;; fail may backtrack to another interpretation of car, so use it
                        (match (cdr exp) (cdr pattern) env depth ok fail))
                     fail)
                  (fail)))
            ((symbol? pattern)
               (let ((val (get env pattern)))
                  (cond
                     ((not val)
                        (ok (env-store env pattern depth exp) fail))
                     ((bound? val)
                        (if (eq? exp (cdr val))
                           (ok env fail)
                           (fail)))
                     ((literal? val)
                        (if (eq? exp pattern)
                           (ok env fail)
                           (fail)))
                     ((and (pair? val) (equal? depth (car val)))
                        (ok (env-store env pattern depth exp) fail))
                     (else
                        (print "Unknown match to " val)
                        (fail)
                        ))))
            ((eq? pattern exp)
               (ok env fail))
            (else
               (fail))))
      
      
      
      
      
      ;;;
      ;;; REWRITING: pattern + environment -> term
      ;;; 
      
      ;; env = ff of key -> (depth . val), depth=0 = fixed value
      
      (define (pick-list lst pos ok fail)
         (cond
            ((null? lst) (fail))
            ((eq? pos 0) (ok (car lst)))
            (else
               (pick-list (cdr lst) (- pos 1) ok fail))))
      
      (define (pick-ellipsis-value env path ok fail)
         (cond
            ((null? env)
               (fail))
            ((null? (cdr path))
               (pick-list env (car path) ok fail))
            (else
               (pick-list env (car path)
                  (lambda (env)
                     (pick-ellipsis-value env (cdr path) ok fail))
                  fail))))
      
      ;; testing
      
      (define (test-ok env x) (list 'ok x))
      (define (test-fail) (list 'fail))
      
      (define (pick-ellipsis binding path ok fail)
         (lets ((depth (car binding))
                (env (cdr binding)))
            (if (= (length path) depth)
               (pick-ellipsis-value env path ok fail)
               (begin
                  ;; maybe allow later
                  (print "pick-ellipsis: mismatching depth: path " path ", depth " depth)
                  (fail)))))
      
      
      
      ;; template env path Ok Fail -> (ok env val) | (fail)
      ;; path = (offset ...)
      (define (rewrite pattern env path ok fail)
         ; (print "rewrite: pat " pattern ", env " env ", path " path)
         (cond
            ((ellipsis-pattern? pattern) ;; (a ... . rest )
               ;; extend path until rewrite fails
               (let loop ((nth 0) (loop-env env) (vals '()))
                  ; (print "Rewriting " nth " ellipsis " pattern ", gensym " (get env gensym-key))
                  (rewrite (car pattern) loop-env (append path (list nth))
                     (lambda (loop-env done)
                        ;; env comes back, ignored (holds gensyms), resume original,
                        ;; but copy gensym
                        ; (print "Pattern " (car pattern) " + path "  (append path (list nth)) " = " done)
                        (loop (+ nth 1)
                           ; loop-env
                           (put env gensym-key (get loop-env gensym-key))
                           (cons done vals))
                     )
                     (lambda ()
                        (rewrite (cddr pattern) env path
                           (lambda (env tail)
                              (ok
                                 (put env gensym-key (get loop-env gensym-key 'gensym)) ;; inherit only gensym
                                 (append (reverse vals) tail))
                                 )
                           fail)))))
            ((pair? pattern)
               (rewrite (car pattern) env path
                  (lambda (env hd)
                     (rewrite (cdr pattern) env path
                        (lambda (env tl)
                           (ok env (cons hd tl)))
                        fail))
                  fail))
            ((symbol? pattern)
               (let ((val (get env pattern)))
                  (cond
                     ((literal? val)
                        (ok env pattern))
                     ((ellipsis? val)
                        (pick-ellipsis val path
                           (lambda (val)
                              (ok env val))
                           fail))
                     ((bound? val)
                        (ok env (cdr val)))
                     ((not val) ;; unbound, gensym
                        (lets ((env val (get-gensym env)))
                           (ok
                              (env-bind env pattern val)
                              val)))
                     (else
                        (error "invalid environment node: " val)))))
            (else
               (ok env pattern))))
      
      (define (test-rewrite pat env)
         (rewrite pat (list->ff env)
            '()
            test-ok test-fail))
      
      (example
         (test-rewrite '(a b c) '((a bound . A) (b bound . B) (c literal)))
            = '(ok (A B c))
      
         (test-rewrite '(a ... b ...)
               '((a 1 a1 a2 a3)
                 (b 1 b1 b2 b3)))
             = '(ok (a1 a2 a3 b1 b2 b3))
      
         (test-rewrite '((a b) ...)
               '((a 1 a1 a2 a3)
                 (b 1 b1 b2 b3)))
             = '(ok ((a1 b1) (a2 b2) (a3 b3)))
      
         (test-rewrite '((a b ...) ...)
               '((a 1 a1 a2)
                 (b 2 (b1 b2 b3) (B1 B2 B3))))
             = '(ok ((a1 b1 b2 b3) (a2 B1 B2 B3)))
      
         (test-rewrite '(x y z) '())
             = '(ok (g1 g2 g3))
      
         (test-rewrite '(x (a x) ... x)
               '((a 1 a1 a2)))
             = '(ok (g1 (a1 g1) (a2 g1) g1))
      
         (test-rewrite '((a x) ...)
               '((a 1 a1 a2)))
             = '(ok ((a1 g1) (a2 g2)))
      
      )
      
      
            
      ;;; rewrite + match test
      
      (define (test-match exp pat lits rewrite-pat target)
         (lets ((fresh (gensym (list exp pat rewrite)))
                (env (fold (lambda (env x) (put env x (cons 'literal x))) empty-env lits))
                (outcome 
                  (match exp pat env 0
                     (lambda (env fail)
                        (rewrite rewrite-pat
                           (put env gensym-key fresh)
                           '()
                           test-ok
                           fail))
                     test-fail)))
            (if (equal? outcome (list 'ok target))
               42 ; (print "OK " exp " -> " target)
               (error "ERROR " (str exp " -> " outcome)))))
      
      
         (test-match
            '(1 2 3)
            '(a b ...)
            '()
            '(111 a b ... b ... 111)
            '(111 1 2 3 2 3 111)
            )
      
         (test-match
            '(11 (1 2 3) (10 20 30))
            '(x (a b ...) ...)
            '()
            '(x (a b ...) ...)
            '(11 (1 2 3) (10 20 30))
            )
      
         (test-match
            '(100 1 2 100 3 4)
            '(x a ... x b ...)
            '()
            '(a ... b ...)
            '(1 2 3 4))
      
         (test-match
            '(x 1 2 x 3 4)
            '(x a ... x b ...)
            '(x)
            '(a ... b ...)
            '(1 2 3 4))
      
            
      
      (define (make-transformer lits pats targets toplevel)
         (lets ((lits (append implicit-literals lits))
                (env (fold (lambda (env x) (put env x (cons 'literal x))) empty-env lits))
                (fresh (gensym (list lits pats targets)))) ;; macro compile time
            (lambda (form free)
               (let ((env (put env gensym-key (gensym (list fresh free)))))
                  (let loop ((pats pats) (targets targets))
                     (if (null? pats)
                        #f
                        (match form (car pats) env 0
                           (lambda (env fail)
                              (rewrite (car targets) env '()
                                 (lambda (env result)
                                    ; (print "TRANSFOMERS MADE: " result "\n" " -  out of " (car targets) "\n" " -  in " (ff->list env))
                                    (cons result (get env gensym-key)))
                                 fail))
                           (lambda ()
                              (loop (cdr pats) (cdr targets))))))))))

      ;; new repl macro style 
           
      (_define-macro xlet
         (lambda (form free)
            ;; -> #(form' free') | #f
            (cons 42 free)
            ))

      (example
         (xlet ((foo 100)) foo) = 42)
               
     
      ;; definer definition via macro api 
      (_define-macro define-syntax-ng
         (make-transformer
            '(define-syntax-ng syntax-rules
              _define-macro make-transformer *toplevel*)
            '(
               (define-syntax-ng name 
                  (syntax-rules literals 
                     (pattern template) 
                     ...))
             )
            '(
               (_define-macro name
                  (make-transformer
                     (quote literals)
                     (quote (pattern ...))
                     (quote (template ...))
                     *toplevel*))
            )
            empty))
      
      (define-syntax-ng xlet
         (syntax-rules ()
            ((xlet ((var val) ...) . body)
               ((lambda (var ...) . body) val ...))))
      
      (example
         (xlet ((a 1) (b 2)) (list a b)) = '(1 2))
      
      
      
      
      
      ;;; -------------------------------------------------------------------------------------------------------------------------------------
      
            (define-syntax-ng define-sum-type
               (syntax-rules (__repl_begin __options __walk __names define-syntax syntax-rules define)
                  ((define-sum-type name
                     __walk ()
                     __options ((body option . args) ...)
                     __names names)
                   (__repl_begin
                      (quote
                         ((define (option . args)
                               (lambda names (option . args)))
                          ...
                          (define-syntax name
                               (syntax-rules names
                                  ((name value ((option . args) . body) ...)
                                     (value
                                        (lambda args . body) ...))))))))
      
                  ((define-sum-type name __walk ((option . args) . rest) __options (o ...)  __names (n ...))
                     (define-sum-type name
                        __walk rest
                        __options (o ... (body option . args))
                        __names (n ... option)))
      
                  ((define-sum-type name . options)
                     (define-sum-type name __walk options __options () __names ()))))
      
      
      (define-sum-type lizt
         (nil)
         (kons a b))
      
      (define (kar x)
         (lizt x
            ((nil) 0)
            ((kons a b) a)))

      (example
         (kar (kons 11 22)) = 11)
               
            
      
      ))


