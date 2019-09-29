#| doc
Owl does not allow you to use a special toplevel or use mutations to implement recursion.
Lambdas are the only way to make variable bindings.
Up to this point the compiler also has `rlambda` functions, 
   which will end up generating recursive bindings.
They are essentially to `letrec` what `lambda` is to `let`.
This compilation step gets rid of all the rlambdas
   turning them the ones we all know and love.
This is done by constructing the fixed points manually.
|#

;; todo: vararg lambdas cannot get self as last parameter!

(define-library (owl fixedpoint)

   (export fix-points)

   (import
      (owl core)
      (owl ast)
      (owl math)
      (owl list)
      (owl equal)
      (owl list-extra)
      (only (owl syscall) error)
      (owl io)
      (owl env))

   (begin

      ; return the least score by pred
      (define (least pred lst)
         (if (null? lst)
            #false
            (cdr
               (fold
                  (λ (lead x)
                     (let ((this (pred x)))
                        (if (< this (car lead)) (cons this x) lead)))
                  (cons (pred (car lst)) (car lst)) (cdr lst)))))

      (define (free-vars exp env)

         (define (take sym found bound)
            (if (or (memq sym bound) (memq sym found))
               found
               (cons sym found)))

         (define (walk-list exp bound found)
            (fold
               (λ (found thing)
                  (walk thing bound found))
               found exp))

         (define (walk exp bound found)
            (tuple-case exp
               ((var exp)
                  (take exp found bound))
               ((lambda formals body)
                  (walk body (union formals bound) found))
               ((lambda-var fixed? formals body)
                  (walk body (union formals bound) found))
               ((case-lambda fn else)
                  (walk fn bound
                     (walk else bound found)))
               ((call rator rands)
                  (walk rator bound
                     (walk-list rands bound found)))
               ((value val) found)
               ((values vals)
                  (walk-list vals bound found))
               ((receive op fn)
                  (walk op bound
                     (walk fn bound found)))
               ((branch kind a b then else)
                  (walk-list (list a b then else) bound found))
               (else
                  (print "free-vars: unknown node type: " exp)
                  found)))

         (walk exp #n #n))

      (define (lambda? exp env)
         (eq? (ref exp 1) 'lambda))

      (define (set-deps node deps) (set node 3 deps))
      (define deps-of (C ref 3))
      (define name-of (C ref 1))

      ; pick a set of bindings for binding
      (define (pick-binding deps env)

         (define (maybe type vals)
            (if (null? vals) #false  (tuple type vals)))

         (or
            ; things which have no dependences
            (maybe 'trivial
               (filter (B null? deps-of) deps))

            ; things which only depend on themselvs (simply recursive)
            (maybe 'simple
               (filter
                  (λ (node)
                     (and
                        (lambda? (value-of node) env)
                        (equal? (deps-of node) (list (name-of node)))))
                  deps))

            ; since the dependencies have been inherited, the smallest
            ; set of dependencies is always shared by the things. therefore
            ; they form a partition ready for mutual recursive binding.

            (maybe 'mutual
               ; grab the first lambda node with least number of dependencies
               (let
                  ((node
                     (least
                        (B length deps-of)
                        (filter (λ (node) (lambda? (value-of node) env)) deps))))
                  (if node
                     (let ((partition (deps-of node)))
                        (filter
                           (λ (node) (memq (name-of node) partition))
                           deps))
                     #n)))

            (error "unable to resolve dependencies for mutual recursion. remaining bindings are " deps)))

      ;;; remove nodes and associated deps
      (define (remove-deps lost deps)
         (map
            (λ (node)
               (set-deps node
                  (diff (deps-of node) lost)))
            (remove
               (λ (node)
                  (memq (name-of node) lost))
               deps)))

      (define (make-bindings names values body)
         (mkcall
            (mklambda names body)
            values))

      (define (var-eq? node sym)
         (tuple-case node
            ((var s) (eq? s sym))
            (else #false)))

      ; convert all (name ..) to (name .. name), and make wrappers when name
      ; is used as a value

      (define (carry-simple-recursion exp name deps)
         (define (walk exp)
            (tuple-case exp
               ((call rator rands)
                  (if (var-eq? rator name)
                     (tuple 'call rator                     ; <- converted call
                        (append (map walk rands) (list rator)))
                     (tuple 'call
                        (walk rator)
                        (map walk rands))))
               ((lambda formals body)
                  (if (memq name formals)
                     exp
                     (tuple 'lambda formals (walk body))))
               ((branch kind a b then else)
                  (tuple 'branch kind (walk a) (walk b) (walk then) (walk else)))
               ((receive op fn)
                  (tuple 'receive (walk op) (walk fn)))
               ((values vals)
                  (tuple 'values (map walk vals)))
               ((value val) exp)
               ((var sym)
                  (if (eq? sym name)
                     (begin
                        ;(print " making a wrapper for " name)
                        ;(print "   - with deps " deps)
                        (tuple 'lambda (reverse (cdr (reverse deps))) (tuple 'call exp (map mkvar deps))))
                     exp))
               (else
                  (error "carry-simple-recursion: what is this node type: " exp))))
         (walk exp))


      (define (carry-bindings exp env)
         (tuple-case exp
            ((call rator rands)  ;;; have to emulate (call (var sym) rands) for now
               (tuple-case rator
                  ((var sym)
                     (tuple-case (lookup env sym)
                        ((recursive formals deps)
                           (if (not (= (length formals) (length rands)))
                              (error
                                 "Wrong number of arguments: "
                                 (list 'call exp 'expects formals)))
                           (mkcall rator
                              (append
                                 (map (C carry-bindings (env-bind env formals)) rands)
                                 (map mkvar deps))))
                        (else
                           (mkcall (carry-bindings rator env)
                              (map (C carry-bindings env) rands)))))
                  (else
                     (mkcall (carry-bindings rator env)
                        (map (C carry-bindings env) rands)))))
            ((lambda formals body)
               (mklambda formals
                  (carry-bindings body
                     (env-bind env formals))))
            ((branch kind a b then else)
               (let
                  ((a (carry-bindings a env))
                   (b (carry-bindings b env))
                   (then (carry-bindings then env))
                   (else (carry-bindings else env)))
                  (tuple 'branch kind a b then else)))
            ((receive op fn)
               (let
                  ((op (carry-bindings op env))
                   (fn (carry-bindings fn env)))
                  (tuple 'receive op fn)))
            ((var sym)
               (tuple-case (lookup env sym)
                  ((recursive formals deps)
                     (let
                        ((lexp
                           (mklambda formals
                              (mkcall exp (map mkvar (append formals deps))))))
                        ; (print "carry-bindings: made local closure " lexp)
                        lexp))
                  (else exp)))
            ((value val) exp)
            ((values vals)
               (tuple 'values
                  (map (C carry-bindings env) vals)))
            (else
               (error "carry-bindings: strage expression: " exp))))

      ;;; ((name (λ (formals) body) deps) ...) env
      ;;; -> ((lambda (formals+deps) body') ...)

      (define (handle-recursion nodes env)
         ; convert the lambda and carry bindings in the body
         (map
            (λ (node)
               (lets
                  ((lexp (value-of node))
                   (formals (ref lexp 2))
                   (body (ref lexp 3)))
                  (mklambda
                     (append formals (deps-of node))
                     (carry-bindings body env))))
            nodes))

      (define (make-wrapper node)
         (lets
            ((name (name-of node))
             (lexp (value-of node))
             (deps (deps-of node))
             (formals (ref lexp 2))
             (body (ref lexp 3)))
            (mklambda formals
               (mkcall
                  (mkvar name)
                  (map mkvar
                     (append formals deps))))))

      ; bind all things from deps using possibly several nested head lambda calls

      (define (generate-bindings deps body env)
         (define second (C ref 2))
         (define first (C ref 1))
         (if (null? deps)
            body
            (tuple-case (pick-binding deps env)

               ; no dependecies, so bind with ((lambda (a ...) X) A ...)
               ((trivial nodes)
                  (make-bindings (map first nodes) (map second nodes)
                     (generate-bindings
                        (remove-deps (map first nodes) deps)
                        body env)))

               ; bind one or more functions which are simply recursive
               ((simple nodes)
                  (let
                     ((env-rec
                        (fold
                           (λ (env node)
                              (let ((formals (ref (value-of node) 2)))
                                 (env-put-raw env
                                    (name-of node)
                                    (tuple 'recursive formals
                                       (list (name-of node))))))
                           env nodes)))
                     ; bind all names to extended functions (think (let ((fakt (lambda (fakt x fakt) ...) ...))))
                     (make-bindings
                        (map first nodes)
                        (handle-recursion nodes env-rec)
                        ; then in the body bind them to (let ((fakt (lambda (x) (fakt x fakt))) ...) ...)
                        (let
                           ((body
                              (generate-bindings
                                 (remove-deps (map first nodes) deps)
                                 body
                                 (env-bind env (map first nodes)))))
                           ; one option is to bind all to wrapper functions. we'll try another alternative now
                           ; and convert all uses to use the extended functions instead, since they all just
                           ; require passing the same value as the operator as an arument and thus are quaranteed
                           ; not to grow the closures (unlike mutual recursive functions)
                           ; plan A, (original) make the function look like the original
                           ;(make-bindings
                           ;   (map first nodes)
                           ;   (map make-wrapper nodes)
                           ;   body)
                           ; plan B, convert to direct calls to the wrapper
                           ; remember, the change is just to append the function name to the call
                           ; and make a (lambda (v ...) (self v .. self)) if it is used as a value
                           (fold
                              (λ (body node)
                                 (lets ((name val deps node))
                                    (carry-simple-recursion body name
                                       (append (ref val 2) deps)))) ; add self to args
                              body nodes)
                           ))))

               ((mutual nodes)
                  ;;; variable order must be preserved across functions
                  (lets
                     ((partition (deps-of (car nodes)))
                      (nodes
                        (map
                           (λ (node)
                              (if (null? (diff (deps-of node) partition))
                                 (set-deps node partition)
                                 (error
                                    "mutual recursion bug, partitions differ: "
                                    (list 'picked partition 'found node))))
                           nodes))
                      (env-rec
                        (fold
                           (λ (env node)
                              (let ((formals (ref (value-of node) 2)))
                                 (env-put-raw env
                                    (name-of node)
                                    (tuple 'recursive formals partition))))
                           env nodes)))
                     (make-bindings
                        (map first nodes)
                        (handle-recursion nodes env-rec)
                        (make-bindings
                           (map first nodes)
                           (map make-wrapper nodes)
                           (generate-bindings
                              (remove-deps (map first nodes) deps)
                              body
                              (env-bind env (map first nodes)))))))

               (else
                  (error "generate-bindings: cannot bind anything from " deps)))))


      (define (dependency-closure deps)

         (define third (C ref 3))
         (define (grow current deps)
            (lets
               ((related
                  (filter (λ (x) (memq (name-of x) current)) deps))
                (new-deps
                  (fold union current
                     (map third related))))
               (if (= (length current) (length new-deps))
                  current
                  (grow new-deps deps))))

         (map
            (λ (node)
               (set-deps node
                  (grow (deps-of node) deps)))
            deps))

      (define (compile-rlambda names values body env)

         ;; -> (name value-exp dependencies)
         (define dependencies
            (zip
               (λ (name value)
                  (tuple name value
                     (intersect names
                        (free-vars value env))))
               names values))

         (generate-bindings
            (dependency-closure dependencies)
            body env))

      ; walk the term and translate all rlambdas to normal lambdas

      (define (unletrec exp env)
         (define (unletrec-list exps)
            (map (C unletrec env) exps))
         (tuple-case exp
            ((var value) exp)
            ((call rator rands)
               (tuple 'call
                  (unletrec rator env)
                  (unletrec-list rands)))
            ((lambda formals body)
               (mklambda formals
                  (unletrec body (env-bind env formals))))
            ((lambda-var fixed? formals body)
               (mkvarlambda formals
                  (unletrec body (env-bind env formals))))
            ((rlambda names values body)
               (lets
                  ((env (env-bind env names))
                   (handle (C unletrec env)))
                  (compile-rlambda names (map handle values) (handle body) env)))
            ((receive op fn)
               (tuple 'receive (unletrec op env) (unletrec fn env)))
            ((value val) exp)
            ((values vals)
               (tuple 'values
                  (unletrec-list vals)))
            ((branch kind a b then else)
               (let
                  ((a (unletrec a env))
                   (b (unletrec b env))
                   (then (unletrec then env))
                   (else (unletrec else env)))
                  (tuple 'branch kind a b then else)))
            ((case-lambda func else)
               (tuple 'case-lambda
                  (unletrec func env)
                  (unletrec else env)))
            (else
               (error "Funny AST node in unletrec: " exp))))

      ;; exp env -> #(ok exp' env)
      (define (fix-points exp env)
         (let ((result (unletrec exp env)))
            (tuple 'ok result env)))
))
