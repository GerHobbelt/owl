#| doc
Fixed Point Computation

Implementing recursion becomes an interesting problem when mutation is not allowed.
Scheme allows recursion in definitions and via the letrec primitive form, which
under the hood require mutable data structures. We don't have them here, and the
only way to construct bindings and functions is a run of the mill lambda.

Luckily, a lambda is all it takes to recurse. This library takes a version of the
program in which one can use a recursive lambda, or rlambda, and converts it to
a corresponding expression using only normal lambdas.

The conversion is a custom one developed for the first version of Owl. It operates by
exteding each recursive lambda with the other recursive functions it may
call, propagating the required values to required call sites at required places,
and constructing wrapper functions which convert the originally intended arguments
to ones the actual recursive operations need.

The added extra arguments are added after the original ones. This is important,
because we want the wrapper functions to just consist of a few loads to free
registers from their environment and a jump.

|#

;; todo: vararg lambdas cannot get self as last parameter!

(define-library (owl eval fixedpoint)

   (export fix-points)

   (import
      (owl core)
      (owl eval ast)
      (owl math)
      (owl list)
      (owl equal)
      (owl list-extra)
      (only (owl syscall) error)
      (owl io)
      (owl eval env)
      (owl eval data))

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
               ((lambda-var fixed? formals body)
                  (walk body (union formals bound) found))
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
            (eq? (ref exp 1) 'lambda-var)) ;; new

      (define (fixed-lambda? exp env)
         (and
            (eq? (ref exp 1) 'lambda-var)
            (ref exp 2))) ;; fixed?

      (define (set-deps node deps) (set node 3 deps))
      (define deps-of (C ref 3))
      (define value-of (C ref 2))
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
                        (fixed-lambda? (value-of node) env)
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
                        (filter
                           (λ (node)
                              (lambda? (value-of node) env))
                           deps))))
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
            (tuple 'lambda-var #t names body)
            ; (mklambda names body)
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
               ((lambda-var fixed? formals body)
                  (if fixed?
                     (if (memq name formals)
                        exp
                        (tuple 'lambda-var #t formals (walk body)))
                     (error "carry-simple-recursion: variable lambda " exp)))
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
                        (tuple 'lambda-var #t (reverse (cdr (reverse deps))) (tuple 'call exp (map mkvar deps))))
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
            ((lambda-var fixed? formals body)
               (tuple
                  'lambda-var fixed?
                  formals
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
                           (tuple
                              'lambda-var #t formals
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


      (define (lambda-formals exp)
         (tuple-case exp
            ((lambda-var fixed? formals body) formals)))

      (define (fixed-lambda-formals exp)
         (tuple-case exp
            ((lambda-var fixed? formals body)
               (if fixed? formals
                  (error "variable arity in recursive function: " formals)))))

      (define (lambda-body exp)
         (tuple-case exp
            ((lambda-var fixed? formals body) body)))

      (define (handle-recursion nodes env)
         ; convert the lambda and carry bindings in the body
         (map
            (λ (node)
               (lets
                  ((lexp (value-of node))
                   (formals (lambda-formals lexp))
                   (body (lambda-body lexp)))
                  (tuple 'lambda-var #t                   ;; warning, was mklambda earlier, so #t implicit
                     (append formals (deps-of node))
                     (carry-bindings body env))))
            nodes))

      (define (make-wrapper node)
         (lets
            ((name (name-of node))
             (lexp (value-of node))
             (deps (deps-of node))
             (formals (lambda-formals lexp))
             (body (lambda-body lexp)))
            (tuple 'lambda-var #t formals
               (mkcall
                  (mkvar name)
                  (map mkvar
                     (append formals deps))))))

      ; bind all things from deps using possibly several nested head lambda calls


      (define (generate-bindings deps body env)
         (define second (C ref 2))
         (define first (C ref 1))
         ;(print "generate-bindings " deps)
         (if (null? deps)
            body
            (tuple-case (pick-binding deps env)

               ; no dependecies, so bind with ((lambda (a ...) X) A ...)
               ((trivial nodes)
                  ;(print " -> trivial " nodes)
                  (make-bindings (map first nodes) (map second nodes)
                     (generate-bindings
                        (remove-deps (map first nodes) deps)
                        body env)))

               ; bind one or more functions, which only call themselves recursively
               ((simple nodes)
                  ;(print " -> simple " nodes)
                  (let
                     ((env-rec
                        (fold
                           (λ (env node)
                              (let ((formals (lambda-formals (value-of node))))
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
                                       (append (lambda-formals val) deps)))) ; add self to args
                              body nodes)
                           ))))

               ((mutual nodes)
                  ;;; variable order must be preserved across functions
                  ;(print "mutual -> " nodes)
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
                              (let ((formals (fixed-lambda-formals (value-of node))))
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
            ;(print " - grow current " current)
            (lets
               ((related
                  (filter (λ (x) (memq (name-of x) current)) deps))
                (new-deps
                  (fold union current
                     (map third related))))
               ;(print " -> new currnet " new-deps)
               (if (= (length current) (length new-deps))
                  current
                  (grow new-deps deps))))

         ;(print "dependency-closure " deps)
         (map
            (λ (node)
               ;(print " - of node " node)
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

         ;(print "compile-rlambda " names)
         (generate-bindings
            (dependency-closure dependencies)
            body env))



      ; walk the term and translate all rlambdas to normal lambdas
      (define (unletrec exp env)
         (define (unletrec-list exps)
            (map (C unletrec env) exps))
         ; (print "unletrec " exp)
         (tuple-case exp
            ((var value) exp)
            ((call rator rands)
               (tuple 'call
                  (unletrec rator env)
                  (unletrec-list rands)))
            ((lambda-var fixed? formals body)
               (tuple 'lambda-var fixed? formals
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
            (else
               (error "Funny AST node in unletrec: " exp))))

      ;; exp env -> #(ok exp' env)
      (define (fix-points exp env)
         ; (print "fixed point " exp)
         (let ((result (unletrec exp env)))
            ; (print "fixed point done " result)
            (ok result env)))
))
