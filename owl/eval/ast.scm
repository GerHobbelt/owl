#| doc
AST transformation

S-expressions could be used internally by the compiler at each step. Here we however
construct a simple tree of tuples format out of the S-expression while checking its
structure. This is done so that the subsequent compiler steps don't have to check
S-expression structure.

The AST format could switch to using sum types at some point.
|#

(define-library (owl eval ast)

   (export call? var? value-of sexp->ast mkcall mklambda mkvarlambda mkvar mkval)

   (import
      (owl list-extra)
      (owl math)
      (owl tuple)
      (owl list)
      (owl symbol)
      (owl core)
      (owl equal)
      (owl eval env)
      (owl eval data)
      (scheme cxr))

   (begin

      (define (call? thing) (eq? (ref thing 1) 'call))
      (define (var? thing) (eq? (ref thing 1) 'var))
      (define value-of (C ref 2))

      (define (mkval val)
         (tuple 'value val))

      (define (mklambda formals body)
         (tuple 'lambda-var #t formals body))

      ;; formals is a list as usual, but last one will be bound to an arg list
      ;; having an extra var? field because the fixed one could be merged to this later
      (define (mkvarlambda formals body)
         (tuple 'lambda-var #false formals body))

      (define (mkcall rator rands)
         (tuple 'call rator rands))

      ;;; cps adds a cont + system
      (define (mkprim op args)
         (tuple 'prim op args))

      (define (mkvar sym)
         (tuple 'var sym))

      ;; formals-sexp → (sym ..)|#false fixed-arity?
      (define (check-formals lst)
         (let loop ((lst lst) (out #n))
            (cond
               ((null? lst)
                  (values (reverse out) #true))
               ((symbol? lst) ;; variable arity
                  (if (memq lst out) ;; reappearence
                     (values #f #f)
                     (values (reverse (cons lst out)) #false)))
               ((symbol? (car lst))
                  (if (memq (car lst) out)
                     (values #f #f)
                     (loop (cdr lst) (cons (car lst) out))))
               (else
                  (values #f #f)))))

      (define (fixed-formals-ok? sexp)
         (lets ((formals fixed? (check-formals sexp)))
            (and formals fixed?)))

      (define (translate-direct-call exp env fail translate)
         (tuple-case (lookup env (car exp))
            ((special thing)
               (case thing
                  ((quote)
                     (if (= (length exp) 2)
                        (mkval (cadr exp))
                        (list "Strange quote: " exp)))
                  ((lambda)
                     (let ((len (length exp)))
                        (cond
                           ((= len 3)
                              (lets
                                 ((formals (cadr exp))
                                  (body (caddr exp))
                                  (formals fixed?
                                    (check-formals formals)))
                                 (cond
                                    ((not formals) ;; non-symbols, duplicate variables, etc
                                       (fail (list "Bad lambda: " exp)))
                                    (fixed?
                                       (mklambda formals
                                          (translate body (env-bind env formals) fail)))
                                    (else
                                       (mkvarlambda formals
                                          (translate body (env-bind env formals) fail))))))
                           (else
                              (fail (list "Bad lambda: " exp))))))
                  ((rlambda) ;;; (rlambda formals definitions body)
                     (if (= (length exp) 4)
                        (let
                           ((formals (list-ref exp 1))
                            (values (list-ref exp 2))
                            (body (list-ref exp 3)))
                           (if
                              (and
                                 (list? values)
                                 (fixed-formals-ok? formals)
                                 (= (length formals) (length values)))
                              (let ((env (env-bind env formals)))
                                 (tuple 'rlambda formals
                                    (map
                                       (λ (x) (translate x env fail))
                                       values)
                                    (translate body env fail)))
                              (fail (list "Bad rlambda: " exp))))
                        (fail (list "Bad rlambda " exp))))
                  ((_branch)
                     (if (= (length exp) 6)
                        (let
                           ((a (list-ref exp 2))
                            (b (list-ref exp 3))
                            (then (list-ref exp 4))
                            (else (list-ref exp 5)))
                           (tuple 'branch
                              (list-ref exp 1) ; type
                              (translate a env fail)
                              (translate b env fail)
                              (translate then env fail)
                              (translate else env fail)))
                        (fail (list "Bad branch: " exp))))
                  ((receive) ; (receive <exp> <receiver>)
                     (tuple 'receive
                        (translate (list-ref exp 1) env fail)
                        (translate (list-ref exp 2) env fail)))
                  ((values)
                     (tuple 'values
                        (map (λ (arg) (translate arg env fail)) (cdr exp))))
                  (else
                     (fail
                        (list
                           "Unknown special operator in ast conversion: "
                           exp)))))
            ((bound)
               (mkcall (mkvar (car exp))
                  (map
                     (λ (x) (translate x env fail))
                     (cdr exp))))
            ((defined value)
               (mkcall value
                  (map (λ (x) (translate x env fail)) (cdr exp))))
            (else
               (fail
                  (list
                     "Unknown value type in ast conversion: "
                     (list 'name (car exp) 'value  (lookup env (car exp)))))
               ;(mkval exp)
               )))

      (define (translate exp env fail)
         (cond
            ((null? exp) (mkval exp))
            ((list? exp)
               (if (symbol? (car exp))
                  (translate-direct-call exp env fail translate)
                  (mkcall
                     (translate (car exp) env fail)
                     (map
                        (λ (x) (translate x env fail))
                        (cdr exp)))))
            ((symbol? exp)
               (tuple-case (lookup env exp)
                  ((bound)
                     (mkvar exp))
                  ;; should be already handled in apply-env
                  ((defined value)
                     value)
                  ((special thing)
                     (fail
                        (list "a special thing being used as an argument: " exp)))
                  ((undefined)
                     (fail (list "what are '" exp "'?")))
                  (else
                     (fail
                        (list "Strange value in ast conversion: "
                           (lookup env exp))))))
            (else (mkval exp))))

      (define (sexp->ast exp env)
         (call/cc
            (λ (drop)
               (ok
                  (translate exp env (B drop fail))
                  env))))
))
