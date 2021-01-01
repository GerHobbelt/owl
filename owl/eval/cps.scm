#| doc
Continuation Passing Style

The continuation passing style, or CPS, transformation converts a lambda expression
to another lambda expression, with some useful properties. The output is a function
with one extra argument, which will be called with the result of the original function.

There are three main reasons for using the conversion. For one, the resulting lambda
expression can be evaluated without the need for a stack, because nested function
calls are eliminated and replaced by tail calls. What would normally be stored on
a stack now ends up being stored in a closure.

The second reason is that Scheme allows capturing the added argument, which is called
the continuation, to a variable. This requires no special support from the runtime,
because the continuation is just a regular lambda.

Thirdly the presence of continuations makes implementing multithreading easy. Owl
has pre-emptive multithreading, meaning a thread does not need to voluntarily give
up control for other threads to run. This is done by jumping from the thread-local
continuation to thread-scheduler continuation whenever a thread switch occurs.
|#

(define-library (owl eval cps)

   (export cps)

   (import
      (owl core)
      (owl eval ast)
      (owl list)
      (owl list-extra)
      (owl math)
      (owl gensym)
      (owl io)
      (only (owl syscall) error)
      (only (owl eval env) primop? primop-of)
      (owl eval data))

   (begin
      ;; fixme: information about cps-special primops could be stored elsewhere


      (define (cps-literal exp env cont free)
         (values
            (mkcall cont (list exp))
            free))

      (define (cps-just-lambda cps formals fixed? body env free)
         (lets
            ((cont-sym free (fresh free))
             (body free (cps body env (mkvar cont-sym) free)))
            (values
               (tuple 'lambda-var fixed?
                  (cons cont-sym formals) body)
               free)))

      (define (cps-lambda cps formals fixed? body env cont free)
         (lets ((lexp free (cps-just-lambda cps formals fixed? body env free)))
            (values (mkcall cont (list lexp)) free)))

      (define (cps-args cps args call env free)
         (if (null? args)
            (lets
               ((call (reverse call))
                (rator (car call))
                (rands (cdr call)))
               (values (mkcall rator rands) free))
            (tuple-case (car args)
               ((lambda-var fixed? formals body)
                  (lets ((lexp free (cps-just-lambda cps formals fixed? body env free)))
                     (cps-args cps (cdr args) (cons lexp call) env free)))
               ((value foo)
                  (cps-args cps (cdr args) (cons (car args) call) env free))
               ((var foo)
                  (cps-args cps (cdr args) (cons (car args) call) env free))
               (else
                  (lets
                     ((this free (fresh free))
                      (rest free
                        (cps-args cps (cons (mkvar this) (cdr args)) call env free)))
                     (cps (car args)
                        env
                        (mklambda (list this) rest)
                        free))))))

      (define (cps-values cps vals env cont free)
         (cps-args cps vals (list cont) env free))

      ;; fixme: check - assuming tuple exp is already cps'd
      (define (cps-bind cps rator rands env cont free)
         (if (= (length rands) 2)
            (tuple-case (cadr rands)
               ((lambda-var fixed? formals body)
                  (if fixed?
                     (lets ((body free (cps body env cont free)))
                        (cps-args cps (list (car rands))
                           (list (mklambda formals body) rator)
                           env free))
                     (error "bad tuple bind: " 'variable-arity)))
               (else
                  (error "bad arguments for tuple bind: " rands)))
            (error "bad arguments for tuple bind: " rands)))


      ;; (a0 .. an) → (cons a0 (cons .. (cons an #n))), modulo AST
      (define (enlist-tail args)
         (foldr
            (λ (x tl)
               (mkcall (tuple 'value cons) (list x tl)))
            (tuple 'value #n)
            args))

      ;; (f0 .. fn) (a0 ... am) → #false | (a0 ... an-1 (cons an (cons ... (cons am #n))))
      (define (enlist-improper-args formals args)
         (cond
            ((null? (cdr formals))
               (list (enlist-tail args)))
            ((null? args) #false) ;; too few args
            ((enlist-improper-args (cdr formals) (cdr args)) =>
               (λ (tail) (cons (car args) tail)))
            (else #false)))

      (define (cps-call cps rator rands env cont free)
         (tuple-case rator
            ((lambda-var fixed? formals body)
               (cond
                  (fixed?
                     (lets
                        ((body free (cps body env cont free)))
                        (if (null? formals)
                           ;;; drop lambdas from ((lambda () X))
                           (values body free)
                           (cps-args cps rands
                              (list (mklambda formals body))
                              env free))))
                  ((enlist-improper-args formals rands) => ;; downgrade to a regular lambda converting arguments
                     (λ (rands)
                        (cps-call cps
                           (mklambda formals body)
                           rands env cont free)))
                  (else
                     (error "Bad head lambda arguments:" (list 'args formals 'rands rands)))))
            ((call rator2 rands2)
               (lets
                  ((this free (fresh free))
                   (call-exp free
                     (cps-args cps rands (list cont (mkvar this)) env free)))
                  (cps rator env
                     (mklambda (list this) call-exp)
                     free)))
            ((branch kind a b then else)
               (lets ((this free (fresh free)))
                  (cps
                     (mkcall (mklambda (list this) (mkcall (mkvar this) rands))
                        (list rator))
                     env cont free)))
            ((value val)
               (let ((pop (primop-of val)))
                  (if (special-bind-primop? pop)
                     (cps-bind cps rator rands env cont free)
                     (cps-args cps rands (list cont rator) env free))))
            (else
               (cps-args cps rands (list cont rator) env free))))

      (define (cps-branch cps kind a b then else env cont free)
         (cond
            ((not (var? cont))
               (lets
                  ((this free (fresh free))
                   (exp free
                     (cps-branch cps kind a b then else env (mkvar this) free)))
                  (values
                     (mkcall
                        (mklambda (list this) exp)
                        (list cont))
                     free)))
            ((call? a)
               (lets
                  ((this free (fresh free))
                   (rest free
                     (cps-branch cps kind (mkvar this) b then else env cont free)))
                  (cps a env (mklambda (list this) rest) free)))
            ((call? b)
               (lets
                  ((this free (fresh free))
                   (rest free
                     (cps-branch cps kind a (mkvar this) then else env cont free)))
                  (cps b env (mklambda (list this) rest) free)))
            (else
               (lets
                  ((then free (cps then env cont free))
                   (else free (cps else env cont free)))
                  (values
                     (tuple 'branch kind a b then else)
                     free)))))

      (define (cps-receive cps exp semi-cont env cont free)
         (tuple-case semi-cont
            ((lambda-var fixed? formals  body)
               (lets ((body-cps free (cps body env cont free)))
                  (cps exp env
                     (tuple 'lambda-var fixed? formals body-cps)
                     free)))
            (else
               (error "cps-receive: receiver is not a lambda. " semi-cont))))

      (define (cps-exp exp env cont free)
         (tuple-case exp
            ((value val)
               (cps-literal exp env cont free))
            ((var sym)
               (cps-literal exp env cont free))
            ((lambda-var fixed? formals body)
               (cps-lambda cps-exp formals fixed? body env cont free))
            ((call rator rands)
               (cps-call cps-exp rator rands env cont free))
            ((branch kind a b then else)
               (cps-branch cps-exp kind a b then else env cont free))
            ((values vals)
               (cps-values cps-exp vals env cont free))
            ((receive exp target)
               (cps-receive cps-exp exp target env cont free))
            (else
               (error "CPS does not do " exp))))

      (define (val-eq? node val)
         (tuple-case node
            ((value this)
               (eq? this val))
            (else #false)))

      (define (cps exp env)
         (or
            (call/cc
               (λ (fail)
                  (let ((cont-sym (gensym exp)))
                     (if (and
                           (call? exp)
                           (val-eq? (ref exp 2) '_sans_cps)
                           ; (= (length (ref exp 3)) 1)
                           )
                        (ok
                           (mklambda (list cont-sym)
                              (mkcall (mkvar cont-sym)
                                 (list (car (ref exp 3)))))
                           env)
                        (lets ((exp free (cps-exp exp env (mkvar cont-sym) (gensym cont-sym))))
                           (ok
                              (mklambda (list cont-sym) exp)
                              env))))))
            (fail "cps failed")))
   ))
