#| doc
Closure Conversion

Even though all bindings and operations are just lambdas, we need to
differentiate a few kinds of them depending on their content and use.

A lambda occurring in the operator position does not need to be treated
as a function, because it simply means assigning names to values. They
are leaft intact and generally end up being implemented as register moves.

The lambdas we need to represent somehow at runtime are further split
to closures and procedures. A procedure is lambda which does not depend on 
values known only at runtime, so the corresponding object can be constructed at compile
time. In the special case where it also does not require any references
to non-trivial other values, the value ends up being just the bytecode.

A closure is a lambda which requires some values known only at runtime.
For these a procedure is generated at compile time, and instructions to
add the remaining values at runtime are added to bytecode.
|#

(define-library (owl eval closure)

   (export
      build-closures
      uncompiled-closure?)

   (import
      (owl core)
      (owl list)
      (only (owl syscall) error)
      (owl eval ast)
      (owl math)
      (owl tuple)
      (owl list-extra)
      (owl eval env)
      (owl eval data)
      (owl eval assemble))

   (begin

      (define (value-primop val)
         (and (tuple? val)
            (eq? 'value (ref val 1))
            (primitive? (ref val 2))))

      (define (closurize-list closurize exps used)
         (if (null? exps)
            (values #n used)
            (lets
               ((this used (closurize (car exps) used #true))
                (tail used (closurize-list closurize (cdr exps) used)))
               (values (cons this tail) used))))

       (define (closurize-call closurize rator rands used)
         (let ((op (value-primop rator)))
            (if op
               (begin
                  ;(print " no clos for " rator)
                  (tuple-case (car rands)
                     ((lambda-var fixed? formals body)
                        (if fixed?
                           (lets
                              ((cont used (closurize (car rands) used #false))
                               (rands used (closurize-list closurize (cdr rands) used)))
                              (values (mkcall rator (cons cont rands)) used))
                           (error "variable arity receiver lambda: " (car rands))))
                     ((var name)
                        (let
                           ((dummy-cont
                              ;;; FIXME, should check arity & gensym
                              ;;; used only once and called immediately
                              (mklambda
                                 (list '_foo)
                                 (mkcall (mkvar name) (list (mkvar '_foo))))))
                           (closurize-call closurize rator
                              (cons dummy-cont (cdr rands))
                              used)))
                     (else
                        (error "Bad primitive continuation: " (car rands)))))
               (lets
                  ((rator used (closurize rator used #false))
                   (rands used (closurize-list closurize rands used)))
                  (values (mkcall rator rands) used)))))

      ;; todo: could close? be removed now?
      (define (closurize exp used close?)
         (tuple-case exp
            ((value val)
               (values exp used))
            ((var sym)
               (values exp (if (memq sym used) used (cons sym used))))
            ((branch kind a b then else)
               ; type 4 (binding compare) branches do not closurize then-case
               (lets
                  ((a used (closurize a used #true))
                   (b used (closurize b used #true))
                   (then used (closurize then used (not (eq? 4 kind))))
                   (else used (closurize else used #true)))
                  (values
                     (tuple 'branch kind a b then else)
                     used)))
            ((call rator rands)
               (closurize-call closurize rator rands used))
            ((lambda-var fixed? formals body)
               (lets
                  ((body bused (closurize body #n #t))
                   (clos (diff bused formals)))
                  (values
                     (if close?
                        (tuple 'closure-var fixed? formals body clos)
                        (tuple 'lambda-var fixed? formals body))
                     (union used clos))))
            (else
               (error "closurize: unknown exp type: " exp))))

      (define (literalize-list literalize exps used)
         (if (null? exps)
            (values #n used)
            (lets
               ((this used (literalize (car exps) used))
                (tail used (literalize-list literalize (cdr exps) used)))
               (values (cons this tail) used))))

      (define (literalize-call literalize rator rands used)
         (lets
            ((rator used
               (if (value-primop rator)
                  (values rator used)
                  (literalize rator used)))
             (rands used (literalize-list literalize rands used)))
            (values (mkcall rator rands) used)))

      (define closure-tag (list 'uncompiled-closure))

      (define (uncompiled-closure? thing)
         (and (pair? thing) (eq? (car thing) closure-tag)))

      (define (literalize exp used)
         (tuple-case exp
            ((value val)
               (values exp
                  (if (or (memq val used) (small-value? val))
                     used
                     (append used (list val)))))
            ((var sym)
               (values exp used))
            ((branch kind a b then else)
               (lets
                  ((a used (literalize a used))
                   (b used (literalize b used))
                   (then used (literalize then used))
                   (else used (literalize else used)))
                  (values
                     (tuple 'branch kind a b then else)
                     used)))
            ((call rator rands)
               (literalize-call literalize rator rands used))
            ((lambda-var fixed? formals body)
               (lets ((body used (literalize body used)))
                  (values (tuple 'lambda-var fixed? formals body) used)))
            ((closure-var fixed? formals body clos) ;; clone branch, merge later
               ;; note, the same closure exp (as in eq?) is stored to both literals
               ;; and code. the one in code will be used to make instructions
               ;; for closing it and the one in literals will be the executable
               ;; part to close against.
               (lets
                  ((body bused (literalize body #n))
                   (closure-exp (tuple 'closure-var fixed? formals body clos bused))
                   (used (append used (list (cons closure-tag closure-exp)))))
                  (values (tuple 'make-closure (+ 1 (length used)) clos bused) used))
                  )
            (else
               (error "literalize: unknown exp type: " exp))))

      (define (build-closures exp env)
         (lets
            ((exp used (closurize exp #n #t))
             (exp lits (literalize exp #n)))
            (if (and (pair? lits) (uncompiled-closure? (car lits)))
               (ok (cdar lits) env)
               (error "Bad closurize output: "
                  (list 'exp exp 'lits lits)))))
))
