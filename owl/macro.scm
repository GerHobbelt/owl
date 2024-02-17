#| doc
Macro Expansion

Macros are operations on code to be performed before evaluating it. This is
done by the macro expander defined in this library. The role of the macro
expander is to apply macros at appropriate positions on the code and pass
required information to it. The actual transformations are done by the
functions stored as values of the macros.

A function to be used as a macro receives two arguments, the form to be
expanded and the next free fresh symbol not occurring in the form or the parent
expression of it, and returns a tuple of the expanded form and the next free
symbol.

The macro expander can be tested from toplevel by using ,expand.
|#

(define-library (owl macro)

   (export
      macro-expand   ;    (exp env fk) → env' exp' | (fk failure-reason)
      match)

   (import
      (owl core)
      (owl list)
      (owl function)
      (only (owl syscall) error)
      (owl equal)
      (owl list-extra)
      (owl math)
      (owl io)
      (owl sort)
      (owl gensym)
      (owl symbol)
      (owl eval env)
      (scheme cxr))

   (begin


      ;;;
      ;;; Basic pattern matching for matching the rule pattern against sexp
      ;;;

      (define (match pattern exp)

         (define (match-pattern pattern exp vals)
            (cond
               ((not vals) #false)
               ((pair? pattern)
                  (if (pair? exp)
                     (match-pattern (car pattern) (car exp)
                        (match-pattern (cdr pattern) (cdr exp) vals))
                     #false))
               ((eq? pattern exp) vals)
               ((eq? pattern '_) vals)
               ((function? pattern)
                  (if (pattern exp) (cons exp vals) #false))
               (else #false)))

         (match-pattern pattern exp #n))



      ; expand all macros top to bottom
      ; exp env free -> #(exp' free')

      (define (syntax-error? exp)
         (and
            (pair? exp)
            (eq? (car exp) 'syntax-error)
            (list? exp)))

      (define (expand exp env free abort)

         (define (expand-list exps env free)
            (if (null? exps)
               (values #n free)
               (lets
                  ((this free (expand (car exps) env free abort))
                   (tail free (expand-list (cdr exps) env free)))
                  (values (cons this tail) free))))

         (cond
            ((null? exp)
               (values exp free))
            ((list? exp)
               (cond
                  ((symbol? (car exp))
                     (tuple-case (lookup env (car exp))
                        ((special thing)
                           (case thing
                              ((quote) (values exp free))
                              ((_define)
                                 ; (print " - expanding define body " (caddr exp))
                                 (lets
                                    ((value free
                                       (expand (caddr exp) env free abort)))
                                    (values
                                       (list '_define (cadr exp) value)
                                       free)))
                              ((lambda)
                                 (if (or (null? (cdr exp)) (null? (cddr exp))) ;; todo: use matcher instead
                                    (abort (list "Bad lambda: " exp))
                                    (lets
                                       ((formals (cadr exp))
                                        (body-exps (cddr exp))
                                        (body
                                          (if (and (pair? body-exps) (null? (cdr body-exps)))
                                             (car body-exps)
                                             (cons 'begin body-exps)))
                                        (body free
                                          (expand body (env-bind env formals) free abort)))
                                       (values (list 'lambda formals body) free))))
                              ((rlambda)
                                 (lets
                                    ((formals (list-ref exp 1))
                                     (definitions (list-ref exp 2))
                                     (body (list-ref exp 3))
                                     (env (env-bind env formals))
                                     (definitions free
                                       (expand-list definitions env free))
                                     (body free
                                       (expand body env free abort)))
                                    (values
                                       (list 'rlambda formals definitions body)
                                       free)))
                              ((receive)
                                 (expand-list exp env free))
                              ((_branch)
                                 (expand-list exp env free))
                              ((values)
                                 (expand-list exp env free))
                              (else
                                 (abort
                                    (list "expand: unknown special form: " exp)))))
                        ((bound)          (expand-list exp env free))
                        ((defined value)  (expand-list exp env free))
                        ((undefined)
                           ;; Library definitions should be handled before macros. Current approach
                           ;; allows macros to expand to libraries, but this seems in retrospect useless
                           ;; and requires macro expander to worry about structure of libraries.
                           (cond
                              ((memq (car exp) '(import export)) ;; library definitions
                                 (values exp free))
                              ;((memq (car exp) '(_define-macro)) ;; definition
                              ;   ())
                              (else (expand-list exp env free))))
                        ((macro transformer)
                           ;; this is where the actual macro expansion takes place
                           (let ((result (transformer exp free)))
                              (cond
                                 ((not result)
                                    (abort exp))
                                 ((syntax-error? result)
                                    (abort (list 'syntax-error exp result)))
                                 (else
                                    (expand (ref result 1) env (ref result 2) abort)))))
                        (else is node
                           ; usually bad module exports, since those are not checked atm
                           (abort (list "expand: rator maps to unknown value " (car exp))))))
                  (else
                     (expand-list exp env free))))
            ((symbol? exp)
               (tuple-case (lookup env exp)
                  ;((macro transformer)
                  ;   (abort (list "Macro being used as a value: " exp)))
                  ((undefined)
                     ;; this can still be a literal used by a macro
                     (values exp free))
                  (else
                     (values exp free))))
            (else
               (values exp free))))

      (define (macro-expand exp env fail)
         (lets/cc exit ;; in case fail is not a continuation
            ((abort (B exit fail))
             (free (gensym exp))
             (exp free (expand exp env free abort)))
            (values env exp)))
))
