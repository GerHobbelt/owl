#| doc
This library exports some read-eval-print-loop functions, such as evaluate.
It is typically called through eval. The `*toplevel*` variable is updated
after each definition, so it can be used to evaluate a term in the corresponding
environment.

```
  (eval (list '+ 1 2) *toplevel*) → 3
  (eval '(/ 1 0) *toplevel*) → #false
```
|#


(define-library (owl eval)

   (export
      evaluate
      exported-eval)

   (import
      (owl core)
      (owl list)
      (owl macro)
      (owl thread)
      (only (owl primop) lets/cc)
      (only (owl primop) call/cc)
      (only (owl syscall) error)
      (owl eval env)
      (owl eval ast)
      (owl eval fixedpoint)
      (owl eval alpha)
      (owl eval cps)
      (owl eval closure)
      (owl eval compile))

   (begin

      (define (ok? x) (eq? (ref x 1) 'ok))
      (define (ok exp env) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))

      (define (execute exp env)
         (receive (exp)
            (λ vals
               (ok
                  (cond
                     ((null? vals) "nothing")
                     ((null? (cdr vals)) (car vals))
                     (else (cons 'values vals)))
                  env))))

      ; (op exp env) -> #(ok exp' env') | #(fail info)
      (define compiler-passes
         (list
                            ;; macros have already been expanded
            apply-env       ;; apply previous definitions
            sexp->ast       ;; safe sane tupled structure
            fix-points      ;; make recursion explicit <3
            alpha-convert   ;; assign separate symbols to all bound values
            cps             ;; convert to continuation passing style
            build-closures  ;; turn lambdas into closures where necessary
            compile         ;; translate and flatten to bytecode
            execute))       ;; call the resulting code

      (define (try-evaluate exp env fail-val)
         (try
            (λ ()
               (call/cc
                  (λ (exit)
                     (fold
                        (λ (state next)
                           (if (ok? state)
                              (next (ref state 2) (ref state 3))
                              (exit state)))
                        (ok exp env)
                        compiler-passes))))
            fail-val))

      (define (evaluate exp env)
         (try-evaluate exp env
            (tuple 'fail "an error occurred")))

      (define (exported-eval exp env)
         (lets/cc fail
            ((abort (λ (why) (fail #f)))
             (env exp (macro-expand exp env abort)))
            (tuple-case (evaluate exp env)
               ((ok value env) value)
               ((fail why) #f))))

))
