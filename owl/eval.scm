#| doc

Evaluation

Evaluation ties together all the separate compiler passes. Each pass is a function of
the environment in which the evaluation occurs and the current version of the expression
to be evaluated, and returns either an error or next versions of the environment and
expression. In the final step the expression has been converted to a thunk, meaning a
function of zero arguments, which when called gives the value(s) of the expression.

Evaluate returns the success/failure data structure used in the compiler. Exported-eval
is the function typically found at toplevel as eval.
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
      (only (owl syscall) error)
      (owl eval data)
      (owl eval env)
      (owl eval ast)
      (owl eval fixedpoint)
      (owl eval alpha)
      (owl eval cps)
      (only (owl tuple) tuple?) ;; temp
      (owl eval closure)
      (owl eval rtl)
      (only (owl io) print)
      (owl proof)
      )

   (begin

      (define (execute exp env)
         (receive (exp)
            (λ vals
               (ok
                  (cond
                     ((null? vals) "nothing")
                     ((null? (cdr vals)) (car vals))
                     (else (cons 'values vals)))
                  env))))

      ; (op exp env) -> #(ok exp' env') | #(fail info) | success
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
         (try-thunk ;; return fail-val in case of error
            (λ ()
               (call/cc
                  (λ (exit)
                     (fold
                        (λ (state next)
                           (success state
                              ((ok exp env)
                                 (next exp env))
                              ((fail why)
                                 (exit (fail why)))))
                        (ok exp env)
                        compiler-passes))))
            (report-failure env fail-val)
            (list 'repl-eval)))

      (define (evaluate exp env)
         (try-evaluate exp env
            (fail "an error occurred")))

      (define (exported-eval exp env)
         (lets/cc fail
            ((abort (λ (why) (fail #f)))
             (env exp (macro-expand exp env abort)))
            (success (evaluate exp env)
               ((ok value env) value)
               ((fail why)
                  (print why)
                  #f))))

      (example
         (evaluate '(car '(11 22)) *toplevel*) = (ok 11 *toplevel*)
         (exported-eval '(car '(11 . 22)) *toplevel*) = 11
      )

))
