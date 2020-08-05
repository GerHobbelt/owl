#| doc
Evaluation

This library exports some read-eval-print-loop functions, such as evaluate.
It is typically called through eval. The `*toplevel*` variable is updated
after each definition, so it can be used to evaluate a term in the corresponding
environment.

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
      (owl eval data)
      (owl eval env)
      (owl eval ast)
      (owl eval fixedpoint)
      (owl eval alpha)
      (owl eval cps)
      (only (owl tuple) tuple?) ;; temp
      (owl eval closure)
      (owl eval compile)
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

      ;; -> old-success, internal ones can return a new one
      (define (try-evaluate exp env fail-val)
         (try ;; return fail-val in case of error
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
            fail-val))

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
         (exported-eval '(car '(11 . 22)) *toplevel*) = 11
      )   

))
