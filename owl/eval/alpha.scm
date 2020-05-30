#| doc
This library is used internally to convert all variables to unique ones to
avoid having to track variable shadowing.
|#

;;;
;;; Alpha conversion
;;;

; convert all variables to fresh symbols. makes compilation easier.

(define-library (owl eval alpha)

   (export alpha-convert)

   (import
      (owl core)
      (owl gensym)
      (owl eval ast)
      (owl math)
      (owl list)
      (owl list-extra)
      (owl io)
      (only (owl syscall) error)
      (owl lcd ff))

   (begin
      (define (ok exp env) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))

      (define (gensyms free n)
         (if (= n 0)
            (values #n free)
            (lets ((gens next (gensyms (gensym free) (- n 1))))
               (values (cons free gens) next))))

      (define (alpha-list alpha exps env free)
         (if (null? exps)
            (values #n free)
            (lets
               ((this free (alpha (car exps) env free))
                (tail free (alpha-list alpha (cdr exps) env free)))
               (values (cons this tail) free))))

      (define (alpha exp env free)
         (tuple-case exp
            ((var sym)
               (values (mkvar (get env sym)) free))
            ((call rator rands)
               (lets
                  ((rator free (alpha rator env free))
                   (rands free (alpha-list alpha rands env free)))
                  (values (mkcall rator rands) free)))
            ((lambda formals body)
               (alpha (tuple 'lambda-var #t formals body) env free))
            ((lambda-var fixed? formals body) ;; <- mostly clone branch to be merged later
               (lets
                  ((new-formals free (gensyms free (length formals)))
                   (body free
                     (alpha body
                        (fold
                           (λ (env node)
                              (put env (car node) (cdr node)))
                            env (zip cons formals new-formals))
                        free)))
                  (values (tuple 'lambda-var fixed? new-formals body) free)))
            ((value val)
               (values exp free))
            ((branch kind a b then else)
               (lets
                  ((a free (alpha a env free))
                   (b free (alpha b env free))
                   (then free (alpha then env free))
                   (else free (alpha else env free)))
                  (values
                     (tuple 'branch kind a b then else)
                     free)))
            ((receive from to)
               (lets
                  ((from free (alpha from env free))
                   (to free   (alpha to   env free)))
                  (values (tuple 'receive from to) free)))
            ((values vals)
               (lets ((vals free (alpha-list alpha vals env free)))
                  (values (tuple 'values vals) free)))
            (else
               (error "alpha: unknown AST node: " exp))))

      (define (alpha-convert exp env)
         (lets
            ((exp free
               (alpha exp empty (gensym exp))))
            (ok exp env)))))
