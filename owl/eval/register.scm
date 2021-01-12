#| doc
Register allocation

The VM has only a finite number of registers. This library attempts to reduce
the number of registers needed by the RTL, and choose better registers for
values to avoid unnecessary shuffling of values.

Old code. Scheduled for a rewrite.
|#

(define-library (owl eval register)

   (export
      allocate-registers
      n-registers)

   (import
      (owl core)
      (owl ff)
      (owl math)
      (owl list-extra)
      (owl io)
      (owl equal)
      (owl eval data)
      (only (owl syscall) error)
      (owl list)
      (scheme base))

   (begin

      (define highest-register 90)

      (define n-registers (+ highest-register 1)) ;; see also c/ovm.c:/define.NR/

      ; reg-touch U r -> mark as live -> make sure it has a value
      ; (must be in some register)

      (define (reg-touch usages reg)
         (let ((val (get usages reg #false)))
            (if val
               usages
               (put usages reg #n))))

      ; set value to (reg) -> would be cool to have it in reg

      (define (reg-root usages reg)
         (put usages reg (list reg)))

      ; return a list of registers from uses (where the value has been moved to), or
      ; some list of low registers if this one is outside the available ones
      (define (use-list uses reg)
         (let ((opts (filter (C < highest-register) (get uses reg #n))))
            (cond
               ((< reg highest-register)
                  opts)
               ((null? opts)
                  (iota 4 1 highest-register))
               (else opts))))

      ; union on each register target
      (define (merge-usages a b)
         (ff-fold
            (λ (a reg places)
               (if places
                  (put a reg
                     (union (use-list a reg) places))
                  a))
            a b))

      (define (bad? to target op)
         (or (eq? to target) (not (eq? to (op to)))))

      ; try to rename the register and exit via fail if the values are disturbed

      (define (rtl-rename code op target fail)
         (rtl-case code
            ((ret a)
               (ret (op a)))
            ((move a b more)
               (cond
                  ((eq? b target)
                     (if (eq? b (op a))
                        (rtl-rename more op target fail)
                        (fail)))
                  ((eq? a target)
                     (fail))
                  ((not (eq? b (op b)))
                     (fail))
                  (else
                     (let ((a (op a)))
                        (if (eq? a b)
                           (rtl-rename more op target fail)
                           (move a b (rtl-rename more op target fail)))))))
            ((prim opcode args to more)
               (if (fixnum? to)
                  (if (bad? to target op)
                     (fail)
                     (prim opcode
                        (map op args)
                        to (rtl-rename more op target fail)))
                  (if (bad? to target op)
                     (fail)
                     (prim opcode (map op args) to (rtl-rename more op target fail)))))
            ((cons-close clos? lp off env to more)
               (if (bad? to target op)
                  (fail)
                  (cons-close clos? (op lp) off (map op env) to (rtl-rename more op target fail))))
            ((ld val to cont)
               (if (bad? to target op)
                  (fail)
                  (ld val to (rtl-rename cont op target fail))))
            ((refi from offset to more)
               (if (bad? to target op)
                  (fail)
                  (refi (op from) offset to (rtl-rename more op target fail))))
            ((goto fn nargs)
               (goto (op fn) nargs))
            ((jeqi i a then else)
               (jeqi i (op a) (rtl-rename then op target fail) (rtl-rename else op target fail)))
            ((jeq a b then else)
               (jeq (op a) (op b) (rtl-rename then op target fail) (rtl-rename else op target fail)))))


      ;; try to remap the register to each known good alternative
      ;; finish with (cont the-register code)

      (define (retarget-first code old news uses cont)
         (if (or (null? news) (and (memq old news) (< old highest-register)))
            (cont old code)         ; no remapping happened
            (let ((new (car news)))
               (if (or (eq? old new) (get uses new #false))
                  (retarget-first code old (cdr news) uses cont)
                  (let ((new-code
                     (call/cc
                        (λ (drop)
                           (rtl-rename code
                              (λ (reg) (if (eq? reg old) new reg))
                              new
                              (λ () (drop #f)))))))
                     (if new-code
                        (cont new new-code) ; remapping success
                        (retarget-first code old (cdr news) uses cont)))))))

      (define (rtl-retard-jump proc op a b then else)
         (lets
            ((then then-uses (proc then))
             (else else-uses (proc else))
             (uses (merge-usages then-uses else-uses)))
            (values
               (op a b then else)
               (reg-touch (if (eq? op jeq) (reg-touch uses a) uses) b))))

      (define (rtl-retard-closure rtl-retard clos-type lpos offset env to more)
         (lets
            ((more uses (rtl-retard more))
             (good (use-list uses to))
             (uses (del uses to))
             (pass (λ () (values (cons-close clos-type lpos offset env to more) (fold reg-touch uses (cons lpos env))))))
            (retarget-first more to good uses
               (λ (to-new more-new)
                  (if (eq? to to-new)
                     (pass)
                     (rtl-retard (cons-close clos-type lpos offset env to-new more-new)))))))

      ; retarget register saves to registers where they are moved
      ; where possible (register retargeting level 1)

      (define (rtl-retard code)
         (rtl-case code
            ((ret a)
               ;; needs R3=cont and Ra
               (if (> a highest-register)
                  ;; needs to be relocated lower, so return here a wish to put is somewhere lower
                  (values code
                     (put (reg-root empty 3) a (iota 4 1 highest-register))) ; please move me anywhere lower
                  (values code
                     (reg-touch (reg-root empty 3) a))))

            ((move a b more)
               (cond
                  ((eq? a b)
                     (rtl-retard more)) ; drop useless instruction
                  ((> b highest-register)
                     (error "out of registers in move: " b))
                  (else
                     (lets
                        ((more uses (rtl-retard more))
                         (uses (del uses b))
                         (targets (use-list uses a)))
                        (if (memq b targets)
                           ; moved to a useful target
                           (values (move a b more) uses)
                           ; leave a wish that the value at a could already be in b
                           (values (move a b more) (put uses a (cons b targets))))))))

            ((prim op args to more)
               (lets
                  ((more uses (rtl-retard more))
                   (pass
                     (λ () (values
                        (prim op args to more)
                        (fold reg-touch (del uses to) args)))))
                  (cond
                     ((fixnum? to)
                        ; retarget the sole argument if possible
                        (let ((good (use-list uses to)))
                           (retarget-first more to good uses
                              (λ (to-new more-new)
                                 (if (eq? to to-new)
                                    (pass)
                                    (rtl-retard
                                       (prim op args to-new more-new)))))))
                     ;; todo: no register retargeting for multiple-return-value primops
                     (else
                        (pass)))))

            ((cons-close clos? lpos offset env to more)
               (rtl-retard-closure rtl-retard clos? lpos offset env to more))

            ((ld val to cont)
               (lets
                  ((cont uses (rtl-retard cont))
                   (good (use-list uses to))
                   (good (if (> to highest-register) (append good (iota 0 1 highest-register)) good))
                   (pass (λ () (values (ld val to cont) (del uses to)))))
                  (retarget-first cont to good uses
                     (λ (to-new cont-new)
                        (if (eq? to to-new)
                           (pass)
                           (rtl-retard (ld val to-new cont-new)))))))

            ((refi from offset to more)
               (lets
                  ((more uses (rtl-retard more))
                   (uses (reg-touch uses from))
                   (good (use-list uses to))
                   (uses (del uses to))
                   (pass
                     (λ () (values (refi from offset to more)
                        (reg-touch uses from)))))
                  (retarget-first more to good uses
                     (λ (to-new more-new)
                        (if (eq? to to-new)
                           (pass)
                           (rtl-retard
                              (refi from offset to-new more-new)))))))
            ((goto op nargs)
               (values code (fold reg-root empty (cons op (iota 3 1 (+ 4 nargs))))))
            ((jeqi i a then else)
               (rtl-retard-jump rtl-retard jeqi i a then else))
            ((jeq a b then else)
               (rtl-retard-jump rtl-retard jeq a b then else))))

      (define (allocate-registers rtl)
         (lets ((rtl usages (rtl-retard rtl)))
            rtl))
))
