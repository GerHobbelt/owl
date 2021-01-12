#| doc
Bytecode Assembly

This library converts the RTL expressions to bytecode executable by the VM.

CPS conversion usually produces a lot of small functions with equal bytecode but potentially
different bindings in the environment. In order to make programs smaller and improve cache
friendliness the bytecode is interned, just like we do with symbols.

|#

(define-library (owl eval assemble)

   (export
      assemble-code
      simple-value?
      small-value?)

   (import
      (owl core)
      (owl ff)
      (owl list)
      (owl math)
      (owl list-extra)
      (owl eval data)
      (only (owl eval data) rtl-case)
      (only (owl syscall) error interact)
      (only (owl eval register) allocate-registers n-registers))

   (begin

      ;; primops = (#(name opcode in-args|#f out-args|#f wrapper-fn) ...)

      ;; ff of opcode → (in|#f out|#f), #f if variable
      (define primop-arities
         (fold
            (λ (ff node)
               (lets ((name op in out wrapper node))
                  (put ff op (cons in out))))
            empty primops))

      (define (opcode-arity-ok? op in out)
         (let ((node (get primop-arities op)))
            (if node
               (and
                  (or (eq? in  (car node)) (not (car node)))
                  (or (eq? out (cdr node)) (not (cdr node))))
               #true)))

      (define (variable-input-arity? op)
         (eq? (car (get primop-arities op '(0))) 'any))

      (define (multiple-return-variable-primop? op)
         (and
            (not (eq? op 32)) ; bind
            (lesser? 1 (cdr (get primop-arities op '(0 . 0))))))

      (define vm-instructions
         (list->ff
            '((move . 9)      ; move a, t:      Rt = Ra
              (refi . 1)      ; refi a, p, t:   Rt = Ra[p], p unsigned
              (goto . 3)      ; jmp a, nargs    call Ra with nargs args
              (clos . 134)    ; clos lp, o, nenv, e0 ... en, t:
              (cloc . 6)      ; cloc lp, o, nenv, e0 ... en, t:
              (clos1 . 198)
              (cloc1 . 70)
              (move2 . 5)     ; two moves, 4 args
              (jeq . 8)       ; jeq a b o1 o2   ip += o if Ra == Rb
              (jeqi . 16)     ; jeqi a o1 o2    ip += o if Ra == imm[i>>6]
              (ld . 10)       ; ldfix n t, encoding: nnnnnnnn nsoooooo (n-1/sign/op)
              (ldi . 13)
              (ret . 24)      ; ret a:          call R3 (usually cont) with Ra
              )))

      (define simple-values
         (list->ff '((0 . 0) (() . 64) (#t . 128) (#f . 192))))

      (define (simple-value? val)
         (get simple-values val #f))

      (define (small-value? val)
         (or
            (simple-value? val)
            (and (fixnum? val) (<= -512 val 512))))

      (define (inst->op name)
         (or
            (get vm-instructions name #false)
            (error "inst->op: unknown instruction " name)))

      ;;;
      ;;; Bytecode assembly
      ;;;

      (define (output-code op lst)
         (if (> op #xff)
            (output-code
               (>> op 8)
               (cons (fxand op #xff) lst))
            (cons op lst)))

      (define (ld-encode val)
         (lets
            ((n _ (fx- (<< val 6) 59)) ; 59 == (-(ld = 10) >> 1 & 63)
             (n (fxior n (type val)))
             (n _ (fx+ n n)))
            n))

      ; rtl -> list of bytes
      ;; ast fail-cont → code' | (fail-cont <reason>)
      (define (assemble code fail)
         (rtl-case code
            ((ret a)
               (list (inst->op 'ret) a))
            ((move a b more)
               (lets
                  ((tl (assemble more fail))
                   (op (inst->op 'move)))
                  (if (eq? (car tl) op) ;; [move a b] + [move c d] = [move2 a b c d] to remove a common dispatch
                     (ilist (inst->op 'move2) a b (cdr tl))
                     (ilist op a b tl))))
            ((prim op args to more)
               (cond
                  ;; fixme: handle mk differently, this was supposed to be a temp hack
                  ((> op #xff)
                     (output-code op
                        (cons (length (cdr args)) ; tuple size
                           (cons (car args) ; type
                              (append (cdr args)
                                 (cons to
                                    (assemble more fail)))))))
                  ((variable-input-arity? op)
                     ;; fixme: no output arity check
                     (cons op
                        (cons (length args)
                           (append args
                              (cons to
                                 (assemble more fail))))))
                  ((fixnum? to)
                     (if (opcode-arity-ok? op (length args) 1)
                        (cons op
                           (append args
                              (cons to
                                 (assemble more fail))))
                        (fail (list "Bad opcode arity for " op (length args) 1))))
                  ((list? to)
                     (if (opcode-arity-ok? op (length args) (length to))
                        (cons op
                           (append args
                              (if (multiple-return-variable-primop? op)
                                 ; <- nargs implicit, FIXME check nargs opcode too
                                 (append to
                                    (assemble more fail))
                                 (cons (length to)          ; <- prefix with output arity
                                    (append to
                                       (assemble more fail))))))
                        (fail (list "Bad opcode arity: " (list op (length args) (length to))))))
                  (else
                     (fail (list "bad case of primop in assemble: " op)))))
            ((cons-close closure? lpos offset env to more)

               ;; closure = clos -> proc -> code
               (if (= lpos 1)
                  (cons (inst->op (if closure? 'clos1 'cloc1))
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons offset
                           (append env
                              (cons to
                                 (assemble more fail))))))
                  (cons (inst->op (if closure? 'clos 'cloc))
                     (cons (+ 2 (length env))
                        ;; size of object (hdr code e0 ... en)
                        (cons lpos
                           (cons offset
                              (append env
                                 (cons to
                                    (assemble more fail)))))))))
            ((ld val to cont)
               (cond
                  ((simple-value? val) =>
                     (λ (i)
                        (ilist (fxior (inst->op 'ldi) i) to
                           (assemble cont fail))))
                  ((fixnum? val)
                     (let ((val (ld-encode val)))
                        (ilist (fxand val #xff) (>> val 8) to
                           (assemble cont fail))))
                  (else
                     (fail (list "cannot assemble a load for " val)))))
            ((refi from offset to more)
               (ilist
                  (inst->op 'refi) from offset to
                  (assemble more fail)))
            ((goto op nargs)
               (list (inst->op 'goto) op nargs))
            ((jeqi i a then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (if (> len #xffff)
                     (fail (list "invalid jump offset: " len))
                     (ilist (fxior (inst->op 'jeqi) i) a (fxand len #xff) (>> len 8) (append else then)))))
            ((jeq a b then else)
               (lets
                  ((then (assemble then fail))
                   (else (assemble else fail))
                   (len (length else)))
                  (if (> len #xffff)
                     (fail (list "invalid jump offset: " len))
                     (ilist (inst->op 'jeq) a b (fxand len #xff) (>> len 8) (append else then)))))))

      ;; make bytecode and intern it (to improve sharing, not mandatory)
      (define (bytes->bytecode bytes)
         (interact 'intern (raw bytes type-bytecode)))

      ; code rtl object -> executable code
      (define (assemble-code obj tail)
         (tuple-case obj
            ;((code arity insts)
            ;   (assemble-code (tuple 'code-var #true arity insts) tail))
            ((code-var fixed? arity insts)
               (lets ((insts (allocate-registers insts)))
                  (if (not insts)
                     (error "failed to allocate registers" "")
                     (lets/cc ret
                        ((fail (λ (why) (error "Error in bytecode assembly: " why) #false))
                         ;(insts (update-rtl insts))
                         (bytes (assemble insts fail))
                         (len (length bytes)))
                        (if (> len #xffff)
                           (error "too much bytecode: " len))
                        ;; drop instructions 2 and 25 after conversion
                        (bytes->bytecode
                           (if fixed?
                              (ilist 60 arity (append bytes tail))
                              (ilist 57 (- arity 1) ;; last is the optinal one
                                 (append bytes tail))))))))
            (else
               (error "assemble-code: unknown AST node " obj))))
))
