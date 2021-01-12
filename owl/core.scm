
(define-library (owl core)

   (export
      λ syntax-error begin
      quasiquote letrec let
      letrec*
      if when unless
      cond case define define*
      prod
      lets or and list
      ilist tuple tuple-case
      define-library
      case-lambda
      define-values
      define-record-type
      _record-values
      B C H I K self
      null?
      type-complex
      type-rational
      type-int+
      type-int-
      type-record

      immediate? allocated? raw? record?

      partial pipe piper if-lets ;; -> would be better as |

      type-bytecode
      type-proc
      type-clos
      type-fix+
      type-fix-
      type-pair
      type-vector-dispatch
      type-vector-leaf
      type-bytevector
      type-tuple
      type-symbol
      type-const
      type-port
      type-string
      type-string-wide
      type-string-dispatch
      type-thread-state

      not
      maybe

      primops
      primop-name ;; primop → symbol | primop
      special-bind-primop?
      ;; primop wrapper functions
      run
      set-ticker
      bind
      mkt
      halt
      wait
      ;; extra ops
      set-memory-limit get-word-size get-memory-limit

      apply
      call/cc
      lets/cc
      call/cc      ;; unary continuation, the usual suspect
      call/cc2     ;; returning two values
      call/cc3     ;; return three values
      create-type
      object-size
      object->list
      len

      opcode-arity-ok?

   )

   (begin

      (define-syntax λ
         (syntax-rules ()
            ((λ . x) (lambda . x))))

      (define-syntax syntax-error
         (syntax-rules (error)
            ((syntax-error . stuff)
               (error "Syntax error: " (quote stuff)))))


      (define-syntax begin
         (syntax-rules (define letrec define-values lets)
            ((begin exp) exp)
            ((begin (define . a) (define . b) ... . rest)
               (begin 42 () (define . a) (define . b) ... . rest))
            ((begin (define-values (val ...) . body) . rest)
               (lets ((val ... (begin . body))) (begin . rest)))
            ((begin 42 done (define ((op . args1) . args2) . body) . rest)
               (begin 42 done (define (op . args1) (lambda args2 . body)) . rest))
            ((begin 42 done (define (var . args) . body) . rest)
               (begin 42 done (define var (lambda args . body)) . rest))
            ((begin 42 done (define var exp1 exp2 . expn) . rest)
               (begin 42 done (define var (begin exp1 exp2 . expn)) . rest))
            ((begin 42 done (define var val) . rest)
               (begin 42 ((var val) . done) . rest))
            ((begin 42 done . exps)
               (begin 43 done () exps))
            ((begin 43 (a . b) c exps)
               (begin 43 b (a . c) exps))
            ((begin 43 () bindings exps)
               (letrec bindings (begin . exps)))
            ((begin first . rest)
               ((lambda (free)
                  (begin . rest))
                  first))))

      (define-syntax letrec
         (syntax-rules (rlambda)
            ((letrec ((?var ?val) ...) ?body) (rlambda (?var ...) (?val ...) ?body))
            ((letrec vars body ...) (letrec vars (begin body ...)))))

      (define-syntax letrec*
         (syntax-rules ()
            ((letrec* () . body)
               (begin . body))
            ((letrec* ((var val) . rest) . body)
               (letrec ((var val))
                  (letrec* rest . body)))))

      (define-syntax let
            (syntax-rules ()
               ((let ((var val) ...) exp . rest)
                  ((lambda (var ...) exp . rest) val ...))
               ((let keyword ((var init) ...) exp . rest)
                  (letrec ((keyword (lambda (var ...) exp . rest))) (keyword init ...)))))

      ;; warning: some peephole optimizations done here
      (define-syntax if
         (syntax-rules (not eq? null? empty?)
            ((if test exp) (if test exp #false))
            ((if (not test) then else) (if test else then))               ;; optimization
            ((if (null? test) then else) (if (eq? test '()) then else))   ;; optimization
            ((if (eq? a b) then else) (_branch 0 a b then else))
            ((if (a . b) then else) (let ((x (a . b))) (if x then else)))
            ((if #false then else) else)
            ((if #true then else) then)
            ((if test then else) (_branch 0 test #false else then))))

      (define-syntax when
         (syntax-rules ()
            ((when test exp1 exp2 ...)
               (if test (begin exp1 exp2 ...) #f))))

      (define-syntax unless
         (syntax-rules ()
            ((unless test exp1 exp2 ...)
               (if test #f (begin exp1 exp2 ...)))))

      (define-syntax cond
         (syntax-rules (else =>)
            ((cond) #false)
            ((cond (else exp . rest))
               (begin exp . rest))
            ((cond (clause => exp) . rest)
               (let ((fresh clause))
                  (if fresh
                     (exp fresh)
                     (cond . rest))))
            ((cond (clause exp . rest-exps) . rest)
               (if clause
                  (begin exp . rest-exps)
                  (cond . rest)))))

      (define-syntax case
         (syntax-rules (else eq? memq =>)
            ((case (op . args) . clauses)
               (let ((fresh (op . args)))
                  (case fresh . clauses)))
            ((case thing) #false)
            ((case thing ((a) => exp) . clauses)
               (if (eq? thing (quote a))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a ...) => exp) . clauses)
               (if (memq thing (quote (a ...)))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a) . body) . clauses)
               (if (eq? thing (quote a))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (else => func))
               (func thing))
            ((case thing (else . body))
               (begin . body))
            ((case thing ((a . b) . body) . clauses)
               (if (memq thing (quote (a . b)))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (atom . then) . clauses) ;; added for (case (type foo) (type-foo thenfoo) (type-bar thenbar) ...)
               (if (eq? thing atom)
                  (begin . then)
                  (case thing . clauses)))))

      (define-syntax define
         (syntax-rules (lambda λ)
            ((define op a b . c)
               (define op (begin a b . c)))
            ((define (op . args) body)
               (_define op (rlambda (op) ((lambda args body)) op)))
            ((define name (lambda (var ...) . body))
               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
            ((define name (λ (var ...) . body))
               (_define name (rlambda (name) ((lambda (var ...) . body)) name)))
            ((define op val)
               (_define op val))))

      ;; fixme, should use a print-limited variant for debugging
      (define-syntax define*
         (syntax-rules (print list)
            ((define* (op . args) . body)
               (define (op . args)
                  (print " * " (list (quote op) . args))
                  . body))
            ((define* name (lambda (arg ...) . body))
               (define* (name arg ...) . body))))

      ;; apply to a function or use (lets ((a b c <- (prod 1 2 3))) ...)
      (define-syntax prod
         (syntax-rules ()
            ((prod val ...)
               (lambda (c) (c val ...)))))

      ;; let sequence
      (define-syntax lets
         (syntax-rules (<-)
            ((lets ((name val) . bindings) exp . exps)
               ;; (name val) ≡ ((λ (name) ...) val)
               ((lambda (name) (lets bindings exp . exps)) val))
            ((lets ((name1 name2 ... <- source) . bindings) exp . exps)
               ;; lcd tuple bind
               (source
                  (lambda (name1 name2 ...) (lets bindings exp . exps))))
            ((lets ((name1 name2 ... (op . args)) . bindings) exp . exps)
               ;; (v1 v2 .. vn (op a1 .. an)) ≡ call-with-values, this is a generalization of the above
               (receive (op . args)
                  (lambda (name1 name2 ...) (lets bindings exp . exps))))
            ((lets ((name1 name2 ... val) . bindings) exp . exps)
               (bind val
                  (lambda (name1 name2 ...) (lets bindings exp . exps))))
            ((lets () exp . exps) (begin exp . exps))))

      ;; the internal one is handled by begin. this is just for toplevel.
      (define-syntax define-values
         (syntax-rules (list)
            ((define-values (val ...) . body)
               (_define (val ...)
                  (lets ((val ... (begin . body)))
                     (list val ...))))))

      (define-syntax or
         (syntax-rules ()
            ((or) #false)
            ((or a) a)
            ((or (a . b) . c)
               (let ((x (a . b)))
                  (or x . c)))
            ((or a . b)
               (if a a (or . b)))))

      (define-syntax and
         (syntax-rules ()
            ((and) #true)
            ((and a) a)
            ((and a . b)
               (if a (and . b) #false))))

      (define-syntax list
         (syntax-rules ()
            ((list) '())
            ((list a . b)
               (cons a (list . b)))))

      (define-syntax quasiquote
         (syntax-rules (unquote quote unquote-splicing append _work _sharp_vector list->vector)
                                                   ;          ^         ^
                                                   ;          '-- mine  '-- added by the parser for #(... (a . b) ...) -> (_sharp_vector ... )
            ((quasiquote _work () (unquote exp)) exp)
            ((quasiquote _work (a . b) (unquote exp))
               (list 'unquote (quasiquote _work b exp)))
            ((quasiquote _work d (quasiquote . e))
               (list 'quasiquote
                  (quasiquote _work (() . d) . e)))
            ((quasiquote _work () ((unquote-splicing exp) . tl))
               (append exp
                  (quasiquote _work () tl)))
            ((quasiquote _work () (_sharp_vector . es))
               (list->vector
                  (quasiquote _work () es)))
            ((quasiquote _work d (a . b))
               (cons (quasiquote _work d a)
                     (quasiquote _work d b)))
            ((quasiquote _work d atom)
               (quote atom))
            ((quasiquote . stuff)
               (quasiquote _work () . stuff))))

      (define-syntax ilist
         (syntax-rules ()
            ((ilist a) a)
            ((ilist a . b)
               (cons a (ilist . b)))))

      (define-syntax tuple
         (syntax-rules ()
            ((tuple a . bs) ;; there are no such things as 0-tuples
               (mkt 2 a . bs))))

      ; replace this with typed destructuring compare later on

      (define-syntax tuple-case
         (syntax-rules (else _ is eq? bind type)
            ((tuple-case (op . args) . rest)
               (let ((foo (op . args)))
                  (tuple-case foo . rest)))
            ;;; bind if the first value (literal) matches first of pattern
            ((tuple-case 42 tuple type ((this . vars) . body) . others)
               (if (eq? type (quote this))
                  (bind tuple
                     (lambda (ignore . vars) . body))
                  (tuple-case 42 tuple type . others)))
            ;;; bind to anything
            ((tuple-case 42 tuple type ((_ . vars) . body) . rest)
               (bind tuple
                  (lambda (ignore . vars) . body)))
            ;;; an else case needing the tuple
            ((tuple-case 42 tuple type (else is name . body))
               (let ((name tuple))
                  (begin . body)))
            ;;; a normal else clause
            ((tuple-case 42 tuple type (else . body))
               (begin . body))
            ;;; throw an error if nothing matches
            ((tuple-case 42 tuple type)
               (syntax-error "weird tuple-case"))
            ;;; get type and start walking
            ((tuple-case tuple case ...)
               (let ((type (ref tuple 1)))
                  (tuple-case 42 tuple type case ...)))))

      (define-syntax case-lambda
         (syntax-rules (pair? null? let and walk)

            ((case-lambda (formals . body) ...)
               ;; construct the wrapping lambda
               (lambda args
                  (case-lambda walk args (formals . body) ...)))

            ((case-lambda walk args (() . body) x ...)
               (if (eq? args #n)
                  (begin . body)
                  (case-lambda walk args x ...)))

            ((case-lambda walk args ((a) . body) opts ...)
               (if (and (eq? 1 (type args)) (eq? #null (cdr args)))
                  (let ((a (car args))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a b) . body) opts ...)
               (if (and (eq? 1 (type args)) (eq? 1 (type (cdr args))) (eq? #null (cdr (cdr args))))
                  (let ((a (car args)) (b (car (cdr args)))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a b c) . body) opts ...)
               (if (and (eq? 1 (type args)) (eq? 1 (type (cdr args))) (eq? 1 (type (cdr (cdr args)))) (eq? #null (cdr (cdr (cdr args)))))
                  (let ((a (car args)) (b (car (cdr args))) (c (car (cdr (cdr args))))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a b c d) . body) opts ...)
               (if (and (eq? 1 (type args))
                        (eq? 1 (type (cdr args)))
                        (eq? 1 (type (cdr (cdr args))))
                        (eq? 1 (type (cdr (cdr (cdr args)))))
                        (eq? #null (cdr (cdr (cdr (cdr args))))))
                  (let ((a (car args))
                        (b (car (cdr args)))
                        (c (car (cdr (cdr args))))
                        (d (car (cdr (cdr (cdr args)))))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a b c d . e) . body) opts ...)
               (car "case-lambda-too-large"))

            ((case-lambda walk args ((a b c . d) . body) opts ...)
               (if (and (eq? 1 (type args)) (eq? 1 (type (cdr args))))
                  (let ((a (car args)) (b (car (cdr args))) (c (car (cdr (cdr args)))) (d (cdr (cdr (cdr args))))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a b . c) . body) opts ...)
               (if (and (eq? 1 (type args)) (eq? 1 (type (cdr args))))
                  (let ((a (car args)) (b (car (cdr args))) (c (cdr (cdr args)))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args ((a . b) . body) opts ...)
               (if (eq? 1 (type args))
                  (let ((a (car args)) (b (cdr args))) . body)
                  (case-lambda walk args opts ...)))

            ((case-lambda walk args (a . body) opts ...)
               (let ((a args)) . body))

            ((case-lambda walk args)
               (car "unsupported-case-lambda-args"))
            ))


      (define-syntax define-library
         (syntax-rules (export _define-library)
            ;; push export to the end (should syntax-error on multiple exports before this)
            ((define-library x ... (export . e) term . tl)
             (define-library x ... term (export . e) . tl))

            ;; accept otherwise in whatever order
            ((define-library thing ...)
             (_define-library (quote thing) ...))

            ;; fail otherwise
            ((_ . wat)
               (syntax-error "Weird library contents: " (quote . (define-library . wtf))))))

      (define (B f g) (λ (x) (f (g x))))

      (define (C f y) (λ (x) (f x y)))

      (define (H f x) (λ (y) (f x y)))

      (define (I x) x)

      (define (K x y) x)

      (define self I)

      (define (null? x) (eq? x '()))

      (define-syntax partial
         (syntax-rules ()
            ((_ f x ...)
               (lambda (y)
                  (f x ... y)))))

      ;;;
      ;;; DESCRIPTOR FORMAT
      ;;;
      ;                            .------------> 24-bit payload if immediate
      ;                            |      .-----> type tag if immediate
      ;                            |      |.----> immediateness
      ;   .------------------------| .----||.---> mark bit (can only be 1 during gc, removable?)
      ;  [pppppppp pppppppp pppppppp tttttti0]
      ;   '-------------------------------|
      ;                                   '-----> 4- or 8-byte aligned pointer if not immediate
      ;
      ; object headers are further
      ;                                    .----> immediate
      ;  [ssssssss ssssssss ffffrt?? tttttt10]
      ;   '---------------| '--|||'| '----|
      ;                   |    ||| |      '-----> object type
      ;                   |    ||| '------------> your tags here!
      ;                   |    ||'--------------> teardown bit - something needs to be done, if freed by gc
      ;                   |    |'---------------> rawness bit (raw objects have no descriptors in them)
      ;                   |    '----------------> fractional part of raw object payload size
      ;                   '---------------------> object size in words

      ;; these are core data structure type tags which are fixed and some also relied on by the vm

      ;; ALLOCATED
      (define type-bytecode         16)
      (define type-proc             17)
      (define type-clos             18)
      (define type-pair              1)
      (define type-vector-dispatch  15)
      (define type-vector-leaf      11)
      (define type-bytevector       19) ;; see also TBVEC in c/ovm.c
      (define type-symbol            4)
      (define type-tuple             2)
      (define type-string            3)
      (define type-string-wide      22)
      (define type-string-dispatch  21)
      (define type-thread-state     31)
      (define type-record            5)
      (define type-int+             40)
      (define type-int-             41)

      ;; IMMEDIATE
      (define type-fix+              0)
      (define type-fix-             32)
      (define type-rational         42)
      (define type-complex          43) ;; 3 free below
      (define type-const            13) ;; old type-null, moved from 1, clashing with pairs
      (define type-port             12)


      ;;           allocated/pointers     allocated/rawdata    immediate
      ;; (size  x)         n                       n               #false
      ;; (sizeb x)       #false                    n               #false

      (define (immediate? obj) (eq? (fxand obj 0) 0))
      (define (allocated? obj) (lesser? (fxior 0 obj) obj))
      (define raw? sizeb)


      ;; records could be removed

      (define (record? x) (eq? type-record (type x)))

      (define-syntax _record-values
         (syntax-rules (emit find)
            ((_record-values emit tag mk pred () fields tail)
               (values tag mk pred . tail))
            ((_record-values emit tag mk pred (x ... (field accessor)) fields tail)
               ;; next must cons accessor of field to tail, so need to lookup its position
               (_record-values find tag mk pred (x ...) fields tail field fields (2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
            ((_record-values find tag mk pred left fields tail key (key . rest) (pos . poss))
               (_record-values emit tag mk pred left fields ((C ref pos) . tail)))
            ((_record-values find tag mk pred left fields tail key (x . rest) (pos . poss))
               (_record-values find tag mk pred left fields tail key rest poss))
            ((_record-values find tag mk pred left fields tail key () (pos . poss))
               (syntax-error "Not found in record: " key))
            ((_record-values find tag mk pred left fields tail key (x . rest) ())
               (syntax-error "Implementation restriction: add more offsets to define-record-type macro" tag))))

      (define-syntax define-record-type
         (syntax-rules (emit)
            ((define-record-type name (constructor fieldname ...) pred (field accessor) ...)
               (define-values
                  (name constructor pred accessor ...)
                  (let ((tag (quote name))) ; ← note, not unique after redefinition, but atm seems useful to get pattern matching
                     (_record-values emit
                        tag
                        (λ (fieldname ...) (mkt type-record tag fieldname ...))
                        (λ (ob) (eq? tag (ref ob 1)))
                        ((field accessor) ...) (fieldname ...) ()))))))

      ;; as with fold, left pipe is the default
      ;; left pipe: aggregate calls to first argument on the left
      (define-syntax pipe
         (syntax-rules ()
            ((_ a) a)
            ((_ a ... (op arg ...))
               (op (pipe a ...) arg ...))
            ((_ a ... x)
               (x (pipe a ...)))))


      ;; right pipe: aggregate calls to the rightmost argument
      (define-syntax piper
         (syntax-rules ()
            ((_ a) a)
            ((_ a ... (op arg ...))
               (op arg ... (piper a ...)))
            ((_ a ... x)
               (x (piper a ...)))))

      (define-syntax if-lets
         (syntax-rules ()
            ((if-lets () then else)
               then)
            ((if-lets ((kp k ... val) . rest) then else)
               (lets ((kp k ... val))
                  (if kp
                     (if-lets rest then else)
                     else)))
            ((if-lets bindings then)
               (if-lets bindings then #false))))

      (define (maybe op arg)
         (if arg (op arg) arg))

      (define (not x) (eq? x #f))

      (define bytes->bytecode
         (C raw type-bytecode))

      (define (app a b)
         (if (eq? a '())
            b
            (cons (car a) (app (cdr a) b))))

      ;; l -> fixnum | #false if too long
      (define (len l)
         (let loop ((l l) (n 0))
            (if (eq? l '())
               n
               (lets ((n o (fx+ n 1)))
                  (and (eq? o 0) (loop (cdr l) n))))))

      (define (func lst)
         (lets ((arity lst lst))
            (bytes->bytecode
               (ilist 60 arity lst))))

      ;; changing any of the below 3 primops is tricky. they have to be recognized by the primop-of of
      ;; the repl which builds the one in which the new ones will be used, so any change usually takes
      ;; 2 rebuilds.

      ;; consider getting rid of bind later
      (define bind
         ; (func '(2 32 4 5 24 5))
         '__bind__
         )
      ; this primop is the only one with variable input arity
      (define mkt
         '__mkt__
         ;(func '(4 23 3 4 5 6 7 24 7))
         )

      ;; these rest are easy
      (define car         (func '(2 105 4 5 24 5)))
      (define cdr         (func '(2 169 4 5 24 5)))
      (define cons        (func '(3 51 4 5 6 24 6)))
      (define run         (func '(3 50 4 5 6 24 6)))
      (define set-ticker  (func '(2 62 4 5 24 5)))
      (define sys-prim    (func '(5 63 4 5 6 7 8 24 8)))
      (define sys         (func '(4 27 4 5 6 7 24 7)))
      (define sizeb       (func '(2 28 4 5 24 5)))
      (define raw         (func '(3 59 4 5 6 24 6)))
      (define eq?         (func '(3 7 4 5 6 24 6)))
      (define fxand       (func '(3 18 4 5 6 24 6)))
      (define fxior       (func '(3 29 4 5 6 24 6)))
      (define fxxor       (func '(3 33 4 5 6 24 6)))
      (define type        (func '(2 15 4 5 24 5)))
      (define ref         (func '(3 47 4 5 6 24 6)))

      ;; make thread sleep for a few thread scheduler rounds
      (define (wait n)
         (if (eq? n 0)
            n
            (lets ((n _ (fx- n 1)))
               (set-ticker 0) ;; allow other threads to run
               (wait n))))

      ;; from cps
      (define (special-bind-primop? op)
         (or (eq? op 32) (eq? op 49)))

      (define set (func '(4 45 4 5 6 7 24 7)))
      (define lesser? (func '(3 44 4 5 6 24 6)))
      (define listuple (func '(4 35 4 5 6 7 24 7)))
      (define mkblack (func '(5 48 4 5 6 7 8 24 8)))
      (define mkred (func '(5 176 4 5 6 7 8 24 8)))

      (define apply-error "implementation restriction: please fold a long list instead of applying a function")

      (define (apply fn l)
         (if (null? l) (fn)
            (lets ((a l l))
               (if (null? l) (fn a)
                  (lets ((b l l))
                     (if (null? l) (fn a b)
                        (lets ((c l l))
                           (if (null? l) (fn a b c)
                              (lets ((d l l))
                                 (if (null? l) (fn a b c d)
                                    (lets ((e l l))
                                       (if (null? l) (fn a b c d e)
                                          (car apply-error)))))))))))))

      (define primops
         (list
            ;; input arity includes a continuation
            (tuple 'sys          27 4 1 sys)
            (tuple 'sizeb        28 1 1 sizeb) ;; raw-obj -> number of bytes (fixnum)
            (tuple 'raw          59 2 1 raw) ;; make a raw object
            (tuple 'cons         51 2 1 cons)
            (tuple 'car         105 1 1 car) ;; opcode: 1 << 6 | 41
            (tuple 'cdr         169 1 1 cdr) ;; opcode: 2 << 6 | 41
            (tuple 'eq?           7 2 1 eq?)
            (tuple 'type         15 1 1 type)
            (tuple 'ref          47 2 1 ref)
            (tuple 'mkt          23 'any 1 mkt) ;; mkt type v0..vn t
            (tuple 'fxand        18 2 1 fxand)
            (tuple 'fxior        29 2 1 fxior)
            (tuple 'bind         32 1 #false bind)  ;; (bind thing (lambda (name ...) body)), fn is at CONT so arity is really 1
            (tuple 'fxxor        33 2 1 fxxor)
            (tuple 'set          45 3 1 set)   ;; (set tuple pos val) -> tuple'
            (tuple 'lesser?      44 2 1 lesser?)  ;; (lesser? a b)
            (tuple 'listuple     35 3 1 listuple)  ;; (listuple type size lst)
            (tuple 'fx-          21 2 2 fx-) ;; (fx- a b) -> lo u
            (tuple 'fx+          22 2 2 fx+) ;; (fx+ a b) -> lo hi
            (tuple 'fxqr         26 3 3 fxqr)  ;; (fxdiv ah al b) -> qh ql r
            (tuple 'fx*          39 2 2 fx*)   ;; (fx* a b)      ;; 2 out
            (tuple 'fx>>         58 2 2 fx>>)   ;; (fx>> a b) -> hi lo, lo are the lost bits
            (tuple 'sys-prim     63 4 1 sys-prim)))

      ;; warning: O(n)
      (define (opcode->primop op)
         (let loop ((primops primops))
            (cond
               ((null? primops) (car 1142)) ;; no (error ...) yet here, failing in a unique way to find this if necessary
               ((eq? (ref (car primops) 2) op)
                  (car primops))
               (else
                  (loop (cdr primops))))))

      (define (opcode-arity-ok? op n)
         (bind (opcode->primop op)
            (λ (name op in out fn)
               (cond
                  ((eq? in n) #true)
                  ((eq? in 'any) #true)
                  (else #false)))))


      ;; special things exposed by the vm
      (define (set-memory-limit n) (sys-prim 7 n #f #f))
      (define (get-word-size) (sys-prim 8 1 #f #f))
      (define (get-memory-limit) (sys-prim 9 #f #f #f))

      ;; stop the vm *immediately* without flushing input or anything else with return value n
      (define (halt n) (sys-prim 6 n #f #f))

      (define call/cc
         ('_sans_cps
            (λ (k f) (f k (lambda (_ a) (k a))))))

      (define call/cc2
         ('_sans_cps
            (λ (k f) (f k (lambda (_ a b) (k a b))))))

      (define call/cc3
         ('_sans_cps
            (λ (k f) (f k (lambda (_ a b c) (k a b c))))))

      (define-syntax lets/cc
         (syntax-rules (call/cc)
            ((lets/cc (om . nom) . fail)
               (syntax-error "let/cc: continuation name cannot be " (quote (om . nom))))
            ((lets/cc var . body)
               (call/cc (λ (var) (lets . body))))))

      (define-syntax lets/cc1
         (syntax-rules (call/cc1)
            ((lets/cc1 (om . nom) . fail)
               (syntax-error "let/cc1: continuation name cannot be " (quote (om . nom))))
            ((lets/cc1 var . body)
               (call/cc1 (λ (var) (lets . body))))))

      ;; unsafe function - not to be exported!
      (define get-header (bytes->bytecode '(1 4 0 5 24 5)))

      (define (create-type type)
         (let ((hdr (get-header (raw '() type))))
            (fxxor hdr hdr)))

      (define (object-size obj)
         (if (immediate? obj)
            0
            (lets ((s _ (fx>> (get-header obj) 8))) s))) ; 8 == SPOS - IPOS

      ;; non-primop instructions that can report errors
      (define (instruction-name op)
         (cond
            ((eq? op 32) 'bind)
            ((eq? op 50) 'run)
            ((eq? op 61) 'arity-error)
            ((eq? op 60) 'arity-error)
            ((eq? op 57) 'variable-arity-error)
            (else #false)))

      (define (primop-name pop)
         (let ((pop (fxand pop 63))) ; ignore top bits which sometimes have further data
            (or (instruction-name pop)
               (let loop ((primops primops))
                  (cond
                     ((null? primops) pop)
                     ((eq? pop (ref (car primops) 2))
                        (ref (car primops) 1))
                     (else
                        (loop (cdr primops))))))))

      (define (read-object obj pos lst)
         (lets ((pos _ (fx- pos 1)))
            (if (eq? pos 0)
               lst
               (read-object obj pos
                  (cons (ref obj pos) lst)))))

      (define (object->list obj)
         (read-object obj (object-size obj) #n))

))
