
(define-library (owl core)

   (export
      λ syntax-error begin
      quasiquote letrec let
      letrec*
      if when unless
      cond case define define*
      lets or and list
      ilist tuple tuple-case
      define-library
      case-lambda
      define-values
      define-record-type
      _record-values
      not B C H I K self
      type-complex
      type-rational
      type-int+
      type-int-
      type-record

      immediate? allocated? raw? record?

      -> ->> if-lets

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
      type-rlist-spine
      type-rlist-node
      type-port
      type-string
      type-string-wide
      type-string-dispatch
      type-thread-state

      ;; sketching types
      type-ff               ;; k v, k v l, k v l r, black node with children in order
      type-ff-r             ;; k v r, black node, only right black child
      type-ff-red           ;; k v, k v l, k v l r, red node with (black) children in order
      type-ff-red-r         ;; k v r, red node, only right (black) child

      ;; k v, l k v r       -- type-ff
      ;; k v r, k v l r+    -- type-ff-right
      ;; k v l, k v l+ r    -- type-ff-leftc

      maybe
   )

   (begin

      (define-syntax λ
         (syntax-rules ()
            ((λ . x) (lambda . x))))

      (define-syntax syntax-error
         (syntax-rules (error)
            ((syntax-error . stuff)
               (error "Syntax error: " (quote stuff)))))

      ;; expand case-lambda syntax to to (_case-lambda <lambda> (_case-lambda ... (_case-lambda <lambda> <lambda)))
      (define-syntax case-lambda
         (syntax-rules (lambda _case-lambda)
            ((case-lambda) #false)
            ; ^ should use syntax-error instead, but not yet sure if this will be used before error is defined
            ((case-lambda (formals . body))
               ;; downgrade to a run-of-the-mill lambda
               (lambda formals . body))
            ((case-lambda (formals . body) . rest)
               ;; make a list of options to be compiled to a chain of code bodies w/ jumps
               ;; note, could also merge to a jump table + sequence of codes, but it doesn't really matter
               ;; because speed-sensitive stuff will be compiled to C where this won't matter
               (_case-lambda (lambda formals . body)
                  (case-lambda . rest)))))

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
            ((letrec () . body)
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

      ;; todo: these essential optimizations and more should be handled by partial eval later
      (define-syntax if
         (syntax-rules (not eq? null? empty?)
            ((if test exp) (if test exp #false))
            ((if (not test) then else) (if test else then))
            ((if (null? test) then else) (if (eq? test '()) then else))
            ((if (empty? test) then else) (if (eq? test #empty) then else))
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
         (syntax-rules (else eqv? memv =>)
            ((case (op . args) . clauses)
               (let ((fresh (op . args)))
                  (case fresh . clauses)))
            ((case thing) #false)
            ((case thing ((a) => exp) . clauses)
               (if (eqv? thing (quote a))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a ...) => exp) . clauses)
               (if (memv thing (quote (a ...)))
                  (exp thing)
                  (case thing . clauses)))
            ((case thing ((a) . body) . clauses)
               (if (eqv? thing (quote a))
                  (begin . body)
                  (case thing . clauses)))
            ((case thing (else => func))
               (func thing))
            ((case thing (else . body))
               (begin . body))
            ((case thing ((a . b) . body) . clauses)
               (if (memv thing (quote (a . b)))
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

      ;; let sequence
      (define-syntax lets
         (syntax-rules (<=)
            ((lets ((name val) . bindings) exp . exps)
               ;; (name val) ≡ ((λ (name) ...) val)
               ((lambda (name) (lets bindings exp . exps)) val))
            ((lets (((name . names) <= val) . bindings) exp . exps)
               (bind val
                  (lambda (name . names) (lets bindings exp . exps))))
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


      (define-syntax define-library
         (syntax-rules (export _define-library define-library)
            ;; push export to the end (should syntax-error on multiple exports before this)
            ((define-library x ... (export . e) term . tl)
             (define-library x ... term (export . e) . tl))

            ;; accept otherwise in whatever order
            ((define-library thing ...)
             (_define-library (quote thing) ...))

            ;; fail otherwise
            ((_ . wtf)
               (syntax-error "Weird library contents: " (quote . (define-library . wtf))))))

      ;; toplevel library operations expand to quoted values to be handled by the repl
      ;(define-syntax import  (syntax-rules (_import)  ((import  thing ...) (_import  (quote thing) ...))))
      ;(define-syntax include (syntax-rules (_include) ((include thing ...) (_include (quote thing) ...))))

      (define (B f g) (λ (x) (f (g x))))

      (define (C f y) (λ (x) (f x y)))

      (define (H f x) (λ (y) (f x y)))

      (define (I x) x)

      (define (K x y) x)

      (define self I)

      (define not (C eq? #f))


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
      (define type-rlist-node       14)
      (define type-rlist-spine      10)
      (define type-string            3)
      (define type-string-wide      22)
      (define type-string-dispatch  21)
      (define type-thread-state     31)
      (define type-record            5)
      (define type-int+             40)
      (define type-int-             41)

      ;; transitional trees or future ffs
      (define type-ff               24)
      (define type-ff-r             25)
      (define type-ff-red           26)
      (define type-ff-red-r         27)

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

      (define-syntax ->
         (syntax-rules ()
            ((-> a) a)
            ((-> a ... (op arg ...))
               (op (-> a ...) arg ...))
            ((-> a ... x)
               (x (-> a ...)))))

      (define-syntax ->>
         (syntax-rules ()
            ((->> a) a)
            ((->> a ... (op arg ...))
               (op arg ... (->> a ...)))
            ((->> a ... x)
               (x (->> a ...)))))

      (define-syntax if-lets
         (syntax-rules ()
            ((if-lets () then else)
               then)
            ((if-lets ((k ... val) . rest) then else)
               (lets ((k ... val))
                  (if k
                     (if-lets rest then else)
                     else)))
            ((if-lets bindings then)
               (if-lets bindings then #false))))

      (define (maybe op arg)
         (if arg (op arg) arg))
))
