#| doc
C Translator

This library is needed when compiling programs to C-programs. This
library is also used to compile Owl itself.

The simples way to build standalone C-programs out of owl programs is to write
the virtual machine code and the FASL-encoded program to run to a file.
This happens when owl is invoked without the -O flags. On the plus side the
resulting program has very little C-code, so the C-compiler won't take
much time when compiling it. This is how the small Owl version is built.

Typically the output is optimized. This is done by translating some of the
bytecode sequences of the program to be compiled to corresponding C-code
fragments, adding them as new instructions to the vanilla virtual machine, and
replacing them from the program with calls to the new instructions. The
resulting C-code is larger and may take quite a bit of memory to compile
using some C-compilers, but the resulting binary will be faster. This
is how the usual bin/ol interpreter is built.
|#

(define-library (owl eval cgen)
   (export
      compile-to-c            ;; obj extras → #false | c-code-string
   )

   (import
      (owl core)
      (owl list)
      ; (owl io)
      (owl list-extra)
      (owl bytevector)
      (owl math)
      (owl function)
      (owl ff)
      (owl string)
      (owl render)
      (only (owl syscall) error))

   (begin

      ; -> list of bytes | #false
      (define (code->bytes code extras)
         (if (bytecode? code)
            (let ((bytes (bytevector->list code)))
               (if (eq? (car bytes) 0) ;; (0 <hi8> <lo8>) == call extra instruction
                  (lets
                     ((opcode (fxior (<< (cadr bytes) 8) (car (cddr bytes))))
                      (bytecode (get extras opcode #false)))
                     (if bytecode
                        (code->bytes bytecode extras) ;; <- vanilla bytecode (modulo boostrap bugs)
                        (error "code->bytes: cannot find original bytecode for opcode " opcode)))
                  bytes))
            #false))

      (define (unknown bs regs fail)
         ; (print " - cgen does not grok opcode " (car bs) " " (if (> (car bs) 63) (list 'low (band (car bs) 63)) ""))
         (fail))

      (define (get1 l) ; (a . tl)
         (values (car l) (cdr l)))

      (define (get2 l) ; (a b . tl)
         (let ((tl (cdr l)))
            (values (car l) (car tl) (cdr tl))))

      (define (get3 l)
         (lets ((b c tl (get2 (cdr l))))
            (values (car l) b c tl)))

      (define (get4 l)
         (lets ((b c d tl (get3 (cdr l))))
            (values (car l) b c d tl)))

      (define (get5 l)
         (lets ((b c d e tl (get4 (cdr l))))
            (values (car l) b c d e tl)))

      (define (get6 l)
         (lets ((b c d e f tl (get5 (cdr l))))
            (values (car l) b c d e f tl)))

      ;; register values
      ;;    #false | not set = no idea what is here
      ;;    one of immediate (bool, fixnum) -> immediate (of this type)
      ;;    one of alloc (pair) -> allocated object (of this type)

      (define (alloc? v)
         (cond
            ((not v) #false)
            ((memq v '(pair alloc)) #true)
            (else #false)))

      ;; drop code to check that value in ref is a pointer (not immediate) unless this is already known in regs
      (define (assert-alloc regs reg op tl)
         (if (alloc? (get regs reg #false))
            tl
            (ilist "assert(allocp(R["reg"]),R["reg"],"op");" tl)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; translator functions


      ;; lraw lst-reg type-reg flipp-reg to
      (define (cify-sysprim bs regs fail)
         (lets ((op a1 a2 a3 ret bs (get5 (cdr bs))))
            (values
               (list "R["ret"]=prim_sys(R["op"],R["a1"],R["a2"],R["a3"]);\n")
               bs (del regs ret))))

      ;; lraw lst-reg type-reg flipp-reg to
      (define (cify-sizeb bs regs fail)
         (lets ((ob to bs (get2 (cdr bs))))
            (values
               (list "if(immediatep(R["ob"])){R["to"]=IFALSE;}else{hval h=V(R["ob"]);R["to"]=rawp(h)?F(payl_len(h)):IFALSE;}")
               bs (put regs to 'fixnum)))) ;; output is always a fixnum

      ;; lraw lst-reg type-reg flipp-reg to
      (define (cify-lraw bs regs fail)
         (lets ((lr tr to bs (get3 (cdr bs))))
            (values (list "R["to"]=prim_lraw(R["lr"],R["tr"]);") bs
               (del regs to)))) ; <- lraw can fail

      ;; ref ob pos to
      (define (cify-ref bs regs fail)
         (lets ((ob pos to bs (get3 (cdr bs))))
            (values (list "R["to"]=prim_ref(R["ob"],R["pos"]);\n") bs
               (del regs to))))

      ; fx+ a b r o
      (define (cify-fxadd bs regs fail)
         (lets ((a b r o bs (get4 (cdr bs))))
            (values
               ;; res is shifted down, so there is room for an overflow bit
               (list "{hval r=immval(R["a"])+immval(R["b"]);R["o"]=F(r>>FBITS);R["r"]=F(r);}\n")
               bs (put (put regs r 'fixnum) o 'bool))))

      ; fxand a b r
      (define (cify-fxand bs regs fail)
         (lets ((a b r bs (get3 (cdr bs))))
            (values (list "R["r"]=R["a"]&R["b"];\n") bs
               (put regs r 'fixnum))))

      ; fxior a b r
      (define (cify-fxior bs regs fail)
         (lets ((a b r bs (get3 (cdr bs))))
            (values (list "R["r"]=R["a"]|R["b"];\n") bs
               (put regs r 'fixnum))))

      ; fxxor a b r
      (define (cify-fxxor bs regs fail)
         (lets ((a b r bs (get3 (cdr bs))))
            (values (list "R["r"]=R["a"]^(FMAX<<IPOS&R["b"]);\n") bs
               (put regs r 'fixnum))))

      ; fx* a b l h
      (define (cify-fxmul bs regs fail)
         (lets ((a b l h bs (get4 (cdr bs))))
            (values
               (list "{uint64_t p=(uint64_t)immval(R["a"])*immval(R["b"]);R["l"]=F(p);R["h"]=F(p>>FBITS);}\n")
               bs (put (put regs h 'fixnum) l 'fixnum))))

      ; fx- a b r u
      (define (cify-fxsub bs regs fail)
         (lets ((a b r u bs (get4 (cdr bs))))
            (values
               (list "{hval r=immval(R["a"])-immval(R["b"]);R["u"]=F(r>>FBITS&1);R["r"]=F(r);}\n")
               bs (put (put regs r 'fixnum) u 'bool))))

      ; fx>> x n hi lo
      (define (cify-fxright bs regs fail)
         (lets ((x n hi lo bs (get4 (cdr bs))))
            (values
               (list "{hval x=immval(R["x"]);uint n=immval(R["n"]);R["hi"]=F(x>>n);R["lo"]=F(x<<(FBITS-n));}\n")
               bs (put (put regs lo 'fixnum) hi 'fixnum))))

      ; fxqr ah al b qh ql rem, for (ah<<16 | al) = (qh<<16 | ql)*b + rem
      (define (cify-fxqr bs regs fail)
         (lets ((ah al b qh ql rem bs (get6 (cdr bs))))
            (values
               (list "{uint64_t a=(uint64_t)immval(R["ah"])<<FBITS|immval(R["al"]);hval b=immval(R["b"]);uint64_t q=a/b;R["qh"]=F(q>>FBITS);R["ql"]=F(q);R["rem"]=F(a-q*b);}\n")
               bs (pipe regs (put qh 'fixnum) (put ql 'fixnum) (put rem 'fixnum)))))

      (define (indent i)
         (list->string
            (fold
               (λ (out x) (cons #\space out))
               #null
               (iota 0 1 (* i 2)))))

      ; bind tuple n r0 .. rn
      (define (cify-bind bs regs fail)
         (lets
            ((ob n bs (get2 (cdr bs)))
             (targets bs (split-at bs n)))
            (values
               (ilist
                  "{word*ob=(word*)R["ob"];hval hdr;"
                  (assert-alloc regs ob "IFALSE"
                     (ilist "hdr=*ob;assert_not(rawp(hdr)||objsize(hdr)!="(+ 1 n)",ob,IFALSE);\n"
                        (foldr
                           (λ (n tl) ;; n = (reg . pos)
                              (ilist "R[" (car n) "]=ob[" (cdr n) "];" tl))
                           (list "}\n")
                           (zip cons targets (iota 1 1 (+ n 1)))))))
               bs
               (fold del regs targets))))

      (define (cify-mkt bs regs fail)
         (lets
            ((type sp bs (get2 (cdr bs))) ; object size is nfields + 1, being the header
             (nfields (+ sp 1))
             (fields bs (split-at bs nfields))
             (to bs bs))
            (values
               (ilist "*fp=make_header("(+ nfields 1)","type");"
                  (foldr ; <- fixme: switch to foldr to write in-order
                     (λ (p tl) ; <- (pos . reg)
                        (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                     (list "R[" to "]=(word)fp;fp+=" (+ nfields 1) ";\n")
                     (zip cons (iota 1 1 (+ nfields 1)) fields)))
               bs
               (put regs to 'alloc))))

      (define (cify-closer type)
         (λ (bs regs fail)
            (lets
               ((size litp litoff bs (get3 (cdr bs)))
                (nfields (- size 2)) ;; #[hdr <code> ...]
                (fields bs (split-at bs nfields))
                (to bs bs))
               (values
                  (ilist "*fp=make_header("size","type");fp[1]=G(R["litp"],"litoff");"
                     (fold
                        (λ (tl p) ; <- (pos . reg)
                           (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                        (list "R[" to "]=(word)fp;fp+=" size ";\n")
                        (zip cons (iota 2 1 (+ size 1)) fields)))
                  bs
                  (put regs to 'alloc)))))

      ;; == cify-closer, but with implicit 1 as litp
      (define (cify-closer-1 type)
         (λ (bs regs fail)
            (lets
               ((size litoff bs (get2 (cdr bs)))
                (litp 1)
                (nfields (- size 2)) ;; #[hdr <code> ...]
                (fields bs (split-at bs nfields))
                (to bs bs))
               (values
                  (ilist "*fp=make_header("size","type");fp[1]=G(R["litp"],"litoff");"
                      (fold
                        (λ (tl p) ; <- (pos . reg)
                           (ilist "fp[" (car p) "]=R[" (cdr p) "];" tl))
                        (list "R[" to "]=(word)fp;fp+=" size ";\n")
                        (zip cons (iota 2 1 (+ size 1)) fields)))
                  bs (put regs to 'alloc)))))

      (define (cify-jump-imm val)
         (λ (bs regs fail)
            (lets
               ((a lo8 hi8 bs (get3 (cdr bs)))
                (jump-len (fxior (<< hi8 8) lo8)))
               (values 'branch (tuple (list "R["a"]=="val) (drop bs jump-len) regs bs regs) regs))))

      (define (cify-load-imm val)
         (λ (bs regs fail)
            (lets
               ((hi to bs (get2 (cdr bs)))
                (val (fxior (<< hi 2) val)))
               (values (list "R["to"]=128*"val"+258;\n") bs (put regs to 'fixnum)))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; translator function dispatch ff

      (define translators
         (list->ff
            (list
               (cons 1 ;; indirect-ref from-reg offset to-reg
                  (λ (bs regs fail)
                     (lets ((from offset to bs (get3 (cdr bs))))
                        (values (list "R["to"]=G(R["from"],"offset");\n") bs (del regs to)))))
               (cons 2 ;; fixed jump-arity n hi8 lo8
                  (λ (bs regs fail)
                     (lets
                        ((arity hi8 lo8 bs (get3 (cdr bs)))
                         (jump-len (fxior (<< hi8 8) lo8)))
                         (values 'branch
                           (tuple
                              (list "acc==" arity)
                              bs regs
                              (drop bs jump-len) regs)
                           regs))))
               (cons 3 ;; goto <rator> <nargs>
                  (λ (bs regs fail)
                     (lets ((rator nargs bs (get2 (cdr bs))))
                        (let ((code (list "ob=(word*)R[" rator "];acc=" nargs ";\n" )))
                           (values code #n regs)))))
               (cons 5 ;; move2 from1 to1 from2 to2
                  (λ (bs regs fail)
                     (lets
                        ((from1 to1 bs (get2 (cdr bs)))
                         (from2 to2 bs (get2 bs))
                         (regs (put regs to1 (get regs from1 #false)))
                         (regs (put regs to2 (get regs from2 #false))))
                           (values (list "R["to1"]=R["from1"];R["to2"]=R["from2"];\n") bs regs))))
               (cons 134 (cify-closer "TCLOS"))
               (cons 6 (cify-closer "TPROC"))
               ;(cons 198 (cify-closer-1 "TCLOS"))
               ;(cons 70 (cify-closer-1 "TPROC"))
               (cons 7 ;; eq? a b to
                  (λ (bs regs fail)
                     (lets ((a b to bs (get3 (cdr bs))))
                        (values (list "R["to"]=BOOL(R["a"]==R["b"]);\n") bs regs))))
               (cons 8 ;; jump-if-equal a b lo8 hi8
                  (λ (bs regs fail)
                     (lets
                        ((a b lo8 hi8 bs (get4 (cdr bs)))
                         (jump-len (fxior (<< hi8 8) lo8)))
                        (values 'branch (tuple (list "R[" a "]==R[" b "]") (drop bs jump-len) regs bs regs) regs))))
               (cons 9 ;; move to from
                  (λ (bs regs fail)
                     (lets ((from to bs (get2 (cdr bs))))
                        (cond ;                                                        .--> note, maybe have #false value set
                           (else (values (list "R[" to "]=R[" from "];\n") bs (put regs to (get regs from #false))))))))
               ;; 13=ldi, see higher ops
               (cons 10 (cify-load-imm 0)) ;; ldfix <n> <to>
               (cons 74 (cify-load-imm 1))
               (cons 138 (cify-load-imm 2))
               (cons 202 (cify-load-imm 3))
               ;; 15=type-byte o r
               (cons 15
                  (λ (bs regs fail)
                     (lets ((o r bs (get2 (cdr bs))))
                        (values
                           (list "{word ob=R["o"];if(allocp(ob))ob=V(ob);R["r"]=F((hval)ob>>TPOS&63);}\n")
                           bs
                           (put regs r 'fixnum)))))
               (cons 18 cify-fxand)
               (cons 21 cify-fxsub)
               (cons 22 cify-fxadd)
               (cons 23 cify-mkt)
               (cons 24 ;; ret r == call R3 with 1 argument at Rr
                  (λ (bs regs fail)
                     (let ((res (cadr bs)))
                        (values
                           (list "ob=(word*)R[3];R[3]=R["res"];acc=1;\n") ; the goto apply is automatic
                           #n regs)))) ;; <- always end compiling (another branch may continue here)
               ;(cons 25 ;; jump-variable-arity n
               ;   (λ (bs regs fail)
               ;      (lets
               ;         ((arity hi8 lo8 bs (get3 (cdr bs)))
               ;          (jump-len (fxior (<< hi8 8) lo8)))
               ;          (values 'branch
               ;            (tuple
               ;               (list "acc>=" arity)
               ;               bs regs
               ;               (drop bs jump-len) regs
               ;               ) regs))))
               (cons 26 cify-fxqr)
               (cons 28 cify-sizeb)
               (cons 29 cify-fxior)
               (cons 32 cify-bind)
               (cons 33 cify-fxxor)
               (cons 39 cify-fxmul)
               (cons 44 ;; less a b r
                  (λ (bs regs fail)
                     (lets ((a b to bs (get3 (cdr bs))))
                        (values (list "R["to"]=prim_less(R["a"],R["b"]);\n") bs (put regs to 'bool)))))
               (cons 45 ;; set obj offset value to ;; TODO <- was adding this one
                  (λ (bs regs fail)
                     (lets ((ob pos val to bs (get4 (cdr bs))))
                        (values (list "R["to"]=prim_set(R["ob"],R["pos"],R["val"]);\n") bs
                           (put regs to (get regs ob 'alloc))))))
               (cons 47 cify-ref)
               (cons 51 ;; cons car cdr to
                  (λ (bs regs fail)
                     (lets ((a b to bs (get3 (cdr bs))))
                        (values
                           (list "R["to"]=cons(R["a"],R["b"]);\n")
                           bs (put regs to 'pair)))))
               (cons 57 ;; variable arity
                  (lambda (bs regs fail)
                     (lets ((arity bs (get1 (cdr bs))))
                        (values
                           (list "if(acc==" arity "){R[acc+3]=INULL;}else if(acc>" arity "){word t;for(t=INULL;acc>" arity ";--acc)t=cons(R[acc+2],t);R[acc+3]=t;}else{error(57,ob,INULL);}\n")
                           bs (put regs 'to 'list)))))
               (cons 60 ;; arity n
                  (λ (bs regs fail)
                     (lets
                        ((arity bs (get1 (cdr bs))))
                        (values
                           (list "if(acc!=" arity "){error(60,ob,INULL);}\n")
                           bs regs))))
               (cons 105 ;; car ob to <- use this to test whether the compiler type handling
                  (λ (bs regs fail)
                     (lets
                        ((from to bs (get2 (cdr bs)))
                         (known-type (get regs from #false)))
                        (cond
                           ((eq? 'pair known-type)
                              (values (list "R[" to "]=G(R[" from "],1);\n") bs (del regs to)))
                           ((eq? 'alloc known-type)
                              (values
                                 (list "assert(V(R[" from "])==PAIRHDR,R[" from "],105);R[" to "]=G(R[" from "],1);\n")
                                 bs (del (put regs from 'pair) to))) ;; upgrade to pair
                           (else
                              ;(if known-type (print " >>> car on unknown type <<<" known-type))
                              ;; check that it is a pointer and an object of correct type
                              (values
                                 (list "assert(pairp(R[" from "]),R[" from "],105);R[" to "]=G(R[" from "],1);\n")
                                 bs (del (put regs from 'pair) to)))))))
               (cons 169 ;; cdr ob to
                  (λ (bs regs fail)
                     (lets
                        ((from to bs (get2 (cdr bs)))
                         (known-type (get regs from #false)))
                        (cond
                           ((eq? 'pair known-type)
                              (values (list "R[" to "]=G(R[" from "],2);\n") bs (del regs to)))
                           ((eq? 'alloc known-type)
                              (values
                                 (list "assert(V(R[" from "])==PAIRHDR,R[" from "],169);R[" to "]=G(R[" from "],2);\n")
                                 bs (del (put regs from 'pair) to))) ;; upgrade to pair
                           (else
                              ;; check that it is a pointer and an object of correct type
                              (values
                                 (list "assert(pairp(R[" from "]),R[" from "],169);R[" to "]=G(R[" from "],2);\n")
                                 bs (del (put regs from 'pair) to)))))))
               (cons 16 (cify-jump-imm "F(0)"))
               (cons 80 (cify-jump-imm "INULL"))
               (cons 144 (cify-jump-imm "ITRUE"))
               (cons 208 (cify-jump-imm "IFALSE"))
               (cons 58 cify-fxright)
               (cons 59 cify-lraw)
               (cons 61 ;; arity-fail
                  (λ (bs regs fail)
                     (values (list "error(61,ob,F(acc));\n") #n regs)))
               (cons 63 cify-sysprim)
               ;; below are lower primop + extra info (like 13=ldi<what>)
               (cons 13 ;; ldz r
                  (λ (bs regs fail)
                     (let ((res (cadr bs)))
                        (values (list "R["res"]=F(0);\n") (cddr bs) (put regs res 'fixnum)))))
               (cons 77 ;; ldn r
                  (λ (bs regs fail)
                     (let ((res (cadr bs)))
                        (values (list "R["res"]=INULL;\n") (cddr bs) (put regs res 'null)))))
               (cons 141 ;; ldt r
                  (λ (bs regs fail)
                     (let ((res (cadr bs)))
                        (values (list "R["res"]=ITRUE;\n") (cddr bs) (put regs res 'bool)))))
               (cons 205 ;; ldf r
                  (λ (bs regs fail)
                     (let ((res (cadr bs)))
                        (values (list "R["res"]=IFALSE;\n") (cddr bs) (put regs res 'bool)))))
               )))


      ;; regs is a ff of partial knowledge going downwards about things currently in registers
      ;; → (obs ... . tail)
      (define (emit-c ops regs fail i tail)
         ;(print "emit-c: " (list 'ops ops 'types regs))
         (if (null? ops)
            tail
            (lets
               ((res tl regs ((get translators (car ops) unknown) ops regs fail)))
               (cond
                  ;((eq? res #true) ;; introduce missing local register for writing
                  ;   (let ((reg tl)) ;; needed register
                  ;      (ilist "{word r" reg ";"
                  ;         (emit-c ops (put regs reg reg) fail (cons "}" tail)))))
                  ;((eq? res #false) ;; read the register from vm register array
                  ;   (let ((reg tl))
                  ;      (ilist "{word r" reg "=R[" reg "];"
                  ;         (emit-c ops (put regs reg reg) fail (cons "}" tail)))))
                  ((eq? res 'branch) ; 'branch #(<test> <then-bytecode> <else-bytecode>)
                     (lets ((condition then-bs then-regs else-bs else-regs tl))
                        (ilist
                           (indent i)
                           "if("
                           (append condition
                              (ilist
                                 "){\n"
                                 (emit-c then-bs then-regs fail (+ i 1)
                                    (ilist
                                       (indent i)
                                       "}else{\n"
                                       (emit-c else-bs else-regs fail (+ i 1)
                                          (ilist (indent i) "}\n" tail)))))))))

                  (else ;; instruction compiled, handle the rest
                     (cons
                        (indent i)
                        (append
                           res
                           (emit-c tl regs fail i tail))))))))

      ;; obj extras → #false | (arity . c-code-string), to become #[arity 0 hi8 lo8] + c-code in vm
      (define (compile-to-c code extras)
         (if (bytecode? code)
            (let ((ops (code->bytes code extras)))
               (call/cc
                  (λ (ret)
                     (list->string
                        (foldr render #n
                           (emit-c ops empty (λ () (ret #false)) 2 #n))))))
            #false))
))

; (import (owl cgen))
; (print (compile-to-c (λ (x) x) #null))

