#| doc
Program Serialization

This library writes programs in formats required from the compiler. Programs
can currently be written as FASL-encoded bytecode for use in the VM, or a mixture
of C and FASL when compiling to C.
|#

(define-library (owl compile)

   (export
      make-compiler    ; ((make-compiler extra-insts) entry path opts native)
      dump-fasl
      load-fasl
      suspend)

   (import
      (owl core)
      (owl fasl)
      (owl list)
      (owl sort)
      (owl syscall)
      (owl ff)
      (owl symbol)
      (owl bytevector)
      (owl vector)
      (owl equal)
      (owl function)
      (owl list-extra)
      (owl string)
      (owl io)
      (owl port)
      (owl math)
      (owl tuple) ;; for thread manager
      (owl render)
      (owl lazy)
      (owl regex)
      (only (owl fasl) objects-below)
      (owl eval cgen)
      (only (owl sys) mem-strings)
      (only (owl syscall) error mail exit-owl)
      (only (owl unicode) utf8-decode)
      (only (owl thread) start-thread-controller)
      (only (owl queue) qnull))

   (begin

      ;;;
      ;;; Symbols must be properly interned in a repl.
      ;;;

      (define (symbols-of node)

         (define tag (list 'syms))

         (define (walk trail node)
            (cond
               ((immediate? node) trail)
               ((get trail node #false) trail)
               ((symbol? node)
                  (let ((trail (put trail node 1)))
                     (put trail tag
                        (cons node (get trail tag #n)))))
               ((raw? node) trail)
               (else
                  (fold walk
                     (put trail node #true)
                     (object->list node)))))
         (define trail
            (walk (put empty tag #n) node))

         (get
            (walk (put empty tag #n) node)
            tag #n))

      (define (file->string path)
         (bytes->string
            (vec-iter
               (let ((vec (file->vector path)))
                  (if vec
                     vec
                     (error "Unable to load: " path))))))

      (define rts-source
         (list->bytevector
            (file->list "c/_vm.c")))

      ; str -> str' | #false
      (define (utf8-decode-string str)
         (let ((cps (utf8-decode (lfoldr cons '() (str-iterr str)))))
            (if cps
               (list->string cps)
               (begin
                  ;; don't use normal IO, since it may not yet be running.
                  (system-println "warning: bad UTF-8 in command line argument")
                  ;; return the string although it has broken data. this allows
                  ;; paths with broken (or intentional) funny encodings to be
                  ;; passed as command line arguments.
                  str))))

      (define (with-decoded-args prog)
         (λ (vm-args)
            (prog
               (map utf8-decode-string vm-args))))

      (define (width x)
         (cond
            ((< x 10) 2)
            ((< x 100) 3)
            (else 4)))

      (define (render-byte-array bytes pos)
         (cond
            ((> pos 75)
               (cons 10
                  (render-byte-array bytes 0)))
            ((null? (cdr bytes))
               (render (car bytes) #n))
            (else
               (let ((this (car bytes)))
                  (render this
                     (cons 44
                        (render-byte-array (cdr bytes) (+ pos (width this)))))))))

      (define (dump-data data path)
         (let ((port (open-output-file path)))
            (if port
               (let loop ((data data))
                  (cond
                     ((null? data)
                        (close-port port)
                        #true)
                     ((pair? data)
                        (if (write-bytevector (car data) port)
                           (loop (cdr data))
                           #false))
                     (else
                        (loop (data)))))
               #false)))

      (define (dump-fasl obj path)
         (dump-data (fasl-encode-stream obj self) path))

      (define (load-fasl path fval)
         (let ((port (open-input-file path)))
            (if port
               (let ((val (fasl-decode (port->byte-stream port) fval)))
                  (close-port port)
                  val)
               fval)))

      (define (render-native-ops nops)
         (runes->string
            (foldr render #n
               (ff-fold
                  (λ (tl func info)
                     (lets ((opcode new-func c-code <- info))
                        ;; render code if there (shared users do not have it)
                        (if c-code
                           ;; all of these end to an implicit goto apply
                           (ilist "   case " opcode ":\n" c-code "     break;\n" tl)
                           tl)))
                  #n nops))))


      ; nodes = ((func . #(opcode warpper src)) ...)

      ; obj → (ff of #[bytecode] → #(native-opcode native-using-bytecode c-fragment|#false))
      (define (choose-native-ops entry extras)
         (let ((all (objects-below entry)))
            (if (null? all)
               (begin
                  ;(print " - no native instructions selected")
                  (list->ff all))
               (let loop ((code 0) (obs all) (out #n)) ;; <- can start at 0 after cleaning up the old code
                  (cond
                     ((null? obs)
                        (list->ff out))
                     ((= code 65536)
                        ;; would need a larger wrappers, but will not likely be necessary
                        ;; could be added as (+ (<< 1 6) 0) -> read 4 bytes
                        (error "too many native opcodes."
                           "report this as an issue if this happens for a real program."))
                     ((compile-to-c (car obs) extras) =>
                        (λ (src)
                           (lets
                              ((wrapper (raw (list 0 (>> code 8) (band code 255)) type-bytecode)))
                              (loop (+ code 1) (cdr obs)
                                 (cons (cons (car obs) (prod code wrapper src)) out)))))
                     (else
                        (loop code (cdr obs) out)))))))

      ; obj -> fixnum|#false
      (define (extended-opcode obj)
         (if (and (bytecode? obj) (eq? 0 (ref obj 0)))
            (+ (<< (ref obj 1) 8) (ref obj 2))
            #false))

      (define (show-func val)
         (cons 'bytecode (bytevector->list val)))

      ; native-ops → (obj → obj')
      (define (make-native-cook native-ops extras)
         (λ (obj)
            (cond
               ;; if chosen to be a macro instruction in the new vm, replace with new bytecode calling it
               ;; write a reference to the wrapper function instead of the original bytecode
               ((get native-ops obj #false) =>
                  (lambda (info)
                     (lets ((op func c <- info)) func)))
               ;; if this is a macro instruction in the current system, convert back
               ;; to vanilla bytecode, or the target machine won't understand this
               ((extended-opcode obj) =>
                  (λ (opcode)
                     ;(print " * mapping superinstruction back to to bytecode: " opcode)
                     (or (get extras opcode #false)
                        (error "could not find bytecode for opcode " opcode))))
               (else obj))))

      ;; make a ff of opcode → original-bytecode. for example the repl
      ;; needs to know what the plain bytecode of each compiled version is in
      ;; order to for example build a new vm with possibly other set of native ops.

      (define (clone-code bc extras) ;; clone to not be eq? with the ones being compiled
         (cond
            ((extended-opcode bc) =>
               ; the opcodes must be described with vanilla bytecode
               ; this does not belong here...
               (λ (opcode)
                  (let ((original (get extras opcode #false)))
                     (if original
                        (clone-code original extras)
                        (error "bug: no original code found for superinstruction " opcode)))))
            (else
               (let ((bytes (bytevector->list bc)))
                  (if (eq? (cadr bytes) 0)
                     (error "bug: vm speciazation instruction probably referencing code from current vm: " bytes))
                  (raw bytes type-bytecode))))) ; <- reallocate it

      (define (original-sources native-ops extras)
         (ff-fold
            (λ (sources bytecode info)
               (lets ((opcode wrapper c-code <- info))
                  (put sources opcode
                     (clone-code bytecode extras))))
            empty native-ops))

      ;; _ → ((bytecode . bytecode) ...)
      (define (codes-of obj)
         (map (lambda (x) (cons x x))
            (keep bytecode?
               (objects-below obj))))

      ;; todo: move with-threading to lib-threads and import from there
      (define (with-threading ob)
         (λ (args)
            (start-thread-controller
               (list
                  (tuple 'root
                     (λ ()
                        (start-base-threads)    ;; get basic io running
                        (exit-owl (ob args))))) ;; exit thread scheduler with exit value of this thread (if it doesn't crash)
               (list
                  (cons 'root qnull)))))   ;; the init thread usually needs a mailbox

      (define (cook-format str)
         (cond
            ((equal? str "c") 'c)
            ((equal? str "fasl") 'fasl)
            (else #false)))

      (define suffix-only (string->regex "s/^.*\\.([a-z]+)$/\\1/"))

      ; → c | fasl (| s)
      (define (choose-output-format opts maybe-path)
         (lets ((path (get opts 'output maybe-path)))
            (if (string? path)
               (cook-format (suffix-only path))
               #false)))

      (define owl-ohai-resume "Welcome back.")

      ; path -> 'loaded | 'saved
      (define (suspend path)
         (let ((maybe-world (syscall 16 #true #true)))
            (if (eq? maybe-world 'resumed)
               owl-ohai-resume
               (begin
                  (dump-fasl maybe-world path)
                  'saved))))

      (define with-args
         (C B mem-strings))

      ; obj → (ff of #[bytecode] → #(native-opcode native-using-bytecode c-fragment))
      ; dump entry object to path, or stdout if path is "-"

      (define (make-compiler extras)
         (λ (entry path opts native . custom-runtime) ; <- this is the usual compile-owl
            (lets
               ((path (get opts 'output "-")) ; <- path argument deprecated
                (mode (get opts 'mode 'program)) ;; 'program | 'library | 'plain
                (mode (if (get opts 'bare) 'plain mode))

                (format
                  ;; use given format (if valid) or choose using output file suffix
                  (or (cook-format (get opts 'output-format #false))
                     (choose-output-format opts path)))

                (entry ;; all non-plain outputs include thread manager
                  (if (eq? mode 'plain)
                     entry
                     (with-threading entry)))

                (entry ;; pass symbols to entry if requested (repls need this)
                  (if (get opts 'want-symbols #false)
                     (entry (symbols-of entry))
                     entry))

                (entry ;; pass code vectors to entry if requested (repls need this)
                  (if (get opts 'want-codes #false)
                     (entry (codes-of entry)) ;; <- gets ((code . code) ...), removable?
                     entry))

                (native-ops ;; choose which bytecode vectors to add as extended vm instructions
                  (choose-native-ops (if (get opts 'native #false) entry native) extras))

                (entry ;; possibly tell the entry function about extended opcodes
                  (if (get opts 'want-native-ops #false)
                     ;; entry is actually ((ff of extended-opcode → vanilla-bytecode) → entry)
                     (entry (original-sources native-ops extras))
                     entry))

                (entry ;; possibly add code to utf-8 decode command line arguments
                  (if (eq? mode 'program)
                     (with-decoded-args entry)
                     entry)) ;; must be pulled

                (entry ;; pull command line args to owl from **argv
                   (if (eq? mode 'program)
                      (with-args entry)
                      entry))

                (native-cook ;; make a function to possibly rewrite bytecode during save (usually to native code wrappers)
                   (make-native-cook native-ops extras))

                (bytes ;; encode the resulting object for saving in some form
                  (fasl-encode-cooked entry native-cook))

                (runtime
                   (if (and (pair? custom-runtime) (car custom-runtime))
                      (car custom-runtime)
                      rts-source))

                (port ;; where to save the result
                  (if (equal? path "-")
                     stdout
                     (open-output-file path))))
               (cond
                  ((not port)
                     (print "Could not open path for writing")
                     #false)
                  ((not format)
                     (print "I do not know how to write that.")
                     (print "Use -o file.c, -o file.fasl, or defined format with -x c or -x fasl")
                     #false)
                  ((eq? format 'fasl) ;; just save the fasl dump
                     (if (write-bytes port bytes)
                        (close-port port)
                        #false))
                  ((eq? format 'c)
                     (write-bytes port ;; output fasl-encoded heap as an array
                        (append
                           (string->bytes "static const unsigned char heap[] = {")
                           (render-byte-array bytes 24)))
                     ;; dump also a fasl if requested
                     (write-bytes port (string->bytes "};\n"))
                     ;; dump ovm.c and replace /*AUTOGENERATED INSTRUCTIONS*/ with new native ops (if any)
                     (write-bytes port
                        (string->bytes
                           (str-replace
                              (list->string (vector->list runtime))
                              "/*AUTOGENERATED INSTRUCTIONS*/"
                              (render-native-ops native-ops))))

                     ;; done, now just gcc -O2 -o foo <path>
                     (close-port port))))))
))
