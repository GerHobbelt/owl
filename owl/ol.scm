#| Owl Entry

This file is the program that ends up being the ol binary. 

Typically such simple command line program would consist of a few definitions 
and library imports, eventually followed by the function of command line
arguments that is the actual entry to the program.

A few complications arise from the fact that we are dealing with a compiler
intended to compile itself.Most essentially, any regular dependencies left from
the existing compiler to the next version of itself will cause a snowballing
effect. Each new version of the compiler would have a larger proportion of old
legacy code with it, leading to increasingly large compilers. Such dependencies 
typically include things like addition, which would bring in the previous version 
of bignum arithmetic.

This, and some other selfcompilation issues, are mostly avoided or detected
by dropping most definitions and existing libraries at the beginning of this
file. This way most code must be freshly loaded.

Each compilation of a new version of owl will additionally use new version to
repeatedly compile itself until a version is reached which compiles an exact
replica of itself from the same source code. This ensures that all changes have
propagated everywhere and there cannot be any snowballing effects due to
accidental dependencies.

|#

#| Copyright (c) Aki Helin
 |
 | Permission is hereby granted, free of charge, to any person obtaining a
 | copy of this software and associated documentation files (the "Software"),
 | to deal in the Software without restriction, including without limitation
 | the rights to use, copy, modify, merge, publish, distribute, sublicense,
 | and/or sell copies of the Software, and to permit persons to whom the
 | Software is furnished to do so, subject to the following conditions
 |
 | The above copyright notice and this permission notice shall be included
 | in all copies or substantial portions of the Software.
 |
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 | DEALINGS IN THE SOFTWARE.
 |#

;; ------------------------------- 8< -------------------------------

;; Cleanup phase

;; reload owl/core.scm to replace (owl core) of the current version

,load "owl/core.scm"

(mail 'intern (tuple 'flush)) ;; ask symbol interner to forget everything

(define *libraries* #n) ;; clear loaded libraries

(import (owl core))   ;; reload default macros needed for defining libraries etc

;; forget everything except these and core values (later list also them explicitly)

,forget-all-but (quote *libraries* _branch _define rlambda)

;; --------------------------------------------------------------------------------

;; Reload phase

;; get define, define-library, etc from the new (owl core)
(import (owl core))

(define *interactive* #false)  ;; be silent
(define *include-dirs* '(".")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-version* "0.2.1a")

(import
   (owl intern)
   (owl eval env)
   (owl eval ast)
   (owl thread)
   (owl args)
   (owl alist)
   (only (owl compile) make-compiler load-fasl)
   (owl eval)
   (owl eval data)
   (owl repl)
   (owl toplevel)
   (owl variable)
   (owl ff)
   (prefix (owl sys) sys-))

;; implementation features, used by cond-expand
(define *features*
   (cons
      (string->symbol (string-append "owl-lisp-" *owl-version*))
      '(owl-lisp r7rs exact-closed ratios exact-complex full-unicode immutable)))

(define (path->string path)
   (let ((data (file->vector path)))
      (if data
         (bytes->string (vector->list data))
         #false)))

(define (choose-mode s)
   (cond
      ((equal? s "program") 'program) ;; threads and IO, handle command line arguments
      ((equal? s "library") 'library) ;; threads and IO, nothing else
      ((equal? s "plain")   'plain)   ;; just the program
      (else #false)))

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (about    "-a" "--about")
       (version  "-v" "--version")
       (evaluate "-e" "--eval"     has-arg comment "evaluate given expression and print result")
       (test     "-t" "--test"     has-arg comment "evaluate given expression exit with 0 unless the result is #false")
       (quiet    "-q" "--quiet"    comment "be quiet (default in non-interactive mode)")
       (run      "-r" "--run"      has-arg comment "run the last value of the given foo.scm with given arguments" terminal)
       (include  "-i" "--include"  has-arg comment "extra directory to load libraries from" plural)
       (load     "-l" "--load"     has-arg comment "resume execution of a saved program state saved with suspend")
       (output   "-o" "--output"   has-arg comment "where to put compiler output (default auto)")
       (output-format  "-x" "--output-format"   has-arg comment "output format when compiling (default auto)")
       (optimize "-O" "--optimize" cook ,string->number comment "optimization level in C-compilation (0-2)")
       (custom-runtime "-C" "--runtime"
          cook ,path->string
          comment "use a custom runtime in C compilation")
       ;(debug    "-d" "--debug" comment "Define *debug* at toplevel verbose compilation")
       (mode     "-m" "--mode"    cook ,choose-mode
          default "program"
          comment "output wrapping: program, library, plain")
       (no-readline #f  "--no-readline" comment "disable builtin line editor")
       )))

(define brief-usage-text "Usage: ol [args] [file] ...")

(define error-usage-text "ol -h helps.")

(define with-c-suffix
   (string->regex "s/\\.[a-zA-Z0-0]+$/.c/"))

(define (c-source-name path)
   (let ((new (with-c-suffix path)))
      (if (string=? path new)
         (string-append path ".c")
         new)))

;; outcome args path -> program-exit-value | nonzero-on-error
(define (owl-run outcome args path)
   (success outcome
      ((ok val env)
         ;; be silent when all is ok
         ;; exit with 126 and have error message go to stderr when the run crashes
         (try (λ () (val args)) 126))
      ((fail why)
         (print-repl-error
            (list "ol: cannot run" path "because there was an error during loading:" why))
         2)))

(define about-owl
"Owl Lisp -- a functional scheme
Copyright (c) Aki Helin
Check out https://haltp.org/owl for more information.")

(define usual-suspects
   (list
         put get del ff-fold fupd
         - + * /
         quotient gcd
         << < <= = >= > >>
         equal? memq member
         band bior bxor
         ; sort
         ; suffix-array bisect
         fold foldr map reverse length zip append
         list-ref lset iota
         ;print
         mail interact
         take filter remove
         thread-controller
         uncons lfold lmap
         rand seed->rands
         (fold (λ (ff x) (put ff x x)) empty (iota 0 1 100)) ;; all kinds of nodes
         ))

;; handles $ ol -c stuff
(define (repl-compile compiler env path opts)
   (try
      (λ ()
         ;; evaluate in a thread to catch error messages here
         (let ((outcome (if (equal? path "-") (repl-port env stdin) (repl-file env path))))
            (success outcome
               ((ok val env)
                  (if (function? val)
                     (begin
                        (compiler val

                           ;; output path
                           (cond
                              ((get opts 'output #false) => self) ; requested with -o
                              ((equal? path "-") path) ; stdin → stdout
                              (else (c-source-name path)))

                           opts

                           ;; to be customizable via command line opts
                           (let ((opt (abs (get opts 'optimize 0))))
                              (cond
                                 ((>= opt 2) val) ;; compile everything to native extended primops for -O2
                                 ((= opt 1) usual-suspects) ;; compile some if -O1
                                 (else #false))) ;; otherwise use bytecode and plain vm

                           (get opts 'custom-runtime))

                           0)
                     (begin
                        (print "The last value should be a function of one value (the command line arguments), but it is instead " val)
                        2)))
               ((fail reason)
                  (print-repl-error
                     (list "Cannot compile" path "because " reason))
                  2))))
      #false))

(define (try-load-state path args)
   (let ((val (load-fasl path #false)))
      (if (function? val)
         (try (λ () (val (cons path args))) 126)
         (begin
            (print "failed to load dump from " path)
            1))))

;; -> vm exit with 0 on success, n>0 on error
(define (try-repl-string env str)
   (success (repl-string env str)
      ((ok val env)
         (exit-owl
            (if (print val) 0 126)))
      ((fail why)
         (print-repl-error
            (list "An error occurred while evaluating:" str ": " why))
         (exit-owl 1))))

;; exit with 0 if value is non-false, 1 if it's false, 126 if error
(define (try-test-string env str)
   (success (repl-string env str)
      ((ok val env)
         (exit-owl (if val 0 1)))
      ((fail why)
         (print-repl-error
            (list "An error occurred while evaluating:" str ": " why))
         (exit-owl 126))))

(define owl-ohai "You see a prompt.")

;; say hi if interactive mode and fail if cannot do so (the rest are done using
;; repl-prompt. this should too, actually)
(define (greeting env)
   (if (env-get env '*interactive* #f)
      (or
         (and
            (print owl-ohai)
            (display "> "))
         (halt 126))))


; ... → program rval going to exit-owl
(define (repl-start vm-args repl compiler env)
   (or
      (process-arguments (cdr vm-args) command-line-rules error-usage-text
         (λ (dict others)
            (lets
               ((env ;; be quiet automatically if any of these are set
                  (if (fold (λ (is this) (or is (get dict this #false))) #false '(quiet test evaluate run output output-format))
                     (env-set env '*interactive* #false)
                     (env-set env '*interactive* #true)))
                (env ;; maybe set debug causing (owl eval) to print intermediate steps
                  (if (get dict 'debug)
                     (env-set env '*debug* #true)
                     env))
                (env ;; add -i include directories, if any
                   (env-set env '*include-dirs*
                      (append (get dict 'include #null) (env-get env '*include-dirs* #null)))))
               (cond
                  ((get dict 'help)
                     (print brief-usage-text)
                     (print-rules command-line-rules)
                     0)
                  ((get dict 'version)
                     (print "Owl Lisp " *owl-version*)
                     0)
                  ((get dict 'about) (print about-owl) 0)
                  ((get dict 'load) => (C try-load-state others))
                  ((or (get dict 'output) (get dict 'output-format))
                     (if (< (length others) 2) ;; can take just one file or stdin
                        (repl-compile compiler env
                           (if (null? others) "-" (car others)) dict)
                        (begin
                           (print "compile just one file for now please: " others)
                           1)))
                  ((get dict 'run) =>
                     (λ (path)
                        (owl-run
                           (try (λ () (repl-file env path)) (fail "crash"))
                           (cons "ol" others) path)))
                  ((get dict 'evaluate) => (H try-repl-string env))
                  ((get dict 'test) => (H try-test-string env))
                  ((null? others)
                     (greeting env)
                     (repl-ui
                        (env-set env '*readline*
                           (if (get dict 'no-readline)
                              #false
                              (sys-isatty stdin)))))
                  (else
                     ;; load the given files
                     (define input
                        (foldr (λ (path tail) (ilist ',load path tail)) #n others))
                     (success (repl (env-set env '*interactive* #false) input)
                        ((ok val env)
                           0)
                        ((fail reason)
                           (print-repl-error reason)
                           1)))))))
      2))

(define (directory-of path)
   (runes->string
      (reverse
         (or
            (memq #\/ (reverse (string->runes path)))
            #n))))

(define compiler ; <- to compile things out of the currently running repl using the freshly loaded compiler
   (make-compiler empty))

(define initial-environment
   (bind-toplevel
      (env-fold env-put-raw
         *owl-kernel*
         (alget *libraries* '(owl toplevel) #f))))

(define (heap-entry symbol-list)
   (λ (codes) ;; all my codes are belong to codes
      (lets
         ((interner-thunk (initialize-interner symbol-list codes)))
         (λ (vm-special-ops)
            (let ((compiler (make-compiler vm-special-ops)))
               ;; still running in the boostrapping system
               ;; the next value after evaluation will be the new repl heap
               (λ (vm-args)
                  ;; now we're running in the new repl
                  (start-thread-controller
                     (list
                        (tuple 'init
                           (λ ()
                              (thread 'repl
                                 (let ((state (make-variable '*state* empty)))
                                    ;; get basic io running
                                    (start-base-threads)

                                    ;; store initial state values
                                    (state 'call
                                       (λ (st)
                                          (pipe st
                                             (put 'command-line-arguments vm-args)
                                             (put 'features *features*)
                                             )))

                                    ;; repl needs symbol etc interning, which is handled by this thread
                                    (thunk->thread 'intern interner-thunk)

                                    ;; set a signal handler, which stops current evaluation thread instead of owl
                                    ;; if a repl eval thread is running
                                    (sys-catch-signals (list sys-sigint)) ;; C-c
                                    (set-signal-action signal-handler/repl)

                                    (exit-owl
                                       (repl-start vm-args repl compiler
                                          (fold
                                             (λ (env defn)
                                                (env-set env (car defn) (cdr defn)))
                                             initial-environment
                                             (list
                                                (cons '*owl* (directory-of (car vm-args)))
                                                (cons '*args* vm-args)
                                                (cons '*features* *features*)
                                                (cons '*include-dirs* *include-dirs*)
                                                (cons '*libraries* *libraries*)
                                                (cons 'dump compiler)
                                                (cons '*owl-version* *owl-version*)
                                                (cons 'eval exported-eval)
                                                (cons 'render render)
                                                (cons '*vm-special-ops* vm-special-ops)
                                                (cons '*state* state))))))))))
                     #n)))))))


(define command-line-rules
   (cl-rules
      `((output "-o" "--output" has-arg comment "output path")
        (specialize "-s" "--specialize" has-arg comment "vm extensions (none, some, all)"))))

(define (choose-natives str all)
   (cond
      ((equal? str "none") #n)
      ((equal? str "some") usual-suspects)
      ((equal? str "all") all)
      (else (print "Bad native selection: " str))))

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extra)
         (let ((opts opts))
            (cond
               ((null? extra)
                  (compiler heap-entry "unused historical thingy"
                     (list->ff
                        `((output . ,(get opts 'output 'bug))
                          (want-symbols . #true)
                          (want-codes . #true)
                          (want-native-ops . #true)))
                     (choose-natives
                        (get opts 'specialize "none")
                        heap-entry))
                  0)
               (else
                  (print "Unknown arguments: " extra)
                  1))))))
