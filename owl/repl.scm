
(define-library (owl repl)

   (export
      repl-file
      repl-port
      repl-string
      repl-ui
      repl
      print-repl-error
      bind-toplevel
      *owl-kernel*)

   (import
      (owl core)
      (owl list)
      (owl eval)
      (owl ff)
      (owl sort)
      (owl eval env)
      (owl eval data)
      (owl io)
      (owl port)
      (owl time)
      (owl alist)
      (owl list-extra)
      (owl render)
      (owl string)
      (owl sexp)
      (owl symbol)
      (owl math integer)
      (owl syntax-rules)
      (only (owl sys) get-heap-bytes-written) ;; <- could be moved to some profiling
      (only (owl readline) port->readline-byte-stream)
      (only (owl parse) fd->exp-stream byte-stream->exp-stream file->exp-stream try-parse silent-syntax-fail resuming-syntax-fail)
      (prefix (only (owl parse) plus) get-kleene-)
      (owl function)
      (owl lazy)
      (owl macro)
      (only (owl metric) format-time format-number)
      (only (owl thread) try-thunk)
      (only (owl regex) string->regex)
      (only (owl compile) suspend)
      (scheme cxr)
      (scheme write)
      (scheme base))

   (begin

      ;; library (just the value of) containing only special forms, primops and
      (define *owl-kernel*
         (fold
            (λ (env op)
               (env-set env (ref op 1) (ref op 5)))
            (env-set-macro
               *tabula-rasa* ;; from (owl env), env with only special form tags, no primops
               'define-syntax
               new-define-syntax-transformer)
            primops))

      (define (? x) #true)

      ;; toplevel variable to which loaded libraries are added
      (define library-key '*libraries*)     ;; list of loaded libraries
      (define features-key '*features*)     ;; list of implementation feature symbols
      (define includes-key '*include-dirs*) ;; paths where to try to load includes from

      (define definition?
         (H match (list '_define symbol? ?)))

      (define macro-definition?
         (H match (list '_define-macro symbol? ?)))

      (define multi-definition?
         (H match (list '_define list? ?)))

      ;; toplevel variable which holds currently loaded (r7rs-style) libraries
      (define libraries-var '*libs*)

      (define error-port stderr)

      (define (print-repl-error lst)
         (define (format-error lst ind)
            (cond
               ((and (pair? lst) (null? (cdr lst)) (list? (car lst)))
                  (cons 10
                     (let ((ind (+ ind 2)))
                        (append (make-list ind 32) (format-error (car lst) ind)))))
               ((pair? lst)
                  (render (car lst)
                     (cons 32
                        (format-error (cdr lst) ind))))
               ((null? lst) '(10))
               (else (render lst '(10)))))
         (write-bytes error-port
            (format-error lst 0)))

      (define repl-op?
         (H match (list 'unquote symbol?)))

      (define (mark-loaded env path)
         (let ((loaded (env-get env '*loaded* #n)))
            (if (member path loaded)
               env
               (env-set env '*loaded*
                  (cons path loaded)))))

      ;; values used by the repl to signal they should be printed as such, not rendered as a value
      (define repl-message-tag "foo")
      (define (repl-message . args) (cons repl-message-tag args))
      (define (repl-message? foo) (and (pair? foo) (eq? repl-message-tag (car foo))))

      (define (maybe-prompt env)
         (and (env-get env '*interactive* #f)
              (or (display "> ")
                  (halt 125)))) ;; stdout in bad shape

      ;; render the value if *interactive*, and print as such (or not at all) if it is a repl-message
      ;; if interactive mode and output fails, the error is fatal
      (define (prompt env val)
         (if (env-get env '*interactive* #false)
            (if (repl-message? val)
               (begin
                  (for-each print (cdr val))
                  (maybe-prompt env))
               (begin
                  ((writer-to empty) stdout val)
                  (print)
                  (maybe-prompt env)))))

      (define (repl-time thunk)
         (try-thunk
            (lambda ()
               (lets
                  ((now (time-ns))
                   (alloc-pre (get-heap-bytes-written))
                   (val (thunk)) ;; <- assumes just one returned value for now
                   (elapsed-ns (- (time-ns) now))
                   (alloc-post (get-heap-bytes-written)))
                  (print ";; heap " (format-number (- alloc-post alloc-pre)))
                  (print ";; time " (format-time elapsed-ns))))
            (lambda (error)
               (print ";; evaluation failed"))
            (list 'repl-eval)))

      ;; regex-fn | string | symbol → regex-fn | #false
      (define (thing->rex thing)
         (cond
            ((function? thing) thing)
            ((string? thing)
               (string->regex (string-append "m/" thing "/")))
            ((symbol? thing)
               (thing->rex (symbol->string thing)))
            (else #false)))

      (define repl-ops-help "Commands:
   ,help             - show this
   ,words            - list all current definitions
   ,expand <expr>    - expand macros in the expression
   ,find [regex|sym] - list all defined words matching regex or m/<sym>/
   ,load string      - (re)load a file
   ,time exp         - time evaluation
   ,libraries        - show all currently loaded libraries
   ,save <path>      - save current heap, restart with $ ol -l <path>
   ,quit             - exit owl")

      (define (symbols? exp)
         (and (list? exp) (every symbol? exp)))

      ;; repl-cont is env, input -> program-exit, directly or via tail call to repl-cont
      (define (repl-load repl-cont repl-exps path in env)
         (lets
            ((exps ;; find the file to read
               (or
                  (file->exp-stream path sexp-parser (silent-syntax-fail #n))
                  (file->exp-stream
                     (string-append (env-get env '*owl* "NA") path)
                     sexp-parser
                     (silent-syntax-fail #n)))))
            (if exps
               (lets
                  ((interactive? (env-get env '*interactive* #false)) ; <- switch prompt during loading
                   (load-env
                     (if prompt
                        (env-set env '*interactive* #false) ;; <- switch prompt during load (if enabled)
                        env))
                   (outcome (repl-exps load-env exps)))
                  (success outcome
                     ((ok val env)
                        (let ((env (env-set env '*interactive* interactive?)))
                           (prompt env (repl-message (str ";; loaded " path)))
                           (repl-cont env  in)))
                     ((fail reason)
                        (print-repl-error
                           (list "Could not load" path "because" reason))
                        (maybe-prompt env)
                        ;; loading results in termination with a non-zero value if
                        ;; the session is not interactive
                        (if interactive?
                           (repl-cont env in)
                           10))))
               (begin
                  (prompt env
                     (repl-message (str ";; failed to find file " path)))
                  (repl-cont env in)))))

      (define (repl-op repl-cont repl op in env)
         (case op
            ((help)
               (prompt env (repl-message repl-ops-help))
               (repl-cont env in))
            ((load)
               (lets ((op in (uncons in #false)))
                  (if (string? op)
                     (repl-load repl-cont repl op in env)
                     (begin
                        (print ";; expected a string argumnet")
                        (repl-cont env in)))))
            ((forget-all-but)
               (lets ((op in (uncons in #false)))
                  (if (symbols? op)
                     (let ((nan (tuple 'defined (tuple 'value 'undefined))))
                        (repl-cont
                           (env-keep env
                              (λ (name)
                                 (if (or (primop-of name) (memq name op))
                                    name
                                    #false)))
                           in))
                     (fail (list "bad word list: " op)))))
            ((words w)
               (prompt env
                  (repl-message
                     (bytes->string
                        (foldr
                           (λ (x tl) (render x (cons #\space tl)))
                           #n
                           (cons "Words: "
                              (sort string<?
                                 (map symbol->string
                                    (env-keys env))))))))
               (repl-cont env in))
            ((find)
               (lets
                  ((thing in (uncons in #false))
                   (rex (thing->rex thing)))
                  (cond
                     ((function? rex)
                        (define (seek env)
                           (filter (B rex symbol->string) (env-keys env)))
                        (print "current toplevel: "
                           (fold str "" (interleave ", " (seek env))))
                        (for-each
                           (λ (lib)
                              (let ((matches (seek (cdr lib))))
                                 (if (not (null? matches))
                                    (print
                                       (str "   " (car lib) ": " (fold str "" (interleave ", " matches)))))))
                           (env-get env '*libraries* #n))
                        (prompt env (repl-message)))
                     (else
                        (prompt env "I would have preferred a regex or a symbol.")))
                  (repl-cont env in)))
            ((libraries libs)
               (print "Currently defined libraries:")
               (for-each print (map car (env-get env library-key #n)))
               (prompt env (repl-message))
               (repl-cont env in))
            ((expand)
               (lets ((exp in (uncons in #false)))
                  (lets/cc ret
                     ((abort (λ (why) (print ";; expansion failed: " exp) (ret #false)))
                      (env exp (macro-expand exp env abort)))
                     (print ";; " exp))
                  (prompt env (repl-message))
                  (repl-cont env in)))
            ((time)
               (lets ((exp in (uncons in #false))
                      (exp (list 'lambda '() exp))) ;; thunk it
                  (success (evaluate exp env)
                     ((ok val envp)
                        (repl-time val)
                        (prompt env (repl-message))
                        (repl-cont env in))
                     ((fail why) ;; <- actually goes boom. repl-time should run in a thread.
                        (print ";; failed to evaluate expression")
                        (prompt env (repl-message))
                        (repl-cont env in)))))
            ((save)
               (lets ((path in (uncons in #false)))
                  (if (string? path)
                     (begin
                        ;; this captures a continuation of the whole system and exits with
                        ;; "saved" to dumping process and "Welcome back" when the serialized
                        ;; heap is started in the future
                        (prompt env (repl-message (suspend path)))

                        (repl-cont env in))
                     (begin
                        (prompt env "Usage: ,save \"file.suffix\"")
                        (repl-cont env in)))))
            ((quit)
               0)
            (else
               (prompt env
                  (repl-message
                     (str ";; unknown repl op: " op ". use ,help for help.")))
               (repl-cont env in))))

      ;; → (name ...) | #false
      (define (exported-names env lib-name)
         (let ((libp (alget (env-get env library-key #n) lib-name #f)))
            (if libp
               (env-fold (λ (out name value) (cons name out)) #n libp)
               #false)))

      ;; todo: this uses direct environment access - move to lib-env or handle here?
      ;; <export spec> = <identifier>
      ;;               | (rename <identifier_1> <identifier_2>)
      ;;               | (exports <lib)
      ;; TODO - use env-keep and check bindings from result instead to avoid absraction violation
      (define (build-export names env fail)
         (let loop ((names names) (unbound #n) (module empty-env))
            (cond
               ((null? names)
                  (cond
                     ((null? unbound) module)
                     ((null? (cdr unbound))
                        (fail (list "Undefined exported value: " (car unbound))))
                     (else
                        (fail (list "Undefined exports: " unbound)))))
               ((env-get-raw env (car names) #false) =>
                  (λ (value)
                     (loop (cdr names) unbound (env-put-raw module (car names) value))))
               ((and ;; swap name for (rename <local> <exported>)
                   (match `(rename ,symbol? ,symbol?) (car names))
                   (env-get-raw env (cadar names) #false)) =>
                  (λ (value)
                     (loop (cdr names) unbound (env-put-raw module (caddar names) value))))
               ((match `(exports ,list?) (car names))
                  (let ((exported (exported-names env (cadr (car names)))))
                     (if exported
                        (loop (append exported (cdr names)) unbound module)
                        (fail (list "Didn't find " (cadar names) " for exporting.")))))
               (else
                  (loop (cdr names) (cons (car names) unbound) module)))))

      ; fixme, use pattern matching...

      (define export?
         (H match `(export . ,symbols?)))

      (define (_ x) #true)

      (define import?  ; toplevel import using the new library system
         (H match `(import . ,(λ (x) #true))))

      (define (library-definition? x)
         (and (pair? x) (list? x) (eq? (car x) '_define-library)))

      ;; used to get definiton of *toplevel*, mainly for use with eval
      (define (bind-toplevel env)
         (env-set env '*toplevel*
            (env-del env '*toplevel*)))

      ;; list starting with val?
      (define (headed? val exp)
         (and (pair? exp) (eq? val (car exp)) (list? exp)))

      ;; (import <import set> ...)
      ;; <import set> = <library name>
      ;;              | (only <import set> <identifier> ...)
      ;;              | (except <import set> <identifier> ...)
      ;;              | (prefix <import set> <identifier>)
      ;;              | (rename <import set_1> (<identifier_a> <identifier_b>) ..)

      ;; ((a b) ...)
      (define (pairs? exp)
         (and (list? exp)
            (every (λ (x) (and (list? x) (= (length x) 2))) exp)))

      ;; → 'ok env | 'needed name | 'circular name, non-ok exists via fail
      (define (import-set->library iset libs fail)
         (cond
            ((alget libs iset #f) =>
               (λ (val)
                  (if (eq? val 'loading) ;; trying to reload something
                     (fail 'circular iset)
                     (values 'ok val))))
            ((match `(only ,? . ,symbols?) iset)
               (lets ((ok lib (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep lib (λ (var) (if (memq var (cddr iset)) var #false))))))
            ((match `(except ,? . ,symbols?) iset)
               (lets ((ok is (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep is (λ (var) (if (memq var (cddr iset)) #false var))))))
            ((match `(rename ,? . ,pairs?) iset)
               (lets ((ok lib (import-set->library (cadr iset) libs fail)))
                  (values 'ok
                     (env-keep lib
                        (λ (var)
                           (let ((val (assq var (cddr iset))))
                              (if val (cdr val) #false)))))))
            ((match `(prefix ,? ,symbol?) iset)
               (lets
                  ((ok lib (import-set->library (cadr iset) libs fail))
                   (prefix (symbol->string (caddr iset))))
                  (values 'ok
                     (env-keep lib
                        (λ (var)
                           (string->symbol
                              (string-append prefix (symbol->string var))))))))
            (else
               (fail 'needed iset))))

      ;; (foo bar baz) → "/foo/bar/baz.scm"
      (define (library-name->path iset)
         (bytes->string
            (cons #\/
               (foldr
                  (λ (thing tl)
                     (append
                        (string->list (symbol->string thing))
                        (if (null? tl)
                           (string->list ".scm")
                           (cons #\/ tl))))
                  #n iset))))

      ;; remove double slashes and dots
      (define envalidate-path
         (string->regex "s/([/.])\\1+/\\1/g"))

      ;; try to find and parse contents of <path> and wrap to (begin ...) or call fail
      (define (repl-include env path fail)
         (lets
            ((include-dirs (env-get env includes-key #n))
             (conv (λ (dir) (envalidate-path (list->string (append (string->list dir) (cons #\/ (string->list path)))))))
             (paths (map conv include-dirs))
             (contentss (map file->list paths))
             (data (find self contentss)))
            (if data
               (let ((exps (list->sexps data "library fail" path)))
                  (if exps ;; all of the file parsed to a list of sexps
                     (cons 'begin exps)
                     (fail (list "Failed to parse contents of " path))))
               (fail (list "Couldn't find " path "from any of" include-dirs)))))

      ;; nonempty list of symbols or integers
      (define (valid-library-name? x)
         (and (pair? x) (list? x) (every (λ (x) (or (integer? x) (symbol? x))) x)))

      ;; try to load a library based on it's name and current include prefixes if
      ;; it is required by something being loaded and we don't have it yet
      ;; → 'ok x env | 'error x reason | 'not-found x _
      (define (try-autoload env repl iset)
         (if (valid-library-name? iset) ;; (foo bar baz) → try to load "./foo/bar/baz.scm"
            (let
               ((exps
                  (call/cc
                     (λ (ret)
                        (repl-include env
                           (library-name->path iset) (λ (why) (ret #false)))))))
               (if exps
                  (success (repl env (cdr exps)) ; drop begin
                     ((ok value env)
                        ;; we now have the library if it was defined in the file
                        (values 'ok env))
                     ((fail why)
                        (values 'error why)))
                  (values 'not-found (library-name->path iset))))
            (values 'error (list "Bad library name:" iset))))

      (define (any->string obj)
         (list->string (render obj #n)))

      (define (import-iset repl ret)
         (lambda (env iset)
            (lets ((status lib (call/cc2 (λ (ret) (import-set->library iset (env-get env library-key #n) ret)))))
               (cond
                  ((eq? status 'needed)
                     (lets ((status env (try-autoload env repl lib)))
                        (cond
                           ((eq? status 'ok)
                              ;; env now contains the library
                              ((import-iset repl ret) env iset))
                           ((eq? status 'error)
                              (ret (fail (list "Failed to load" lib "because" env))))
                           (else
                              (ret (fail (list "I didn't have or find library" (any->string lib))))))))
                  ((eq? status 'ok)
                     (env-fold env-put-raw env lib)) ;; <- TODO env op, should be in (owl env)
                  ((eq? status 'circular)
                     (ret (fail (list "Circular dependency causing reload of" (bytes->string (render lib #n))))))
                  (else
                     (ret (fail (list "BUG: bad library load status: " status))))))))

      (define (library-import env exps fail repl)
         ;(print "importing " exps)
         (lets/cc ret ()
            (ok
               (repl-message "loaded")
               (fold (import-iset repl ret) env exps))))

      ;; toplevel import doing what library-import does within libraries
      (define (toplevel-library-import env exps repl)
         (lets/cc ret
            ((fail (λ (x) (ret (fail x)))))
            (library-import env exps fail repl)))

      (define (match-feature req feats libs fail)
         (cond
            ((memv req feats) #true) ;; a supported implementation feature
            ((symbol? req) #false)
            ((assv req libs) #true) ;; an available (loaded) library
            ((and (headed? 'not req) (= (length req) 2))
               (not (match-feature (cadr req) feats libs fail)))
            ((headed? 'and req)
               (every (λ (req) (match-feature req feats libs fail)) (cdr req)))
            ((headed? 'or req)
               (any (λ (req) (match-feature req feats libs fail)) (cdr req)))
            (else
               (fail "Weird feature requirement: " req))))

      (define (choose-branch bs env fail)
         (cond
            ((null? bs) bs) ;; nothing matches, no else
            ((match `(else . ,list?) (car bs)) (cdar bs))
            ((pair? (car bs))
               (if (match-feature
                        (caar bs)
                        (env-get env features-key #n)
                        (env-get env library-key #n)
                        fail)
                  (cdar bs)
                  (choose-branch (cdr bs) env fail)))
            (else
               (fail (list "Funny cond-expand node: " bs)))))


      ;; -> success, fail is a cont to a regular fail
      (define (repl-library exp env repl fail)
         (cond
            ((null? exp) (fail "no export?"))
            ((headed? 'import (car exp))
               (success (library-import env (cdar exp) fail repl)
                  ((ok value env)
                     (repl-library (cdr exp) env repl fail))
                  ((fail why)
                     (fail why))))
            ((headed? 'begin (car exp))
               ;; run basic repl on it
               (success (repl env (cdar exp))
                  ((ok value env)
                     ;; continue on to other defines or export
                     (repl-library (cdr exp) env repl fail))
                  ((fail why)
                     (fail why))))
            ((headed? 'export (car exp))
               ;; build the export out of current env
               (ok (build-export (cdar exp) env fail) env))
            ((headed? 'include (car exp))
               (repl-library
                  (foldr
                     (λ (path exp) (cons (repl-include env path fail) exp))
                     (cdr exp) (cdar exp))
                  env repl fail))
            ((headed? 'cond-expand (car exp))
               (repl-library
                  (append (choose-branch (cdar exp) env fail) (cdr exp))
                  env repl fail))
            (else
               (fail (list "unknown library term: " (car exp))))))

      ;; variables which are added to *owl-core* when evaluating libraries
      (define library-exports
         (list
            library-key     ;; loaded libraries
            includes-key    ;; where to load libraries from
            features-key))  ;; implementation features

      ;; env exps -> env' | #f
      (define (repl-exps env exps eval-repl repl)
         (fold
            (lambda (env exp)
               (and env
                  (success (eval-repl exp env repl)
                     ((ok result env)
                        env)
                     ((fail why)
                        (print "evaluation of " exp " failed.")
                        #false))))
            env exps))

      (define (eval-repl exp env repl)
         (lets/cc ret
            ((abort (λ (why) (ret (fail (list "Macro expansion of " exp " failed: " why)))))
             (env exp (macro-expand exp env abort)))
            (cond
               ((import? exp)
                  (success (toplevel-library-import env (cdr exp) repl)
                     ((ok value env)
                        (ok value env))
                     ((fail why)
                        (fail
                           (list "Failed to import " exp " because: " why)))))
               ((definition? exp)
                  (success (evaluate (caddr exp) env)
                     ((ok value env2)
                        (lets
                           ((env (env-set env (cadr exp) value))
                            )
                           (ok
                              (repl-message
                                 (bytes->string (render ";; Defined " (render (cadr exp) #n))))
                              (bind-toplevel env))))
                     ((fail reason)
                        (fail
                           (list "Definition of" (cadr exp) "failed because" reason)))))
               ((macro-definition? exp)
                  (success (evaluate (caddr exp) env)
                     ((ok value env2)
                        (lets
                           ((env (env-set-macro env (cadr exp) value)))
                           (ok
                              (repl-message
                                 (bytes->string (render ";; Defined macro " (render (cadr exp) #n))))
                              (bind-toplevel env))))
                     ((fail reason)
                        (fail
                           (list "Definition of macro" (cadr exp) "failed because" reason)))))
               ((multi-definition? exp)
                  (success (evaluate (caddr exp) env)
                     ((ok value env2)
                        (let ((names (cadr exp)))
                           (if (and (list? value)
                                 (= (length value) (length names)))
                              (ok (repl-message ";; All defined")
                                 (fold
                                    (λ (env pair)
                                       (env-set env (car pair) (cdr pair)))
                                    env
                                    (zip cons names value)))
                              (fail
                                 (list "Didn't get expected values for definition of " names)))))
                     ((fail reason)
                        (fail
                           (list "Definition of" (cadr exp) "failed because" reason)))))
               ((export? exp)
                  (lets ((module (build-export (cdr exp) env self))) ; <- to be removed soon, dummy fail cont
                     (ok module env)))
               ((headed? '__repl_begin exp)
                  ;; multiple expressions, likely containing definitions
                  ;; could convert multi define to this
                  (let ((envp (repl-exps env (cadr (cadr exp)) eval-repl repl)))
                     (if envp
                        (ok "done" envp)
                        (fail
                           "Evaluation of multiple expressions failed"))))
               ((library-definition? exp)
                  ;; evaluate libraries in a blank *owl-kernel* env (only primops, specials and define-syntax)
                  ;; include just loaded *libraries* and *include-paths* from the current one to share them
                  (lets/cc ret
                     ((exps (map cadr (cdr exp))) ;; drop the quotes
                      (name exps (uncons exps #false))
                      (libs (env-get env library-key #n))
                      ;; mark the current library as being loaded for circular dependency detection
                      (env (env-set env library-key (alset libs name 'loading)))
                      (fail
                        (λ (reason)
                           (ret (fail (list "Library" name "failed:" reason)))))
                      (lib-env
                        (fold
                           (λ (lib-env key) (env-set lib-env key (env-get env key #n)))
                           *owl-kernel* library-exports))
                      (lib-env
                         (bind-toplevel
                            (env-set lib-env current-library-key name))))
                     (success (repl-library exps lib-env repl fail) ;; anything else must be incuded explicitly
                        ((ok library lib-env)
                              (ok
                                 (repl-message
                                    (list->string
                                       (foldr render #n
                                          (list ";; Library " name " added" ))))
                                 (env-set env library-key
                                    (cons (cons name library)
                                       (remove ;; drop the loading tag for this library
                                          (B (C equal? name) car)
                                          (env-get lib-env library-key #n)))))) ; <- lib-env may also have just loaded dependency libs
                        ((fail why)
                           (fail
                              (list "Library" name "failed to load because" why))))))
               (else
                  (success (evaluate exp env)
                     ((ok val env) (ok val env))
                     ((fail why)
                        (fail (list "Failed to evaluate " exp " because " why))))))))


      ;; env exp-ll -> success
      (define (repl env in)
         (let loop ((env env) (in in) (last 'blank))
            (cond
               ((null? in)
                  (ok last env))
               ((pair? in)
                  (lets ((this in (uncons in #false)))
                     (cond
                        ((eof-object? this)
                           (ok last env))
                        ((repl-op? this)
                           (repl-op repl repl (cadr this) in env))
                        (else
                           (success (eval-repl this env repl)
                              ((ok result env)
                                 (prompt env result)
                                 (loop env in result))
                              ((fail reason)
                                 (fail reason)))))))
               (else
                  (loop env (in) last)))))


      ;; run the repl on a fresh input stream, report errors and catch exit

      (define (stdin-sexp-stream env)
         (byte-stream->exp-stream
            (if (env-get env '*readline* #f)
               (port->readline-byte-stream stdin)
               (port->byte-stream stdin))
            sexp-parser
            (resuming-syntax-fail
               (λ (x)
                  ;; x is not typically a useful error message yet
                  (print ";; syntax error")
                  (if (env-get env '*interactive* #false)
                     (display "> "))))))  ;; reprint prompt

      (define eof '(eof))

      ;; like repl-port, but bounces back to operation on errors, processes
      ;; repl commands (,help) and remembers input history and environment after
      ;; errors

      (define (repl-port env fd)
         (repl env
            (fd->exp-stream fd sexp-parser (silent-syntax-fail #n))))

      ;; env -> program exit value (unsigned byte)
      (define (repl-ui env)
         (let loop ((env env) (input (stdin-sexp-stream env)))
            (lets ((exp input (uncons input eof)))
               (cond
                  ((eq? exp eof)
                     (if (env-get env '*interactive* #false)
                        (print "bye bye _o/~"))
                     0)
                  ((repl-op? exp)
                     (repl-op loop repl (cadr exp) input env))
                  (else
                     (success (eval-repl exp env repl)
                        ((ok result env)
                           (prompt env result)
                           (loop env input))
                        ((fail why)
                           (print-repl-error why)
                           (maybe-prompt env)
                           (loop env input))))))))

      (define (repl-file env path)
         (let ((fd (if (equal? path "-") stdin (open-input-file path))))
            (if fd
               (success (repl-port env fd)
                  ((ok exp env)
                     (ok exp env))
                  ((fail why)
                     (fail (list "Cannot load " path ": " why))))
               (fail (list "Cannot open " path)))))

      (define (repl-string env str)
         (lets ((exps (try-parse (get-kleene-plus sexp-parser) (str-iter str) #false (silent-syntax-fail #f) #false)))
            (if exps
               (repl env exps)
               (fail "not parseable"))))
))
