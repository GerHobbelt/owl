#| doc
Command Line Arguments

This library makes it easy to write tools which use standard UNIX command line
tool conventions. These include accepting short and long versions of flags (-h
or --help), packing multiple flags to a single short one (-hai = -h -a -i),
handling and verification of flag arguments (--output foo.txt), ability to
handle flags anywhere in the arguments and use of -- to stop command line
argument processing (e.g. to remove a file called -h or process globbed files
coming from an untrusted source).


|#


(define-library (owl args)

   (export
      process-arguments    ;; sexp → cl-rules
      format-rules         ;; cl-rules → str
      print-rules          ;; cl-rules → _
      parse-args           ;; cl-rules args -> dict args' | false false
      cl-rules)            ;; sexp → cl-rules

   (import
      (owl core)
      (owl symbol)
      (owl list-extra)
      (owl lazy)
      (owl boolean)
      (owl function)
      (owl math)
      (owl syscall)
      (owl io)
      (owl port)
      (owl render)
      (owl list)
      (owl string)
      (owl equal)
      (owl regex)
      (owl ff)
      (owl lcd)
      (scheme cxr)
      (scheme write)
      )

   (begin
      ;; cl-rules is a ff of
      ;;   'short -> -x
      ;;   'long  -> --xylitol

      ;; str (rule-ff ..) → #false | rule-ff
      (define (select-rule string rules)
         (if (null? rules)
            #false
            (let ((this (car rules)))
               (if (or (equal? string (get this 'short))
                       (equal? string (get this 'long)))
                  this
                  (select-rule string (cdr rules))))))

      (define explodable? (string->regex "m/^-[^-]{2,}/"))

      ;; "-foo" → ("-f" "-o" "-o") | #false
      (define (explode str)
         (if (explodable? str)
            (map
               (λ (char) (runes->string (list 45 char)))
               (cdr (string->bytes str)))
            #false))

      ;; switch to returning these
      (define (fail fools)
         (write-bytes stderr (foldr render '(10) fools))
         #false)

      (define blank "nan") ; <- unique because allocated here

      (define (undefined? ff key)
         (eq? blank (get ff key blank)))

      (define (defined? ff key)
         (not (undefined? ff key)))

      ;; check that all rules which are marked mandatory have the corresponding id defined in dict
      (define (mandatory-args-given? dict rules)
         (fold
            (λ (ok? rule)
               (if (get rule 'mandatory) ;; this is mandatory
                  (if (defined? dict (get rule 'id))
                     ok?
                     (begin
                        (write-bytes stderr
                           (foldr render '(10)
                              (list "mandatory option not given: " (get rule 'long "(missing)"))))
                        #false))
                  ok?))
            #true rules))

      ;; set set all default which are not set explicitly
      (define (fill-defaults dict rules)
         (fold
            (λ (dict rule)
               (let ((id (get rule 'id)))
                  (if (and (undefined? dict id) (defined? rule 'default))
                     (put dict id
                        (let ((cookd ((get rule 'cook) (get rule 'default))))
                           (if (get rule 'plural) (list cookd) cookd))) ; <- a single plural default value needs to be listed
                     dict)))
            dict rules))

      ;; a fast /^-/ to shave some startup ms for thousands of arguments, which are getting common for some tools
      (define (dashy? str)
         (let ((s (sizeb str)))
            (if (lesser? s 1)
               #false
               (eq? 45 (ref str 0)))))

      (define (walk rules args dict others)
         (cond
            ((null? args)
               (if (mandatory-args-given? dict rules)
                  (prod (fill-defaults dict rules) (reverse others))
                  #false))
            ((dashy? (car args))
               (cond
                  ((string=? (car args) "--")
                     (walk rules #n dict (append (reverse (cdr args)) others)))
                  ((select-rule (car args) rules) =>
                     (λ (rule)
                        (lets
                           ((cook (get rule 'cook))
                            (id (get rule 'id)))
                           (if cook ;; <- set if this expects an argument
                              (if (null? (cdr args))
                                 (fail (list "'" (car args) "' requires an argument."))
                                 (lets
                                    ((value (cook (cadr args)))
                                     (ok? ((get rule 'pred self) value)))
                                    (if ok?
                                       (walk rules
                                          ;; instert an implicit -- after terminal rules to stop
                                          (if (get rule 'terminal) (cons "--" (cddr args)) (cddr args))
                                          (put dict id
                                             (if (get rule 'plural)
                                                ;; put values to a list if this is a multi argument
                                                (append (get dict id #n) (list value))
                                                value))
                                          others)
                                       (fail
                                          (list "The argument '" (car args) "' did not accept '" (cadr args) "'.")))))
                              ;; this doesn't have an argument, just count them
                              (walk rules (cdr args)
                                 (put dict id (+ 1 (get dict id 0)))
                                 others)))))
                  ((explode (car args)) =>
                     (λ (opts) ;; --foo → -f -o -o
                        (walk rules (append opts (cdr args)) dict others)))
                  ((string=? (car args) "-") ;; allow a solitary - to be used as an argument (usually to mean stdin/out)
                     (walk rules (cdr args) dict (cons (car args) others)))
                  (else
                     (fail (list "Unknown argument: " (car args))))))
            (else
               ;;; add this to other arguments
               (walk rules (cdr args) dict (cons (car args) others)))))

      ; + cook, pred, terminal, multi, id

      (define (process-arguments args rules error-msg cont)
         (let ((res (walk rules args empty #n)))
            (if res
               (lets ((dict others <- res))
                  (cont dict others))
               (begin
                  (print-to stderr error-msg)
                  #false))))

      ;; a simpler api; rules args -> dict rest | false false on error
      (define (parse-args rules args)
         (let ((res (walk rules args empty null)))
            (if res
               (lets ((dict others <- res))
                  (values dict others))
               (values false false))))

      ;; and now a friendlier way to define the rules

      (define (cl-rule node lst)
         (if (null? lst)
            node
            (lets ((op lst (uncons lst #false)))
               (cond
                  ((eq? op 'mandatory) (cl-rule (put node op #true) lst))
                  ((eq? op 'plural)    (cl-rule (put node 'plural #true) lst))
                  ((eq? op 'terminal)  (cl-rule (put node op #true) lst))
                  ((eq? op 'has-arg) ;; short for cook id
                     (cl-rule node (ilist 'cook self lst)))
                  ((eq? op 'cook)
                     (if (and (pair? lst) (function? (car lst)))
                        (cl-rule (put node 'cook (car lst)) (cdr lst))
                        (error "cl-rule: cook is not a function: " (list (car lst) 'has 'type (type (car lst))))))
                  ((eq? op 'check)
                     (if (and (pair? lst) (function? (car lst)))
                        (cl-rule (put node op (car lst)) (cdr lst))
                        (error "cl-rule: check is not a function: " (car lst))))
                  ((eq? op 'default)
                     (if (and (pair? lst) (string? (car lst)))
                        (cl-rule (put node op (car lst)) (cdr lst))
                        (error "cl-rule: default is not a string: " (car lst))))
                  ((eq? op 'comment)
                     (if (and (pair? lst) (string? (car lst)))
                        (cl-rule (put node op (car lst)) (cdr lst))
                        (error "cl-rule: comment is not a string: " (car lst))))
                  (else
                     (error "cl-rule: i do not get this: " lst))))))

      ; (name short long comment default (cook) (predicate) (mandatory?) (single?) (terminal?))
      (define (cl-rules lst)
         (map
            (λ (lst)
               (if (and (>= (length lst) 3) (symbol? (car lst)))
                  (cl-rule
                     (list->ff (zip cons '(id short long) lst))
                     (cdddr lst))
                  (error "cl-rules: funny option: " lst)))
            lst))

      ;; printing help based on the rules

      (define nl (runes->string '(10)))

      ;; format rule descriptions for printing
      ;; rules → string
      (define (format-rules rules)
         (runes->string
            (foldr
               (λ (rule tl)
                  (foldr
                     render
                     tl
                     (list "  "
                        (let ((short (get rule 'short)))
                           (if short
                              (string-append short " | ")
                              "     "))
                        (get rule 'long)
                        (if (get rule 'cook) " <arg>" "")
                        (if (get rule 'comment)
                           (string-append ", " (get rule 'comment))
                           "")
                        (if (get rule 'default)
                           (string-append " [" (get rule 'default) "]")
                           "")
                        (if (get rule 'mandatory) " (mandatory)" "")
                        (if (get rule 'plural) " (can be several)" "")
                        (if (get rule 'terminal) " (terminal)" "")
                        nl)))
               #n rules)))

      (define print-rules
         (B display format-rules))
))
