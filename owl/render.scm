#| doc
Rendering Values

There are two ways to serialize values to sequences of bytes: S-expressions and the
FASL encoding. S-expressions represent trees of certain values and can thus only
be used to accurately represent a subset of all possible values. FASL encoding can
represent all values, apart from from their external semantics such as open network
connections.

This library implements the S-expression part. There are further two
flavors of rendering: the one intended to be printed out of programs and the one
which results in expressions that can be read back in. The first one is usually seen
as the output of print, whereas the other one is typically written with write.

Some R7RS Scheme extensions to represent shared structure in S-expressions is also
implemented.

Str and str* are used to translate values to strings, whereas render and render* are
typically used in a fold to construct lists of bytes to be later converted to strings
or other formats.
|#

(define-library (owl render)

   (import
      (owl core)
      (owl eof)
      (owl string)
      (owl list)
      (owl list-extra)
      (owl boolean)
      (owl ff)
      (owl tuple)
      (owl function)
      (owl syscall)
      (owl lazy)
      (owl proof)
      (owl math)
      (owl port)
      (only (owl symbol) render-symbol symbol?)
      (only (owl bytevector) bytevector? bytevector->list)
      (only (owl vector) vector? vector->list)
      (only (owl math) render-number number?)
      (only (owl string) render-string string?))

   (export
      make-serializer    ;; names → (obj tl → (byte ... . tl))
      ;serialize         ;; obj tl        → (byte ... . tl), eager, always shared
      ;serialize-lazy    ;; obj tl share? → (byte ... . tl), lazy, optional sharing
      render             ;; obj tl        → (byte ... . tl) -- deprecated
      render*            ;;   as above, as in write
      str                ;; renderable ... → string
      str*               ;;   as above, as in write
      )

   (begin

      ;; used e.g. in print
      (define (make-renderer meta)
         (define (render obj tl)
            (cond

               ((null? obj)
                  (ilist #\( #\) tl))

               ((number? obj)
                  (render-number obj tl 10))

               ((string? obj)
                  (render-string obj tl))

               ((pair? obj)
                  (if (and (eq? (car obj) 'quote)
                           (pair? (cdr obj))
                           (null? (cddr obj)))
                     (cons #\'
                        (render (cadr obj) tl))
                     (cons #\(
                        (cdr
                           (let loop ((obj obj) (tl (cons #\) tl)))
                              (cond
                                 ((null? obj) tl)
                                 ((pair? obj)
                                    (cons #\space
                                       (render (car obj) (loop (cdr obj) tl))))
                                 (else
                                    (ilist #\space #\. #\space (render obj tl)))))))))

               ((boolean? obj)
                  (append (string->list (if obj "#true" "#false")) tl))

               ((symbol? obj)
                  (render-symbol obj tl))

               ((vector? obj)
                  (cons #\#
                     (if (bytevector? obj)
                        (ilist #\u #\8 (render (bytevector->list obj) tl))
                        (render (vector->list obj) tl))))

               ((function? obj)
                  ;; anonimas
                  ;(let ((symp (interact 'intern (tuple 'get-name obj))))
                  ;   (if symp
                  ;      (ilist #\# #\< (render symp (cons #\> tl)))
                  ;      (render "#<function>" tl)))
                  (render "#<function>" tl)
               )

               ((tuple? obj)
                  (ilist #\# #\[
                     (render (ref obj 1)
                        (fold
                           (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
                           (cons #\] tl)
                           (iota (tuple-length obj) -1 1)))))

               ((record? obj)
                  (ilist #\# #\{
                     (render (ref obj 1) ;; type tag object
                        (fold
                           (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
                           (cons #\} tl)
                           (iota (tuple-length obj) -1 1)))))

               ((tuple? obj)
                  (ilist #\# #\[ (render (tuple->list obj) (cons #\] tl))))

               ((port? obj) (ilist #\# #\[ #\f #\d #\space (render (port->fd obj) (cons #\] tl))))
               ((eof-object? obj) (ilist #\( #\e #\o #\f #\- #\o #\b #\j #\e #\c #\t #\) tl))

               (else
                  (append (string->list "#<WTF>") tl)))) ;; What This Format?
         render)

      (define render
         (make-renderer empty))

      ;;; serialize suitably for parsing, not yet sharing preserving

      ;; hack: positive id = not written yet, negative = written, so just output a reference

      ; laziness changes:
      ;  - use explicit CPS to 'return'
      ;  - emit definition on first encounter

      ;; used e.g. in write
      (define (make-ser names)
         (define (ser sh obj k)
            (cond

               ;((get sh obj #f) =>
               ;   (λ (id)
               ;      (if (< id 0) ;; already written, just refer
               ;         (ilist #\# (render (abs id) (pair #\# (k sh))))
               ;         (ilist #\#
               ;            (render id
               ;               (ilist #\# #\=
               ;                  (ser (del sh obj) obj
               ;                     (λ (sh)
               ;                        (delay
               ;                           (k (put sh obj (- 0 id))))))))))))

               ((null? obj)
                  (ilist #\( #\) (k sh)))

               ((number? obj)
                  (render-number obj (delay (k sh)) 10))

               ((string? obj)
                  (cons #\"
                     (render-quoted-string obj  ;; <- all eager now
                        (pair #\" (k sh)))))

               ((pair? obj)
                  (if (and (eq? (car obj) 'quote)
                           (pair? (cdr obj))
                           (null? (cddr obj)))
                     (cons #\'
                        (ser sh (cadr obj) k))
                     (cons 40
                        (let loop ((sh sh) (obj obj))
                           (cond
                              ((null? obj)
                                 ;; run of the mill list end
                                 (pair 41 (k sh)))
                              ((get sh obj #f) =>
                                 (λ (id)
                                    (ilist #\. #\space #\#
                                       (render (abs id)
                                          (cons #\#
                                             (if (< id 0)
                                                (pair 41 (k sh))
                                                (pair #\=
                                                   (ser (del sh obj) obj
                                                      (λ (sh)
                                                         (pair 41
                                                            (k
                                                               (put sh obj
                                                                  (- 0 id)))))))))))))
                              ((pair? obj)
                                 ;; render car, then cdr
                                 (ser sh (car obj)
                                    (λ (sh)
                                       (delay
                                          (if (null? (cdr obj))
                                             (loop sh (cdr obj))
                                             (cons #\space (loop sh (cdr obj))))))))
                              (else
                                 ;; improper list
                                 (ilist #\. #\space
                                    (ser sh obj
                                       (λ (sh) (pair 41 (k sh)))))))))))

               ((boolean? obj)
                  (append
                     (string->list (if obj "#true" "#false"))
                     (delay (k sh))))

               ((symbol? obj)
                  (render-symbol obj (delay (k sh))))

               ((vector? obj)
                  (cons #\#
                     (if (bytevector? obj)
                        (ilist #\u #\8 (ser sh (bytevector->list obj) k))
                        (ser sh (vector->list obj) k)))) ;; <- should convert incrementally!

               ((function? obj)
                  (let ((name (get names obj #f)))
                     ;; render name is one is known, just function otherwise
                     ;; note - could print `(foo ,map ,+ -) instead of '(foo #<map> <+> -) in the future
                     (if name
                        (foldr render (delay (k sh)) (list "#<" name ">"))
                        (render "#<function>" (delay (k sh))))))

               ((tuple? obj)
                  (ilist #\# #\[
                     (ser sh (tuple->list obj)
                        (λ (sh) (pair #\] (k sh))))))

               ((port? obj)   (render obj (λ () (k sh))))
               ((eof-object? obj) (render obj (λ () (k sh))))

               (else
                  (append (string->list "#<WTF>") (delay (k sh))))))
         ser)

      (define (not-self-quoting? val)
         (or (null? val) (pair? val) (symbol? val)))

      ;; could drop val earlier to possibly gc it while rendering
      (define (maybe-quote val lst)
         (if (not-self-quoting? val)
            (cons #\' lst)
            lst))

      ;; a value worth checking for sharing in datum labeler
      (define (shareable? x)
         (not (or (function? x) (symbol? x) (port? x))))

      (define (partial-object-closure seen obj)
         (cond
            ((immediate? obj) seen)
            ((get seen obj #f) =>
               (λ (n) (fupd seen obj (+ n 1))))
            (else
               (let ((seen (put seen obj 1)))
                  (if (raw? obj)
                     seen
                     (fold partial-object-closure seen
                        (tuple->list obj)))))))

      (define (sub-objects root pred)
         (ff->list
            (partial-object-closure empty root)))

      ;; val → ff of (ob → node-id)
      (define (label-shared-objects val)
         (lets
            ((refs (sub-objects val shareable?))
             (shares
               (fold
                  (λ (shared p)
                     (lets ((ob refs p))
                        (cond
                           ((eq? refs 1) shared)
                           ((shareable? ob) (cons ob shared))
                           (else shared))))
                  #n refs)))
            (let loop ((out empty) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (make-lazy-serializer names)
         (let ((ser (make-ser names)))
            (λ (val tl share?)
               (maybe-quote val
                  (ser empty val (λ (sh) tl))))))

      (define (make-serializer names)
         (lets
            ((serialize-lazy (make-lazy-serializer names)))
            (λ (val tl)
               (force-ll
                  (serialize-lazy val tl #true)))))

      (define render*
         (make-serializer empty))

      (define (str . args)
         (bytes->string
            (foldr render #n args)))

      (define (str* . args)
         (bytes->string
            (foldr render* #n args)))

      (example
         (render  "foo" null) = (string->list "foo")
         (render* "foo" null) = (string->list "\"foo\""))
))
