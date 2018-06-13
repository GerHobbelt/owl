
;; making printable representations

(define-library (owl render)

   (import
      (owl defmac)
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
      (owl math)
      (owl port)
      (only (owl symbol) render-symbol symbol?)
      (only (owl bytevector) bytevector? bytevector->list)
      (only (owl vector) vector? vector->list)
      (only (owl math) render-number number?)
      (only (owl string) render-string string?))

   (export
      make-serializer    ;; names → ((obj tl) → (byte ... . tl))
      ;serialize         ;; obj tl        → (byte ... . tl), eager, always shared
      ;serialize-lazy    ;; obj tl share? → (byte ... . tl), lazy, optional sharing
      render             ;; obj tl        → (byte ... . tl) -- deprecated
      str                ;; renderable ... → string
      )

   (begin

      (define lp #\()
      (define rp #\))

      ;; this could be removed?
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
                  (cons #\(
                     (cdr
                        (let loop ((obj obj) (tl (cons #\) tl)))
                           (cond
                              ((null? obj) tl)
                              ((pair? obj)
                                 (cons #\space
                                    (render (car obj) (loop (cdr obj) tl))))
                              (else
                                 (ilist #\space #\. #\space (render obj tl))))))))

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

               ((eq? obj #empty) ;; don't print as #()
                  (ilist #\# #\e #\m #\p #\t #\y tl))

               ((ff? obj)
                  (cons #\@ (render (ff-foldr (λ (st k v) (cons k (cons v st))) null obj) tl)))

               ((tuple? obj)
                  (ilist #\# #\[ (render (tuple->list obj) (cons #\] tl))))

               ((port? obj) (ilist #\# #\[ #\f #\d #\space (render (port->fd obj) (cons #\] tl))))
               ((eof-object? obj) (ilist #\# #\e #\o #\f tl))
               ((eq? obj #empty) (ilist #\# #\e #\m #\p #\t #\y tl))

               (else
                  (append (string->list "#<WTF>") tl)))) ;; What This Format?
         render)

      (define render
         (make-renderer #empty))

      ;;; serialize suitably for parsing, not yet sharing preserving

      ;; hack: positive id = not written yet, negative = written, so just output a reference

      ; laziness changes:
      ;  - use explicit CPS to 'return'
      ;  - emit definition on first encounter

      (define (make-ser names)
         (define (ser sh obj k)
            (cond

               ((getf sh obj) =>
                  (λ (id)
                     (if (< id 0) ;; already written, just refer
                        (ilist #\# (render (abs id) (pair #\# (k sh))))
                        (ilist #\#
                           (render id
                              (ilist #\# #\=
                                 (ser (del sh obj) obj
                                    (λ (sh)
                                       (delay
                                          (k (put sh obj (- 0 id))))))))))))

               ((null? obj)
                  (ilist #\' #\( #\) (k sh)))

               ((number? obj)
                  (render-number obj (delay (k sh)) 10))

               ((string? obj)
                  (cons #\"
                     (render-quoted-string obj  ;; <- all eager now
                        (pair #\" (k sh)))))

               ((pair? obj)
                  (cons 40
                     (let loop ((sh sh) (obj obj))
                        (cond
                           ((null? obj)
                              ;; run of the mill list end
                              (pair 41 (k sh)))
                           ((getf sh obj) =>
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
                                    (λ (sh) (pair 41 (k sh))))))))))

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
                  (let ((name (getf names obj)))
                     ;; render name is one is known, just function otherwise
                     ;; note - could print `(foo ,map ,+ -) instead of '(foo #<map> <+> -) in the future
                     (if name
                        (foldr render (delay (k sh)) (list "#<" name ">"))
                        (render "#<function>" (delay (k sh))))))


               ;; not sure yet what the syntax for these should be
               ;((tuple? obj)
               ;   (ilist 40 84 117 112 108 101 32
               ;      (render (ref obj 1)
               ;         (fold
               ;            (λ (tl pos) (cons 32 (render (ref obj pos) tl)))
               ;            (cons 41 tl)
               ;            (iota (tuple-length obj) -1 1)))))

               ((eq? obj #empty) ;; @() is also valid
                  (ilist #\# #\e #\m #\p #\t #\y (delay (k sh))))

               ((ff? obj)
                  (cons #\@ (ser sh (ff-foldr (λ (st k v) (cons k (cons v st))) null obj) k)))

               ((tuple? obj)
                  (ilist #\# #\[
                     (ser sh (tuple->list obj)
                        (λ (sh) (pair #\] (k sh))))))

               ((port? obj)   (render obj (λ () (k sh))))
               ((eof-object? obj) (render obj (λ () (k sh))))
               ((eq? obj #empty)    (render obj (λ () (k sh))))

               (else
                  (append (string->list "#<WTF>") (delay (k sh))))))
         ser)

      (define (self-quoting? val)
         (or
            (immediate? val)
            (number? val) (string? val) (function? val)
            (ff? val)))

      ;; could drop val earlier to possibly gc it while rendering
      (define (maybe-quote val lst)
         (if (self-quoting? val)
            lst
            (cons #\' lst)))

      ;; a value worth checking for sharing in datum labeler
      (define (shareable? x)
         (not (or (function? x) (symbol? x) (port? x))))

      (define (partial-object-closure seen obj)
         (cond
            ((immediate? obj) seen)
            ((getf seen obj) =>
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
                  null refs)))
            (let loop ((out empty) (shares shares) (n 1))
               (if (null? shares)
                  out
                  (loop (put out (car shares) n) (cdr shares) (+ n 1))))))

      (define (make-lazy-serializer names)
         (let ((ser (make-ser names)))
            (λ (val tl share?)
               (maybe-quote val
                  (ser
                     (if share? ;; O(n), allow skipping
                        (label-shared-objects val)
                        empty)
                     val (λ (sh) tl))))))

      (define (make-serializer names)
         (let ((serialize-lazy (make-lazy-serializer names)))
            (λ (val tl)
               (force-ll
                  (serialize-lazy val tl #true)))))

      (define (str . args)
         (bytes->string
            (foldr render null args)))
))
