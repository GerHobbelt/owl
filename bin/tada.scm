#!/usr/bin/ol -r

(import
   (owl sexp)
   (owl sys)
   (owl args)
   (only (owl io) write-to))

;; todo: multiple match squery → get all examples
;; todo: per-function documentation: simple arg case
;; todo: per-function documentation: type (function vs macro)
;; todo: per-function documentation: well formed comments

;;; File system

(define (dir->list-recursive path)
   (define (walk path out)
      (if (directory? path)
         (fold
            (λ (out sub)
               (walk (str path "/" sub) out))
            out (dir->list path))
         (cons path out)))
   (walk path #null))

(define (dirs->list-recursive paths)
   (fold
      (λ (out thing)
         (append (dir->list-recursive thing) out))
      #null paths))


;;; HTML

(define safe-chars
   (fold
      (lambda (ff x) (put ff x x))
      #empty
      (list #\space #\- #\_ #\: #\( #\) #\?)))

(define (html-safe s)
   (list->string
      (foldr
         (lambda (char tl)
            (cond
               ((<= #\a char #\z) (cons char tl))
               ((<= #\0 char #\9) (cons char tl))
               ((<= #\A char #\Z) (cons char tl))
               ((getf safe-chars char) (cons char tl))
               (else (render (str "&#" char ";") tl))))
         #n
         (string->list s))))

;;; sexp query

(define (car-eq? exp a)
   (and (pair? exp) (eq? (car exp) a)))

(define (squery exps sq)
   (cond
      ((null? exps)
         #false)
      ((car-eq? (car exps) (car sq))
         (if (null? (cdr sq))
            (car exps)
            (or
               (squery (cdar exps) (cdr sq))
               (squery (cdr exps) sq))))
      (else
         (squery (cdr exps) sq))))



;;; Command line

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (source-dir "-d" "--directory" plural has-arg
          comment "a directory to find sources form (recursively)")
       (output   "-o" "--output" has-arg default "manual.sexp"
          comment "output sexp file"))))
          

;;; Embedded documentation in comments

(define (find-initial-documentation bs)
   (if (m/^#\| doc\n/ bs)
      (s/^#\| doc\n(.*?)\|#.*/\1/ bs)
      #false))


;;; Different kinds of documentation

(define (maybe op arg)
   (if arg (op arg) arg))

(define (maybe-put ff key val)
   (if val (put ff key val) ff))

(define (gather-documentation others path)
   (print " - " path)
   (lets ((content (file->list path))
          (sexps (list->sexps content #false #false)) ;; module is in one sexp
          (lib-name (maybe cadr (squery sexps '(define-library))))
          (exports (maybe cdr (squery sexps '(define-library export))))
          (initial-doc
             (maybe bytes->string
                (find-initial-documentation content)))
          (examples ;; first one for now
             (squery sexps '(define-library begin example))))
      (if (or initial-doc lib-name)
         (cons
            (-> #empty
               (put 'path path)
               (put 'length (length content))
               (maybe-put 'name lib-name)
               (maybe-put 'exports exports)
               (maybe-put 'examples examples)
               (maybe-put 'initial-doc initial-doc))
            others)
         others)))

;;; Entry

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extras)
         (let ((docs 
                  (fold gather-documentation #null
                     (sort string>?
                        (dirs->list-recursive
                           (getf opts 'source-dir)))))
               (output-path
                  (getf opts 'output)))
            (for-each print docs)
            (print "Writing to " output-path)
            (write-to (open-output-file output-path) docs))))
   0)

