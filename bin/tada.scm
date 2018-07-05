#!/usr/bin/ol -r

(import
   (owl sexp)
   (owl sys)
   (owl args)
   (owl proof)
   (owl date)
   (only (owl io) write-to))

;; todo: per-function documentation: simple arg case
;; todo: per-function documentation: type (function vs macro)
;; todo: per-function documentation: well formed comments

(define (make-markdown-prelude)
   (lets ((d m y H M S (date (time))))
(str
"---
title: \"Owl Lisp v" *owl-version* " manual\"
date: " y "-" m "-" d "
geometry: \"left=3cm,right=3cm,top=2cm,bottom=2cm\"
output: pdf_document
---

")))

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

(define (sselect-all exps sq)
   (define (walk exps sq found)
      (cond
         ((null? exps)
            found)
         ((car-eq? (car exps) (car sq))
            (if (null? (cdr sq))
               (cons (car exps)
                  (walk (cdr exps) sq found))
               (walk (cdar exps) (cdr sq)
                  (walk (cdr exps) sq
                     found))))
         (else
            (walk (cdr exps) sq found))))
   (walk exps sq #null))

(define (sselect exps sq)
   (cond
      ((null? exps)
         #false)
      ((car-eq? (car exps) (car sq))
         (if (null? (cdr sq))
            (car exps)
            (or
               (sselect (cdar exps) (cdr sq))
               (sselect (cdr exps) sq))))
      (else
         (sselect (cdr exps) sq))))

(example
   (sselect-all '((A (B 1)) (A (C 1)) (A (B 2))) '(A B))
      = '((B 1) (B 2))
   (sselect-all '((A (B 1) (B 2))) '(A B))
      = '((B 1) (B 2))
   (sselect-all '((A) (A (B 1) (B 11)) (A (C 1)) (A (B 2) (B 22))) '(A B))
      = '((B 1) (B 11) (B 2) (B 22)))

;;; Command line

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (source-dir "-d" "--directory" plural has-arg
          comment "a directory to find sources form (recursively)")
       (output   "-o" "--output" has-arg default "manual.md"
          comment "output file")
       (prelude  "-p" "--prelude" has-arg default "doc/manual.md"
          comment "documentation prelude file"))))


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
          (lib-name (maybe cadr (sselect sexps '(define-library))))
          (exports (maybe cdr (sselect sexps '(define-library export))))
          (initial-doc
             (maybe bytes->string
                (find-initial-documentation content)))
          (examples
             (map cdr
               (sselect-all sexps '(define-library begin example)))))
      (if (or initial-doc lib-name)
         (cons
            (-> #empty
               (put 'path path)
               (put 'length (length content))
               (maybe-put 'name lib-name)
               (maybe-put 'exports exports)
               (maybe-put 'examples
                  (if (null? examples)
                     #f
                     examples))
               (maybe-put 'initial-doc initial-doc))
            others)
         others)))

;;; Rendering

(define (render-example example)
   (str " - `" (car example) "` → `" (caddr example) "`\n"))

(define (render-documentation doc)
   (print "Documentation examples are " (doc 'examples #f))
   (str
      "\n## " (doc 'name (doc 'path)) "\n"
      (doc 'initial-doc "\n")
      "\n"
      "Exported values:\n\n"
      (foldr
         (λ (x tl) (str "- `" x "`\n" tl))
         "" (doc 'exports #null))
      "\n"
      (if (doc 'examples #f)
         (str
            "### Examples\n\n"
            (foldr str ""
               (map render-example
                  (doc 'examples))))
         "")
   ))

;;; Entry

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extras)
         (lets
             ((docs
               (fold gather-documentation #null
                  (sort string>?
                     (dirs->list-recursive
                        (getf opts 'source-dir)))))
              (prelude
                 (bytes->string
                    (file->list (getf opts 'prelude))))
               (output-path
                  (getf opts 'output))
               (output-port
                  (open-output-file output-path)))
            (for-each print docs)
            (print "Writing to " output-path)
            (print-to output-port (make-markdown-prelude))
            (print-to output-port prelude)
            (for-each
               (λ (doc-node)
                  (print "rendering " doc-node)
                  (print-to output-port
                     (render-documentation doc-node)))
               docs)
            (close-port output-port))))
   0)

