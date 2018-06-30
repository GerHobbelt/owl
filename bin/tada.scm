#!/usr/bin/ol -r

(import 
   (owl sexp)
   (owl sys)
   (owl args))

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


;;; Command line

(define command-line-rules
   (cl-rules
     `((help     "-h" "--help")
       (source-dir "-d" "--directory" plural has-arg
          comment "a directory to find sources form (recursively)"))))


;;; Different kinds of documentation

(define (gather-documentation path)
   (print " - " path)
   (lets ((content (file->list path))
          (sexps (list->sexps content #false #false))) ;; module is in one sexp
      (print (list 'path path 'length (length content) 'sexps (length sexps)))))


;;; Entry

(λ (args)
   (process-arguments (cdr args) command-line-rules "you lose"
      (λ (opts extras)
         (print "opts: " opts)
         (print "extras: " extras)
         (let ((files (dirs->list-recursive (getf opts 'source-dir))))
            (print "gathering documentation from " (length files) " files.")
            (map gather-documentation files))))
   0)


