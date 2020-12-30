#| doc 
Association Lists

Association lists a lists of pairs of keys and their corresponding values. This 
library has functions for processing them following the naming and argument order
conventions used also in other data structures.
|#

(define-library (owl alist)

   (import
      (owl core)
      (owl proof))

   (export
      ;; usual naming conventions: [short datastructure type prefix]
      ;; [name shared across data structures]
      alset
      alget
      aldel
      aled
      aledn)

   (begin

      ;; association list operations. same naming conventions as used elsewhere, such as in (owl ff)

      ;; edit list spine node at key / end
      (define (aledn lst key op)
         (cond
            ((eq? lst #null)
               (op lst))
            ((eq? (car (car lst)) key)
               (op lst))
            (else
               (cons (car lst)
                  (aledn (cdr lst) key op)))))

      ;; edit a list value node at key
      (define (aled lst key op def)
         (aledn lst key
            (lambda (node)
               (if (null? node)
                  (list (cons key (op def)))
                  (cons
                     (cons key (op (cdr (car node))))
                     (cdr node))))))

      (define (alset lst key val)
         (aled lst key
            (lambda (old) val)
            #f))

      (define (aldel lst key)
         (aledn lst key
            (lambda (val)
               (if (null? val)
                  val
                  (cdr val)))))

      (define (alget lst key def)
         (cond
            ((eq? lst #null)
               def)
            ((eq? (car (car lst)) key)
               (cdr (car lst)))
            (else
               (alget (cdr lst) key def))))

      (example
         let al = '((a . 1) (b . 2))
         (alget al 'b #f) = 2
         (alget al 'x #f) = #f
         (alset al 'b 20) = '((a . 1) (b . 20))
         (alset al 'c 3) = '((a . 1) (b . 2) (c . 3))
         (pipe al (aldel 'a) (aldel 'b) (alset 'x 10)) = '((x . 10)))
))
