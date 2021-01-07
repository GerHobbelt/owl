#| doc
Association Lists

Association lists a lists of pairs of keys and their corresponding values. This
library has functions for processing them following the naming and argument order
conventions used also in other data structures.
|#

(define-library (owl alist)

   (import
      (owl core)
      (owl list)
      (owl equal)
      (owl proof))

   (export

      ;; shared
      alist
      alkeys
      alvalues

      ;; eq? keys
      setq
      getq
      delq
      edq
      ednq

      ;; equal? keys
      alset
      alget
      aldel
      aled
      aledn)

   (begin

      ;; association list operations. same naming conventions as used elsewhere, such as in (owl ff)

      ;; edit list spine node at key / end
      (define (mk-edn equal?)
         (define (edn lst key op)
            (cond
               ((eq? lst #null)
                  (op lst))
               ((equal? (car (car lst)) key)
                  (op lst))
               (else
                  (cons (car lst)
                     (edn (cdr lst) key op)))))
        edn)

      (define (mk-get equal?)
         (define (get lst key def)
            (cond
               ((eq? lst #null)
                  def)
               ((equal? (car (car lst)) key)
                  (cdr (car lst)))
               (else
                  (get (cdr lst) key def))))
         get)

      (define (make-association-ops compare)
         (lets
            ((edn (mk-edn compare))
             (get (mk-get compare))
             (edv
                (lambda (lst key op def)
                   (edn lst key
                     (lambda (node)
                        (if (null? node)
                           (list (cons key (op def)))
                           (cons
                              (cons key (op (cdr (car node))))
                              (cdr node)))))))
             (set
                (lambda (lst key val)
                   (edv lst key (lambda (old) val)
                   #f)))
             (del
                (lambda (lst key)
                   (edn lst key
                      (lambda (val)
                         (if (eq? val #null)
                            val
                            (cdr val)))))))
            (values
               edn
               edv
               get
               set
               del)))

      (define-values (ednq edq getq setq delq)
         (make-association-ops eq?))

      (define-values (aledn aled alget alset aldel)
         (make-association-ops equal?))

      (example
         let al = '((a . 1) (b . 2))
         (alget al 'b #f) = 2
         (alget al 'x #f) = #f
         (alset al 'b 20) = '((a . 1) (b . 20))
         (alset al 'c 3) = '((a . 1) (b . 2) (c . 3))
         (pipe al (aldel 'a) (aldel 'b) (alset 'x 10)) = '((x . 10)))


      ;; equality-independent functions and macros

      (define-syntax alist
         (syntax-rules ()
            ((alist a b . cs)
               (cons (cons a b) (alist . cs)))
            ((alist)
               '())
            ((alist x)
               (syntax-error "Uneven alist arguments. Last one: " 'x))))

      (define (alkeys al)
         (map car al))

      (define (alvalues al)
         (map cdr al))

      (example
         (alkeys (alist 'a 1 'b 2)) = '(a b)
         (alvalues (alist 'a 1)) = '(1))
))



