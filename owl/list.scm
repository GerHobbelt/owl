#| doc
Basic List Functions

Operations in this library depend only on primitive operations. The
rest of the usual operations typically depend on numbers, which are
implemented in (owl math).
|#

(define-library (owl list)

   (export
      pair? null null?
      caar cadr cdar cddr
      car* cdr*
      list?
      zip foldl foldr fold map for-each
      mem memq assq getq
      last
      fold-map foldr-map
      append concatenate
      reverse
      filter remove separate
      keep
      every any
      unfold
      first find find-tail
      take-while                ;; pred, lst -> as, bs
      break
      split                     ;; split pred lst -> lst'
      fold2
      halve
      interleave
      diff union intersect
      ╯°□°╯)

   (import
      (owl core)
      (owl proof)
      (only (owl syscall) error))

   (begin

      (define null '())

      ;; any -> bool
      (define (pair? x) (eq? type-pair (type x)))

      ;; any -> bool
      (define null? (C eq? #n))

      ;; (a . b) -> a, _ -> _
      (define (car* a) (if (pair? a) (car a) a))

      ;; (a . b) -> b, _ -> _
      (define (cdr* a) (if (pair? a) (cdr a) a))

      ;; '((a . b) . c) -> a
      (define caar (B car car))
      ;; '(a . (b . c)) -> b
      (define cadr (B car cdr))
      ;; '((a . b) . c) -> b
      (define cdar (B cdr car))
      ;; '(a . (b . c)) -> c
      (define cddr (B cdr cdr))

      ;; any -> bool, check if a thing is a linked list, O(n)
      (define (list? l)
         (cond
            ((null? l) #true)
            ((pair? l) (list? (cdr l)))
            (else #false)))

      ;; fn as bs -> ((fn a b) ...), zip values of lists together with a function
      (define (zip op a b)
         (cond
            ((null? a) a)
            ((null? b) b)
            (else
               (let ((hd (op (car a) (car b))))
                  (cons hd (zip op (cdr a) (cdr b)))))))

      ;; op state lst -> state', walk over a list from left and compute a value
      (define (foldl op state lst)
         (if (null? lst)
            state
            (foldl op
               (op state (car lst))
               (cdr lst))))

      (define fold foldl)

      (example
         (zip cons '(1 2 3) '(a b c d)) = '((1 . a) (2 . b) (3 . c)))

      (define (unfold op st end?)
         (if (end? st)
            #n
            (lets ((st this (op st)))
               (cons this (unfold op st end?)))))

      ;; op s1 s2 lst -> s1' s2', fold with 2 states
      (define (fold2 op s1 s2 lst)
         (if (null? lst)
            (values s1 s2)
            (lets ((s1 s2 (op s1 s2 (car lst))))
               (fold2 op s1 s2 (cdr lst)))))

      ;; op st lst -> st', compute a value from the right
      ;;    (foldr - 0 '(1 2 3)) = 2
      (define (foldr op st lst)
         (if (null? lst)
            st
            (op (car lst)
               (foldr op st (cdr lst)))))

      (example (foldr cons #n '(a b c)) = '(a b c))

      ;; fn lst -> lst', run a function to all elements of a list
      (define (map fn lst)
         (if (null? lst)
            #n
            (lets
               ((hd tl lst)
                (hd (fn hd))) ;; compute head first
               (cons hd (map fn tl)))))

      (example
         (map not '(#false #false #true)) = '(#true #true #false))

      ;; fn lst -> _, run a function to all elements of a list for side effects
      (define (for-each op lst)
         (if (null? lst)
            #n
            (begin
               (op (car lst))
               (for-each op (cdr lst)))))

      (define (mem pred lst x)
         (cond
            ((null? lst) #false)
            ((pred x (car lst)) lst)
            (else (mem pred (cdr lst) x))))

      (define (memq x lst)
         (cond
            ((null? lst) #false)
            ((eq? (car lst) x) lst)
            (else (memq x (cdr lst)))))

      ;; key list -> pair | #f, get a pair from an association list
      (define (assq k lst)
         (cond
            ((null? lst) #false)
            ((eq? k (car (car lst))) (car lst))
            (else (assq k (cdr lst)))))

      (define (getq lst k def)
         (cond
            ((null? lst) def)
            ((eq? (car (car lst)) k)
               (cdr (car lst)))
            (else
               (getq (cdr lst) k def))))

      (example
         (mem eq? '(1 2 3) 2) = '(2 3)
         (assq 'a '((a . 1) (b . 2))) = '(a . 1)
         (assq 'c '((a . 1) (b . 2))) = #false)


      ;; last list default -> last-elem | default, get the last value of a list
      (define (last l def)
         (fold (λ (a b) b) def l))

      (example
         (last '(1 2 3) 'a) = 3
         (last '() 'a) = 'a)

      (define (append2 a b)
         (if (null? a)
            b
            (cons (car a) (append2 (cdr a) b))))

      ;; list-of-lists -> list, SRFI-1
      (define (concatenate lst)
         (if (pair? lst)
            (if (null? (cdr lst))
               (car lst) ; don't recurse down the list just to append nothing
               (append2 (car lst) (concatenate (cdr lst))))
            lst))

      ;; append list ... -> list', join lists
      ;;    (append '(1) '() '(2 3)) = '(1 2 3)
      (define append
         (case-lambda
            ((a b) (append2 a b))
            (lst (concatenate lst))))

      (example
         (append '(1 2 3) '(a b c)) = '(1 2 3 a b c))

      ;(define (reverse l) (fold (λ (r a) (cons a r)) #n l))

      (define (rev-loop a b)
         (if (null? a)
            b
            (rev-loop (cdr a) (cons (car a) b))))

      ;; lst -> lst', reverse a list
      (define reverse (C rev-loop #n))

      (example
         (reverse '(1 2 3)) = '(3 2 1))

      ;; misc

      (define (first pred lst def)
         (cond
            ((null? lst)
               def)
            ((pred (car lst))
               (car lst))
            (else
               (first pred (cdr lst) def))))

      ;; todo: move to srfi-1?
      (define (find pred lst)
         (first pred lst #f))

      (example
         (find null? '(1 2 3)) = #f
         (find null? '(1 ())) = ())

      ;; pred lst -> tail-list | #f, SRFI-1
      (define (find-tail pred lst)
         (and
            (pair? lst)
            (if (pred (car lst))
               lst
               (find-tail pred (cdr lst)))))

      (define (take-while pred lst)
         (let loop ((lst lst) (taken #n))
            (cond
               ((null? lst) (values (reverse taken) #n))
               ((pred (car lst)) (loop (cdr lst) (cons (car lst) taken)))
               (else (values (reverse taken) lst)))))

      ;; pred lst -> head-list tail-list, SRFI-1
      (define (break pred lst)
         (if (pair? lst)
            (if (pred (car lst))
               (values '() lst)
               (lets ((l r (break pred (cdr lst))))
                  (values (cons (car lst) l) r)))
            (values '() lst)))

      (define (split pred lst)
         (lets ((h t (break pred lst)))
            (if (pair? t)
               (cons h (split pred (cdr t)))
               (list h))))

      (example
         (split (lambda (x) (eq? x 'x)) '(a b x c d x e)) = '((a b) (c d) (e)))

      (define (keep pred lst)
         (foldr (λ (x tl) (if (pred x) (cons x tl) tl)) #n lst))

      ;; pred lst -> 'list, SRFI-1
      (define filter keep)

      ;; pred lst -> 'list, SRFI-1
      (define (remove pred lst)
         (filter (B not pred) lst))

      (example
         let l = '(1 2 () 3 () 4)
         (filter null? l) = '(() ())
         (remove null? l) = '(1 2 3 4))

      (define (separate lst pred)
         (let loop ((lst lst) (is '()) (isnt '()))
            (cond
               ((null? lst)
                  (values
                     (reverse is)
                     (reverse isnt)))
               ((pred (car lst))
                  (loop (cdr lst) (cons (car lst) is) isnt))
               (else
                  (loop (cdr lst) is (cons (car lst) isnt))))))

      (example
         let l = '(#t (1 2) #f (3))
         (separate l pair?) = (values '((1 2) (3)) '(#t #f)))

      (define (every pred lst)
         (or (null? lst) (and (pred (car lst)) (every pred (cdr lst)))))

      (define (any pred lst)
         (and (pair? lst) (or (pred (car lst)) (any pred (cdr lst)))))

      (let ((l '(#t #f ())))
         (example
            (any null? l) = #true
            (every null? l) = #false))

      (define (fold-map o s l)
         (let loop ((s s) (l l) (r #n))
            (if (null? l)
               (values s (reverse r))
               (lets ((s a (o s (car l))))
                  (loop s (cdr l) (cons a r))))))

      (define (foldr-map o s l)
         (if (null? l)
            (values s #n)
            (lets
               ((a (car l))
                (s l (foldr-map o s (cdr l))))
               (o a s))))

      (define (diff a b)
         (cond
            ((null? a) a)
            ((memq (car a) b)
               (diff (cdr a) b))
            (else
               (cons (car a)
                  (diff (cdr a) b)))))

      (define (union a b)
         (cond
            ((null? a) b)
            ((memq (car a) b)
               (union (cdr a) b))
            (else
               (cons (car a)
                  (union  (cdr a) b)))))

      (define (intersect a b)
         (cond
            ((null? a) #n)
            ((memq (car a) b)
               (cons (car a)
                  (intersect (cdr a) b)))
            (else
               (intersect (cdr a) b))))

      (let ((abc '(a b c)) (def '(d e f)) (cd '(c d)))
         (example
            (diff abc abc) = ()
            (union abc def) = '(a b c d e f)
            (intersect abc cd) = '(c)
            (diff abc cd) = (diff abc (intersect abc cd))))

      (define (interleave mid lst)
         (if (null? lst)
            #n
            (let loop ((a (car lst)) (as (cdr lst)))
               (if (null? as)
                  (list a)
                  (ilist a mid (loop (car as) (cdr as)))))))

      ;; lst → a b, a ++ b == lst, length a = length b | length b + 1
      (define (halve lst)
         (let walk ((t lst) (h lst) (out #n))
            (if (null? h)
               (values (reverse out) t)
               (let ((h (cdr h)))
                  (if (null? h)
                     (values (reverse (cons (car t) out)) (cdr t))
                     (walk (cdr t) (cdr h) (cons (car t) out)))))))

      (lets ((l '(a b c d e f)))
         (example
            l = (lets ((head tail (halve l))) (append head tail))))

      (define ╯°□°╯ reverse)

      (example
         (interleave 'x '(a b c)) = '(a x b x c)
         (interleave 'x '()) = ()
         (halve '(a b c d)) = (values '(a b) '(c d))
         (halve '(a b c d e)) = (values '(a b c) '(d e)))

))

