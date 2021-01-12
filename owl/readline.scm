#| doc
Interactive Input

This library is used by by the REPL to read input similarly to what the 
traditional readline library does.
|#
(define-library (owl readline)

   (export

      readline-default-options                   ;; settings to be passed for readline
      readline-result-stream
      readline
      port->readline-line-stream
      port->readline-byte-stream)

   (import
      (owl core)
      (owl math)
      (owl list)
      (owl string)
      (owl tuple)
      (owl syscall)
      (owl render)
      (owl lazy)
      (owl list-extra)
      (owl port)
      (owl ff)
      (owl terminal)
      (owl io)
      (owl sys)
      (owl sort)
      (scheme base)
      (scheme write)
      (only (owl unicode) utf8-decoder utf8-encode))


   (begin

      ;; show as much of right as fits after cx (cursor x)
      ;; return cursor to cx
      (define (update-line-right right w cx)
         (write-bytes stdout (clear-line-right null))
         (if (pair? right)
            (let ((visible-right (list->string (take right (- w cx)))))
               (display visible-right)
               (write-bytes stdout (cursor-left null (string-length visible-right))))))

      ;; → cx
      (define (update-line-left x y off left)
         (lets
            ((visible-left
               (list->string (drop (reverse left) off)))
             (cx (+ x (string-length visible-left))))
            (cursor-pos x y)
            (write-bytes stdout (clear-line-right null))
            (display visible-left) cx))

      ;; upgrade a possible string to readline state at end of it
      (define (history->state elem off)
         (cond
            ((string? elem) ;; compute a suitable offset
               (let ((len (string-length elem)))
                  (values
                     (reverse (string->list elem))
                     null
                     (max 0 (* (- (quotient len off) 1) off)))))
            (else
               (values (ref elem 1) (ref elem 2) (ref elem 3)))))

      (define (whitespace? x)
         (or (eq? x #\space)
             (eq? x #\tab)))

      (define (backspace-over-word left ll bs blanks?)
         (cond
            ((null? left)
               ll)
            ((whitespace? (car left))
               (if blanks?
                  (backspace-over-word (cdr left) (cons bs ll) bs #true)
                  ll))
            (blanks?
               (backspace-over-word left ll bs #false))
            (else
               (backspace-over-word (cdr left) (cons bs ll) bs #false))))

      ;; new state things:
      ;;    - alternate position
      ;;    - clipboard
      ;;    - undo buffer
      ;; todo: ctrl-i = tab
      ;; todo: ctrl-j = enter
      ;; todo: ctrl-t = transpose previous two chars
      ;; todo: ctrl-u = clear line up to cursor AND COPY
      ;; todo: ctrl-k add copying
      ;; todo: ctrl-x ctrl-u = incremental undo (for each line)
      ;; todo: ctrl-x ctrl-e = edit line in $EDITOR
      ;; todo: ctrl-x ctrl-v = display readline version
      ;; todo: ctrl-x ctrl-x = alternate cursor position (initially 0)
      ;; todo: alt-b = move one word back
      ;; todo: alt-f = move one word forward
      ;; todo: ctrl-u = delete beginning of line up to cursor
      ;; todo: ctrl-y = yank cursor position from clipboard
      ;; todo: ctrl-r = enter reverse history search
      ;;       ctrl-r in history search : get next from results
      ;;       match position highlighted (can be many per line)
      ;;       ctrl-
      ;; todo: ctrl-J = end an incremental search
      ;; todo: ctrl-G = end incremental search, restore current line

      (define readline-default-options
         (list->ff
            (list
               (cons 'backspace-out #false)     ;; return 'backspace if empty input is erased
               (cons 'autocomplete
                  ;; (byte ..) for autocomplete, ((byte ..) ...) for options
                  (λ (rl r) null)))))

      (define eof (tuple 'eof))

      (define (drop-trailing-space l)
         (let ((rl (reverse l)))
            (if (and (pair? rl) (eq? (car rl) #\space))
               (reverse (cdr rl))
               l)))
      
      (define (readline ll history x y w opts)
         (lets ((history (cons null history))  ; (newer . older)
                (offset-delta (+ 1 (quotient (- w x) 2)))
                (width (- w x)))
            (let loop ((ll ll) (hi history) (left null) (right null) (cx x) (off 0))
               (lets ((op ll (uncons ll eof)))
                  (tuple-case op
                     ((key k)
                        (let ((left (cons k left)) (cx (+ cx 1)))
                           (display (list->string (list k)))
                           (update-line-right right w cx)
                           (if (= cx w) (lets ((off (+ off offset-delta)) (visible-left (list->string (drop (reverse left) off)))) (cursor-pos x y) (write-bytes stdout (clear-line-right null)) (display visible-left) (update-line-right right w cx) (loop ll hi left right (+ x (string-length visible-left)) off)) (loop ll hi left right cx off))))
                     ((tab)
                        (lets ((data ((get opts 'autocomplete) left right)))
                           (cond
                              ((null? data)
                                 (loop ll hi left right cx off ))
                              ((pair? (car data)) ;; options
                                 (lets
                                    ((data (sort (λ (a b) (< (length a) (length b))) data))
                                     (opts
                                       (foldr 
                                          (λ (x data) (if (null? data) x (append x (cons #\space data)))) 
                                          null (map drop-trailing-space data)))
                                     (opts-fitting
                                        (take opts (min (- w cx) (length opts)))))
                                    (write-bytes stdout (font-dim opts-fitting))
                                    (write-bytes stdout (font-normal (cursor-left null (length opts-fitting))))
                                    (loop ll hi left right cx off)))
                              (else
                                 (loop
                                    (append (map (λ (x) (tuple 'key x)) data) ll)
                                    hi left right cx off )))))
                     ((backspace)
                        (if (= cx x)
                           (if (= off 0)
                              (if (get opts 'backspace-out)
                                 (values ll 'backspace)
                                 (loop ll hi left right cx off))
                              (lets ((off (- off offset-delta)) (visible-left (list->string (drop (reverse left) off))) (cx (+ x (string-length visible-left)))) (cursor-pos x y) (write-bytes stdout (clear-line-right null)) (display visible-left) (update-line-right right w cx) (loop (cons op ll) hi left right cx off)))
                           (let ((cx (- cx 1)))
                              (write-bytes stdout (cursor-left null 1))
                              (update-line-right right w cx)
                              (loop ll hi (cdr left) right cx off))))
                     ((delete)
                        (if (pair? right)
                           (let ((right (cdr right)))
                              (update-line-right right w cx)
                              (loop ll hi left right cx off))
                           (loop ll hi left right cx off)))
                     ((arrow dir)
                        (cond
                           ((eq? dir 'left)
                              (if (= cx x) ;; beginning
                                 (if (= off 0) ;; no scroll, nop
                                    (loop ll hi left right cx off)
                                    (lets ;; unscroll + recurse, shared
                                       ((off (- off offset-delta))
                                        (visible-left (list->string (drop (reverse left) off)))
                                        (cx (+ x (string-length visible-left))))
                                       (cursor-pos x y)
                                       (write-bytes stdout (clear-line-right null))
                                       (display visible-left)
                                       (update-line-right right w cx)
                                       (loop (cons op ll) hi left right cx off)))
                                 (begin
                                    (write-bytes stdout (cursor-left null 1))
                                    (loop ll hi (cdr left) (cons (car left) right) (- cx 1) off))))
                           ((eq? dir 'right)
                              (cond
                                 ((null? right) ;; no way to go
                                    (loop ll hi left right cx off))
                                 ((= cx w) ;; end, scroll + recurse, share
                                    (lets ((off (+ off offset-delta)) (visible-left (list->string (drop (reverse left) off))) (cx (+ x (string-length visible-left))))
                                       (cursor-pos x y)
                                       (write-bytes stdout (clear-line-right null))
                                       (display visible-left)
                                       (update-line-right right w cx)
                                       (loop (cons op ll) hi left right cx off)))
                                 (else
                                    (write-bytes stdout (cursor-right null 1))
                                    (loop ll hi (cons (car right) left) (cdr right) (+ cx 1) off))))
                           ((eq? dir 'up)
                              (cond
                                 ((null? (cdr hi)) ;; nothing oldr available
                                    (loop ll hi left right cx off))
                                 (else
                                    (lets ((new old hi)
                                           (current (tuple left right off))
                                           (left right off (history->state (car old) offset-delta))
                                           (cx (update-line-left x y off left)))
                                       (update-line-right right w cx)
                                       (loop ll (cons (cons current new) (cdr old)) left right cx off)))))
                           ((eq? dir 'down)
                              (cond
                                 ((null? (car hi)) ;; nothing newer available
                                    (loop ll hi left right cx off))
                                 (else
                                    (lets
                                       ((new old hi)
                                        (current (tuple left right off))
                                        (left right off (history->state (car new) offset-delta))
                                        (cx (update-line-left x y off left)))
                                       (update-line-right right w cx)
                                       (loop ll (cons (cdr new) (cons current old)) left right cx off)))))
                           (else
                              (loop ll hi left right cx off))))
                     ((enter)
                        (values ll (list->string (append (reverse left) right))))
                     ((ctrl key)
                        (case key
                           ((a) ;; beginning of line
                              (let ((right (append (reverse left) right)))
                                 (cursor-pos x y)
                                 (update-line-right right w x) (loop ll hi null right x 0)))
                           ((e)
                              (loop (fold (λ (ll x) (cons (tuple 'arrow 'right) ll)) ll (iota 0 1 (length right))) hi left right cx off))
                           ((w)
                              (let ((bs (tuple 'backspace)))
                                 (loop (backspace-over-word left ll bs #true) hi left right cx off)))
                           ((p)
                              (loop (cons (tuple 'arrow 'up) ll) hi left right cx off))
                           ((n)
                              (loop (cons (tuple 'arrow 'down) ll) hi left right cx off))
                           ((b)
                              (loop (cons (tuple 'arrow 'left) ll) hi left right cx off))
                           ((f)
                              (loop (cons (tuple 'arrow 'right) ll) hi left right cx off))
                           ((d)
                              (loop (cons (tuple 'delete) ll) hi left right cx off))
                           ((u)
                              (loop
                                 (append (map (λ (x) (tuple 'backspace)) left) ll)
                                 hi left right cx off))
                           ((k)
                              (write-bytes stdout (clear-line-right null))
                              (loop ll hi left null cx off))
                           (else
                              (loop ll hi left right cx off))))
                     ((esc)
                        (values ll #false))
                     ((eof)
                        (values ll #false))
                     (else
                        (loop ll hi left right cx off)))))))

      (define (get-dimensions ll)
        (lets ((w h ll (get-terminal-size ll))
               (x y ll (get-cursor-position ll)))
              (values x y w ll)))

      (define editable-readline
         (case-lambda
            (()
               (lets ((x y w ll (get-dimensions (terminal-input empty))))
                  (readline ll null x y w readline-default-options)))
            ((ll)
               (lets ((x y w ll (get-dimensions ll)))
                  (readline ll null x y w readline-default-options)))
            ((ll history)
               (lets ((x y w ll (get-dimensions ll)))
                  (readline ll history x y w readline-default-options)))
            ((ll history opts)
               (lets ((x y w ll (get-dimensions ll)))
                  (readline ll history x y w opts)))))

      (define (history-cons val lst)
         (if (or (equal? val "")
                (and (pair? lst)
                   (equal? val (car lst))))
             lst
             (cons val lst)))

      (define (readline-result-stream history prompt merger finish opts)
         (let loop ((history history)
                    (ll (terminal-input empty)))
            (if prompt (prompt))
            (set-terminal-rawness #true)
            (lets ((ll res (editable-readline ll history opts)))
               (set-terminal-rawness #false)
               (if res
                  (merger res (λ () (loop (history-cons res history) ll)))
                  (finish loop ll)))))

      (define (port->readline-byte-stream port)
         (readline-result-stream
            #null #false
            (λ (line ls)
               ;; enter was pressed so echo it
               (write-bytevector #(10) stdout)
               (append (string->bytes line)
                  (cons #\newline ls)))
            (λ (loop ll) #null) readline-default-options))

      (define (port->readline-line-stream port prompt x)
         (readline-result-stream
            #null
            (λ () (display prompt))
            cons
            (λ (loop ll) #null) readline-default-options))))

