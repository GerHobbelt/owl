;; todo: separate writing and generating functions properly

(define-library (owl terminal)

   (export

      set-terminal-rawness

      font-normal          ;; lst → lst'
      font-bright          ;; lst → lst'
      font-dim             ;; lst → lst'
      font-standard        ;; lst → lst'
      font-reverse         ;; lst → lst'
      font-attrs           ;; lst a b c → lst'

      font-gray

      font-fg-black
      font-fg-red
      font-fg-green
      font-fg-yellow
      font-fg-blue
      font-fg-magenta
      font-fg-cyan
      font-fg-white
      font-bg-black
      font-bg-red
      font-bg-green
      font-bg-yellow
      font-bg-blue
      font-bg-magenta
      font-bg-cyan
      font-bg-white

      clear-screen         ;; lst → lst'
      clear-screen-top     ;; lst → lst'
      clear-screen-bottom  ;; lst → lst'
      clear-line           ;; lst → lst'
      clear-line-left      ;; lst → lst'
      clear-line-right     ;; lst → lst'
      set-cursor           ;; lst x y → lst'

      cursor-hide          ;; lst → lst'
      cursor-show          ;; lst → lst'
      cursor-save          ;; lst → lst'
      cursor-restore       ;; lst → lst'

      cursor-up            ;; lst n → lst'
      cursor-down          ;; lst n → lst'
      cursor-left          ;; lst n → lst'
      cursor-right         ;; lst n → lst'

      enable-line-wrap     ;; lst n → lst'
      disable-line-wrap    ;; lst n → lst'

      set-scroll-range     ;; lst y1 y2 → lst'
      scroll-up            ;; lst → lst'
      scroll-down          ;; lst → lst'

      cursor-pos

      terminal-input
      get-terminal-size
      get-cursor-position
      )

   (import
      (owl core)
      (owl math)
      (owl primop)
      (owl list)
      (owl string)
      (owl tuple)
      (owl syscall)
      (owl render)
      (owl lazy)
      (owl list-extra)
      (owl port)
      (owl lcd ff)
      (scheme base)
      (scheme write)
      (owl io)
      (only (owl unicode) utf8-decoder utf8-encode)
      (owl sys))


   (begin

      (define esc 27)

      (define (num->bytes n tl)
         (cond
            ((eq? n 1) (cons #\1 tl))
            ((eq? (type n) type-fix+)
              (append (string->list (number->string n 10)) tl))
            (else
              (print-to stderr "num->bytes: bad pos " n)
              (cons #\0 tl))))

      ;;; ^[[<n><op>
      (define (unary-op n op)
         (write-bytes stdout
            (ilist esc #\[ (num->bytes n (list op)))))

      (define (set-terminal-rawness rawp)
         (sys-prim 26 rawp #f #f))

      ;;; Text mode

      ;; attributes
      (define (font-normal lst)    (ilist esc #\[     #\m lst))
      (define (font-bright lst)    (ilist esc #\[ #\1 #\m lst))
      (define (font-dim lst)       (ilist esc #\[ #\2 #\m lst))
      (define (font-standard lst)  (ilist esc #\[ #\3 #\m lst))
      (define (font-reverse lst)   (ilist esc #\[ #\7 #\m lst))

      (define (font-gray lst)
         (ilist esc #\[ #\1 #\; #\3 #\0 #\m lst))

      (define (font-fg-black lst)   (ilist esc #\[ #\3 #\0 #\m lst))
      (define (font-fg-red lst)     (ilist esc #\[ #\3 #\1 #\m lst))
      (define (font-fg-green lst)   (ilist esc #\[ #\3 #\2 #\m lst))
      (define (font-fg-yellow lst)  (ilist esc #\[ #\3 #\3 #\m lst))
      (define (font-fg-blue lst)    (ilist esc #\[ #\3 #\4 #\m lst))
      (define (font-fg-magenta lst) (ilist esc #\[ #\3 #\5 #\m lst))
      (define (font-fg-cyan lst)    (ilist esc #\[ #\3 #\6 #\m lst))
      (define (font-fg-white lst)   (ilist esc #\[ #\3 #\7 #\m lst))

      (define (font-bg-black lst)   (ilist esc #\[ #\4 #\0 #\m lst))
      (define (font-bg-red lst)     (ilist esc #\[ #\4 #\1 #\m lst))
      (define (font-bg-green lst)   (ilist esc #\[ #\4 #\2 #\m lst))
      (define (font-bg-yellow lst)  (ilist esc #\[ #\4 #\3 #\m lst))
      (define (font-bg-blue lst)    (ilist esc #\[ #\4 #\4 #\m lst))
      (define (font-bg-magenta lst) (ilist esc #\[ #\4 #\5 #\m lst))
      (define (font-bg-cyan lst)    (ilist esc #\[ #\4 #\6 #\m lst))
      (define (font-bg-white lst)   (ilist esc #\[ #\4 #\7 #\m lst))

      (define (font-attrs lst a b c)
         (ilist esc #\[ (render a (cons #\; (render b (cons #\; (render c (cons #\m lst))))))))

      ;;; Clearing content

      (define (clear-line lst)       (ilist esc #\[ #\2 #\K lst))
      (define (clear-line-right lst) (ilist esc #\[ #\K lst))
      (define (clear-line-left lst)  (ilist esc #\[ #\1 #\K lst))

      (define (clear-screen lst)     (ilist esc #\[ #\2 #\J lst))
      (define (clear-screen-top lst) (ilist esc #\[ #\1 #\J lst))
      (define (clear-screen-bottom lst) (ilist esc #\[ #\J lst))

      ;;; Wrapping

      (define (enable-line-wrap lst)     (ilist esc #\[ #\7 #\h lst))
      (define (disable-line-wrap lst)    (ilist esc #\[ #\7 #\l lst))

      ;;; Scrolling

      (define (set-scroll-range lst a b)
         (ilist esc #\[ (render a (cons #\; (render b (cons #\r lst))))))

      (define (scroll-up   lst) (ilist esc #\[ #\M lst))
      (define (scroll-down lst) (ilist esc #\[ #\D lst))

      ;;; Terminal input stream

      (define (get-natural ll def)
         (let loop ((n 0) (first? #true) (ll ll))
            (lets ((x ll (uncons ll #false)))
               (cond
                  ((not x) (values def ll))
                  ((< 47 x 58) (loop (+ (* n 10) (- x 48)) #false ll))
                  (first? (values def (cons x ll)))
                  (else (values n (cons x ll)))))))

      (define (get-imm ll val)
         (lets ((x ll (uncons ll #false)))
            (if (eq? x val)
               (values x ll)
               (values #false (cons x ll)))))

      ;; convert this to a proper stream parser later
      (define (terminal-input . opt)
         (let ((port (if (null? opt) stdin (car opt))))
            (let loop
               ((ll
                  (utf8-decoder
                     (port->byte-stream port)
                     (λ (loop line ll) (print-to stderr "Bad UTF-8 in terminal input") (loop line ll)))))
               (cond
                  ((pair? ll)
                     (lets ((hd ll ll))
                        (cond
                           ((eq? hd esc) ;; decode escape sequence
                              (lets ((op ll (uncons ll #false)))
                                 (cond
                                    ((eq? op 91) ;; [
                                       (lets ((op ll (uncons ll #false)))
                                          (cond
                                             ((eq? op 65) (cons (tuple 'arrow 'up) (loop ll)))
                                             ((eq? op 66) (cons (tuple 'arrow 'down) (loop ll)))
                                             ((eq? op 67) (cons (tuple 'arrow 'right) (loop ll)))
                                             ((eq? op 68) (cons (tuple 'arrow 'left) (loop ll)))
                                             (else (lets ((a ll (get-natural (cons op ll) #false))) (if a (lets ((x ll (uncons ll #false))) (cond ((not x) null) ((eq? x #\;) (lets ((b ll (get-natural ll #false))) (if b (lets ((op ll (uncons ll #false))) (if op (cond ((eq? op #\R) (cons (tuple 'cursor-position a b) (loop ll))) (else (cons (tuple 'esc-unknown-binop a ";" b (list->string (list op))) null))) null)) null))) ((and (eq? a 3) (eq? x #\~)) (cons (tuple 'delete) (loop ll))) (else (cons (tuple 'esc-unknown-unary-op a (list->string (list x))) (loop ll))))) null))))))
                                    ((eq? op 79)
                                       (lets ((next ll (uncons ll #false)))
                                          (cond
                                             ((eq? next 68) (cons (tuple 'ctrl 'arrow-left) (loop ll)))
                                             ((eq? next 67) (cons (tuple 'ctrl 'arrow-right) (loop ll)))
                                             ((eq? next 65) (cons (tuple 'ctrl 'arrow-up) (loop ll)))
                                             ((eq? next 66) (cons (tuple 'ctrl 'arrow-down) (loop ll)))
                                             (else (cons (tuple 'esc) (loop (ilist 79 next ll)))))))
                                    (else
                                       (cons (tuple 'esc)
                                          (loop (cons op ll)))))))
                           ((eq? hd 127)
                              (cons (tuple 'backspace) (loop ll)))
                           ((eq? hd 13)
                              (cons (tuple 'enter) (loop ll)))
                           ((eq? hd 21)
                              (cons (tuple 'nak) (loop ll))) ;; ^u
                           ((eq? hd 3)
                              (cons (tuple 'end-of-text) (loop ll))) ;; ^c
                           ((eq? hd 4)
                              (cons (tuple 'end-of-transmission) (loop ll))) ;; ^d
                           ((eq? hd 1)
                              (cons (tuple 'ctrl 'a) (loop ll))) ;; ^n
                           ((eq? hd 5)
                              (cons (tuple 'ctrl 'e) (loop ll))) ;; ^w
                           ((eq? hd 6)
                              (cons (tuple 'ctrl 'f) (loop ll)))
                           ((eq? hd 9)
                              (cons (tuple 'tab) (loop ll)))
                           ((eq? hd 2)
                              (cons (tuple 'ctrl 'b) (loop ll)))
                           ((eq? hd  8)
                              (cons (tuple 'ctrl 'h) (loop ll)))
                           ((eq? hd 18)
                              (cons (tuple 'ctrl 'r) (loop ll)))
                           ((eq? hd 14)
                              (cons (tuple 'ctrl 'n) (loop ll)))
                           ((eq? hd 16)
                              (cons (tuple 'ctrl 'p) (loop ll)))
                           ((eq? hd 11)
                              (cons (tuple 'ctrl 'k) (loop ll)))
                           ((eq? hd 12)
                              (cons (tuple 'ctrl 'l) (loop ll)))
                           ((eq? hd 24)
                              (cons (tuple 'ctrl 'x) (loop ll)))
                           ((eq? hd 23)
                              (cons (tuple 'ctrl 'w) (loop ll)))
                           ((eq? hd 22)
                              (lets ((val ll (uncons ll 0)))
                                 (cons (tuple 'key val) ;; force treatment as a key
                                    (loop ll))))
                           (else
                              (cons (tuple 'key hd) (loop ll))))))
                  ((null? ll) ll)
                  (else (λ () (loop (ll))))))))

      ;;; Cursor movement

      (define (cursor-pos x y)
         (write-bytes stdout
            (ilist esc #\[ (num->bytes y (cons #\; (num->bytes x (list #\f)))))))

      (define (set-cursor lst x y)
         (if (and (> x 0) (> y 0))
            (ilist esc #\[ (num->bytes y (cons #\; (num->bytes x (cons #\f lst)))))
            (error "set-cursor: bad position " (cons x y))))

      (define (cursor-up lst n)
         (ilist esc #\[ (num->bytes n (cons #\A lst))))

      (define (cursor-down lst n)
         (ilist esc #\[ (num->bytes n (cons #\B lst))))

      (define (cursor-right lst n)
         (ilist esc #\[ (num->bytes n (cons #\C lst))))

      (define (cursor-left lst n)
         (ilist esc #\[ (num->bytes n (cons #\D lst))))

      (define (cursor-hide lst)
         (ilist esc #\[ #\? #\2 #\5 #\l lst))

      (define (cursor-show lst)
         (ilist esc #\[ #\? #\2 #\5 #\h lst))

      (define (cursor-save lst)
         (ilist esc #\[ #\s lst))

      (define (cursor-restore lst)
         (ilist esc #\[ #\u lst))

      (define (cursor-top-left n)
         (write-bytevector #(27 #\[ #\H) stdout))

      ;; Interaction with terminal

      ;; ^[6n = get cursor position ^[<x>;<y>R
      ;; ^[5n = check terminal status -> ^[0n = ok, ^[3n = not ok
      ;; ^[[c = get terminal type ->
      ;; input: up    esc 91 65
      ;;        down  esc 91 66
      ;;        right esc 91 67
      ;;        left  esc 91 68
      ;;        enter 13
      ;;        bs    127
      ;;        ^K    11  -- remove line right
      ;;        ^U    21  -- remove line left

      (define (wait-cursor-position ll)
         (let loop ((head null) (ll ll))
            (lets
               ((this ll (uncons ll #false)))
               (cond
                  ((not this) (values #false #false ll))
                  ((eq? 'cursor-position (ref this 1))
                     (values (ref this 3) (ref this 2) (append (reverse head) ll)))
                   (else
                      (loop (cons this head) ll))))))

      ;; ll → cols rows ll'
      (define (get-cursor-position ll)
         ;; request cursor position
         (write-bytevector #(27 #\[ #\6 #\n) stdout)
         (wait-cursor-position ll))

      (define (get-terminal-size ll)
         (lets ((x y ll (get-cursor-position ll))
                (res (cursor-pos 4095 4095))
                (xm ym ll (get-cursor-position ll)))
            (cursor-pos x y) (values xm ym ll)))

))
