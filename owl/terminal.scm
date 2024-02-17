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
      (owl list)
      (owl tuple)
      (owl syscall)
      (owl render)
      (owl lazy)
      (owl list-extra)
      (owl port)
      (prefix (owl parse) get-)
      (owl ff)
      (scheme base)
      (owl io)
      (only (owl unicode) utf8-decoder utf8-encode)
      )


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

      ;;; Terminal input stream

      (define get-small-char
         (get-parses
            ((cp
               (get-rune-if
                  (λ (x) (> x 31))))) ;; 127 is also handled specially
           (tuple 'key cp)))

      ;; esc[X
      (define bracket-chars
         (list->ff
            (list  ;  X
               (cons 65 (tuple 'arrow 'up))
               (cons 66 (tuple 'arrow 'down))
               (cons 67 (tuple 'arrow 'right))
               (cons 68 (tuple 'arrow 'left)))))

      ;; esc[X~
      (define tilde-chars
         (list->ff
            (list ;   X
               (cons #\1 (tuple 'home))
               (cons #\4 (tuple 'end))
               (cons #\5 (tuple 'page-up))
               (cons #\6 (tuple 'page-down))
               (cons #\2 (tuple 'insert)))))

      (define special-keys
         (list->ff
            (list
               (cons 127 (tuple 'backspace))
               (cons   1 (tuple 'ctrl 'a))
               (cons   2 (tuple 'ctrl 'b))
               (cons   3 (tuple 'ctrl 'c))
               ;; ^d not handled, currently causes by default parser to stop
               (cons   5 (tuple 'ctrl 'e))
               (cons   6 (tuple 'ctrl 'f))
               (cons   7 (tuple 'ctrl 'g))
               (cons   8 (tuple 'ctrl 'h))
               (cons   9 (tuple 'tab))     ;; aka C-i
               (cons  10 (tuple 'ctrl 'j))
               (cons  11 (tuple 'ctrl 'k))
               (cons  12 (tuple 'ctrl 'l))
               (cons  13 (tuple 'enter))   ;; aka C-m
               (cons  14 (tuple 'ctrl 'n))
               (cons  15 (tuple 'ctrl 'o))
               (cons  16 (tuple 'ctrl 'p))
               (cons  17 (tuple 'ctrl 'q))
               (cons  18 (tuple 'ctrl 'r))
               (cons  19 (tuple 'ctrl 's))
               (cons  20 (tuple 'ctrl 't))
               (cons  21 (tuple 'ctrl 'u))
               ;; C-v handled specially to quote chars
               (cons  23 (tuple 'ctrl 'w))
               (cons  24 (tuple 'ctrl 'x))
               (cons  25 (tuple 'ctrl 'y))
               (cons  26 (tuple 'ctrl 'z))
               )))

      (define (get-by-ff ff)
         (get-parses
            ((byte (get-byte-if (λ (x) (get ff x #f)))))
            (get ff byte #f)))

      (define get-natural
         (get-parses
            ((bs (get-plus! (get-byte-if (λ (x) (and (<= #\0 x) (<= x #\9)))))))
            (fold (λ (n b) (+ (* n 10) (- b #\0))) 0 bs)))

      (define get-nonblocking-plain-escape
         (get-parses
            ((skip (get-imm esc))
             (is (get-input-ready? stdin))
             (verify
                (begin
                   ;(print (list 'input 'ready 'is is))
                    (eq? is #false)) 'foo))
            (tuple 'esc)))

      (define get-plain-escape
         (get-parses
            ((skip (get-imm esc)))
            (tuple 'esc)))
      
      (define get-esc-caret-sequence
         (get-parses
            ((skip (get-imm #\[))
             (val
                (get-one-of
                   (get-by-ff bracket-chars)
                   (get-parses
                      ((val (get-by-ff tilde-chars))
                       (skip (get-imm #\~)))
                      val)
                   (get-parses
                      ((a get-natural)
                       (skip (get-imm  #\;))
                       (b get-natural)
                       (skip (get-imm #\R)))
                      (tuple 'cursor-position a b)))))
            val))

      (define get-escape-sequence
         (get-parses
            ((skip (get-imm esc))
             (val get-esc-caret-sequence))
            val))

      (define get-special-key
         (get-by-ff special-keys))

      (define get-quoted-key
         (get-parses
            ((skip (get-imm 22)) ;; ctrl-v
             (val get-byte)) ;; force it to be treated as a key
            (tuple 'key val)))

      (define get-logged
         (get-byte-if (λ (x) (print "NOT " x) #f)))

      (define get-terminal-input
         (get-one-of
            get-special-key
            get-nonblocking-plain-escape    ;; must be before escape sequence to avoid blocking
            get-escape-sequence ;; esc[ ... stuff
            get-plain-escape    ;; not an escape sequence, could merge with above      
            get-small-char
            get-quoted-key   ;; ∀ x, ctrl-v <x> → #(key <x>)
            ))

      (define (terminal-input opts)
         (set-terminal-rawness #true)
         (get-byte-stream->exp-stream
            (port->byte-stream
               (get opts 'port stdin))
            get-terminal-input
            (λ (cont ll error)
               (cond
                  ((and (= (car ll) 4) (get opts 'eof-exit? #t))
                     ;; ^d → quit, unless otherwise requested
                     null)
                  (else
                     (print-to stderr "ERROR: weird terminal input: " ll)
                     (cont (cdr ll)))))))


      ;; Interaction with terminal

      ;; ^[6n = get cursor position ^[<x>;<y>R
      ;; ^[5n = check terminal status -> ^[0n = ok, ^[3n = not ok
      ;; ^[[c = get terminal type ->

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
