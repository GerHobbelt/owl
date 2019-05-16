;;;
;;; HTTP server
;;;

(define-library (owl http)

   (export 
      parse-http)

   (import
      (owl base)
      (owl unicode)
      (prefix (owl parse) get-)
      (owl env))

   (begin

      ;;;
      ;;; HTTP parsing
      ;;;

      (define get-nonspace
         (get-byte-if (λ (x) (not (eq? x #\space)))))

      (define get-crlf
         (get-parses
            ((skip (get-either (get-imm #\return) (get-epsilon #f)))
             (skip (get-imm #\newline)))
            #t))
    
      ;; temp 
      (define get-query
         (get-parses
            ((chars (get-greedy-plus get-nonspace)))
            (list->string chars)))
      
      (define maybe-get-query-params
         (get-either
            (get-parses
               ((skip (get-imm #\?))
                (vals (get-greedy-plus get-nonspace))) ;; temp
               (list->string vals))
            (get-epsilon #f)))
             
      (define get-first-line
         (get-parses
            ((method 
               (get-one-of
                  (get-word "GET" 'get)
                  (get-word "POST" 'post)
                  (get-word "HEAD" 'head)
                  (get-word "OPTIONS" 'options)
                  (get-word "DELETE" 'delete)
                  (get-word "PUT" 'put)
                  (get-word "PATCH" 'patch)
                  (get-word "TRACE" 'trace)
                  (get-word "CONNECT" 'connect)))
             (skip (get-imm #\space))
             (query get-query)
             (query-params maybe-get-query-params)
             (skip (get-word " HTTP/1." 'foo))
             (ver-char (get-either (get-imm #\0) (get-imm #\1)))
             (skip get-crlf))
            (-> #empty
               (put 'method method)
               (put 'query query)
               )))

      (define get-header-line
         (get-parses
            ((key (get-greedy-plus (get-byte-if (λ (x) (not (eq? x #\:))))))
             (skip (get-imm #\:))
             (skip (get-maybe (get-imm #\space) #f))
             (val (get-greedy-plus (get-byte-if (λ (x) (not (eq? x #\newline))))))
             ;(val (get-greedy-plus (get-byte-if (λ (x) (and (not (eq? x #\return) (not (eq? x #\newline))))))))
             (skip get-crlf)
             )
            (cons (list->string key) (list->string val))
            ;(cons key #f)
            ))
       
      ;; doing string->symbol on all would cause memory leak
      (define (known-header->symbol str)
         (cond
            ((string-ci=? str "Host") 'host)
            ((string-ci=? str "User-Agent") 'user-agent)
            ((string-ci=? str "Referer") 'referer) ;; originally Referrer
            ((string-ci=? str "Origin") 'origin) ;; Origin > Referer, https://wiki.mozilla.org/Security/Origin
            ((string-ci=? str "X-sid") 'x-sid)
            ((string-ci=? str "Cookie") 'cookie)
            ((string-ci=? str "Content-type") 'content-type)
            ((string-ci=? str "Content-length") 'content-length)
            ((string-ci=? str "Accept-Language") 'accept-language)
            ((string-ci=? str "Accept") 'accept) ;; text/html, text/plain, ...
            ((string-ci=? str "Accept-Encoding") 'accept-encoding)
            (else #false)))

      (define get-http 
         (get-parses
            ((req get-first-line)
             (headers (get-greedy-plus get-header-line))
             (skip get-crlf))
            (put req 'headers headers)
            ))
               
      (define (hex-val a)
         (cond
            ((not a) #false)
            ((< 47 a 58) (- a 48))
            ((< 96 a 103) (- a 87))
            ((< 64 a 71) (- a 55))
            (else #false)))

      (define get-byte-url-decoding
         (get-either
            (get-parses
               ((skip (get-imm #\+)))
               #\space)
            ;(get-parses
            ;   ((skip (get-imm #\%))
            ;    (a get-hex)
            ;    (b get-hex))
            ;   (+ (<< a 4) b))
            (get-parses
               ((val get-rune)) ;; <- overly permissive, but works for now
               val)))


   (define (parse-http-or lst val)
      (cond
         ((string? lst)
            (parse-http-or (string->list lst) val))
         (else
            (get-first-match get-http lst val)))) 

   (define parse-http             
      (case-lambda
         ((lst val)
            (parse-http-or lst val))
         ((lst)
            (parse-http-or lst #f))))

   (lets ((res tail 
(parse-http "GET /foo?bar=bax+quux HTTP/1.1
Host: localhost
User-Agent: foo/1.0
Accept: text/html,text/plain,application/xml;q=0.9,*/*;q=0.8
Accept-Language: fi-FI,fi;q=0.8,en-US;q=0.5,en;q=0.3
Accept-Encoding: gzip, deflate
Connection: keep-alive
Upgrade-Insecure-Requests: 1

foo")))
      (write res)
      (print "")
      (print ", tail " tail)
)))


