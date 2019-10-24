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
      (owl eval env))

   (begin

      ;;;
      ;;; HTTP parsing
      ;;;

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
               ((val get-byte))
               val)))

      (define get-nonspace
         (get-byte-if (λ (x) (not (eq? x #\space)))))

      (define get-crlf
         (get-parses
            ((skip (get-either (get-imm #\return) (get-epsilon #f)))
             (skip (get-imm #\newline)))
            #t))

      (define get-query-path
         (get-parses
            ((chars (get-greedy-plus (get-byte-if (λ (x) (not (memq x '(#\space #\newline #\? #\#))))))))
            (list->string chars)))

      (define get-key-value
         (get-parses
            ((key (get-greedy-plus (get-byte-if (λ (x) (not (memq x '(#\space #\newline #\& #\# #\=)))))))
             (value
                (get-either
                   (get-parses
                      ((skip (get-imm #\=))
                       (val (get-greedy-plus (get-byte-if (λ (x) (not (memq x '(#\space #\newline #\& #\#))))))))
                      (list->string val))
                   (get-epsilon #f))))
            (cons (list->string key)
                  value)))

      (define maybe-get-query-params
         (get-either
            (get-parses
               ((skip (get-imm #\?))
                (first get-key-value)
                (rest (get-greedy-star
                         (get-parses
                            ((skip (get-imm #\&))
                             (val get-key-value))
                            val))))
               (cons first rest))
            (get-epsilon #f)))

      (define maybe-get-query-fragment
         (get-either
            (get-parses
               ((skip (get-imm #\#))
                (vals (get-greedy-plus get-nonspace))) ;; temp
               (list->string vals))
            (get-epsilon #f)))

      (define (maybe-put ff key val)
         (if val
            (put ff key val)
            ff))

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
             (path get-query-path)
             (query-params maybe-get-query-params)
             (query-fragment maybe-get-query-fragment)
             (skip (get-word " HTTP/1." 'foo))
             (ver-char (get-either (get-imm #\0) (get-imm #\1)))
             (skip get-crlf))
            (-> empty
               (put 'method method)
               (put 'path path)
               (maybe-put 'query-params query-params)
               (maybe-put 'query-fragment query-fragment)
               )))

      (define (known-header->symbol str)
         (cond
            ((string-ci=? str "Host") 'host)
            ((string-ci=? str "User-Agent") 'user-agent)
            ((string-ci=? str "Referer") 'referer) ;; originally Referrer
            ((string-ci=? str "Origin") 'origin) ;; Origin > Referer, https://wiki.mozilla.org/Security/Origin
            ((string-ci=? str "Cookie") 'cookie)
            ((string-ci=? str "Content-type") 'content-type)
            ((string-ci=? str "Content-length") 'content-length)
            ((string-ci=? str "Accept-Language") 'accept-language)
            ((string-ci=? str "Accept") 'accept) ;; text/html, text/plain, ...
            ((string-ci=? str "Accept-Encoding") 'accept-encoding)
            ((string-ci=? str "Connection") 'connection)
            (else #false)))

      (define get-header-line
         (get-parses
            ((key (get-greedy-plus (get-byte-if (λ (x) (not (eq? x #\:))))))
             (skip (get-imm #\:))
             (skip (get-maybe (get-imm #\space) #f))
             (val (get-greedy-plus (get-byte-if (λ (x) (not (eq? x #\newline))))))
             (skip get-crlf))
            (lets
               ((key (list->string key))
                (key (or (known-header->symbol key) key)))
               (cons key (list->string val)))))

      (define get-http
         (get-parses
            ((req get-first-line)
             (headers (get-greedy-plus get-header-line))
             (skip get-crlf))
            (fold
               (λ (req pair)
                  (if (symbol? (car pair))
                     (put req (car pair) (cdr pair))
                     (put req 'headers
                        (cons pair (get req 'headers #null)))))
               req headers)))

      (define (hex-val a)
         (cond
            ((not a) #false)
            ((< 47 a 58) (- a 48))
            ((< 96 a 103) (- a 87))
            ((< 64 a 71) (- a 55))
            (else #false)))




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
(parse-http "GET /foo?bar=foo+bar&baz=42#lol HTTP/1.1
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


