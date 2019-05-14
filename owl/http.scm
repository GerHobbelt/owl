(define-library (owl http)
   
   (import
      (owl base)
      (owl io)
      (owl sys))
   
   (export 
      http-get)
   
   (begin
     
      (define (cut-at lst val)
         (cond 
            ((null? lst)
               (values lst lst))
            ((eq? (car lst) val)
               (values #n lst))
            (else
               (lets ((head tail (cut-at (cdr lst) val)))
                  (values
                     (cons (car lst) head) tail)))))
 
      ;; a crude test
      (define (http-get url)
         (if (m/^http:\/\// url)
            (lets
               ((url (s/http:\/\/// url))
                (hostl reql (cut-at (string->list url) #\/))
                (host (resolve-host (list->string hostl)))
                (conn (if host (open-connection host 80) #false)))
               (if conn
                  (begin
                     (print-to conn 
                        (str "GET " (list->string reql) " HTTP/1.1\n"
                             "Host: " (list->string hostl) "\n\n"))
                     (list->string
                        (force-ll
                           (port->byte-stream conn))))
                  #false))
            #false))
      
))
      
