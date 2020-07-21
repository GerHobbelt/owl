(define-library (owl eval data)

   (import
      (owl core)
      (owl sum))
   
   (export
      success
      ok         ;; exp env -> success
      fail       ;; reason  -> success
      )
   
   (begin
   
      (define-sum-type success
         (ok exp env)
         (fail why))

      ))            
            
