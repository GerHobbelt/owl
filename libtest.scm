
(import 
   (only 
      (owl syscall) 
      library-exit))

(λ (arg)
   (print "Hello, we have threading")
   (print "Arg is " arg)
   (library-exit 
      11
      (λ (a)
         (library-exit 
            22
            (λ (a)
               (library-exit 
                  33
                  (λ (x)
                     42)))))))
                  
            
