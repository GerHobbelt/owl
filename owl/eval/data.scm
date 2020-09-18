#| doc
Compiler Data Structures

This library contains shared data structures used in the compiler.
Originally all the data structures were either S-expressions or tuples with a
fixed structure. They are being converted to sum types, which makes it possible
to verify some aspects of use of the data structure at compile time.
|#


(define-library (owl eval data)

   (import
      (owl core)
      (owl lcd))

   (export
      success
      ok         ;; exp env -> success
      fail       ;; reason  -> success

      rtl-case
      ret move prim cons-close ld refi goto jeqi jeq
      )

   (begin

      (define-sum-type success
         (ok exp env)
         (fail why))

      (define-sum-type rtl-case
         (ret n)
         (move a b more)
         (prim op args to more)
         (cons-close closure? lpos offset env to more)
         (ld val to cont)
         (refi from offset to more)
         (goto op nargs)
         (jeqi i a then else)
         (jeq a b then else))

))

