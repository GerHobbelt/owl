#| doc
Default Toplevel

Values exported in this library are available when starting owl interactively. The form
(exports ...) allows exporting all values exported by another library.
|#

(define-library (owl toplevel)

   (export
      (exports (owl core))
      (exports (owl list))
      (exports (owl boolean))
      (exports (owl rlist))
      (exports (owl list-extra))
      (exports (owl ff))
      (exports (owl io))
      (exports (owl lazy))
      (exports (owl string))
      (exports (owl symbol))
      (exports (owl sort))
      (exports (owl bytevector))
      (exports (owl vector))
      (exports (owl equal))
      (exports (owl random))
      (exports (owl render))
      (exports (owl syscall))
      (exports (owl function))
      (exports (owl fasl))
      (exports (owl port))
      (exports (owl time))
      (exports (owl regex))
      (exports (owl math extra))
      (exports (owl math))
      (exports (owl tuple))
      (exports (owl digest))
      (exports (owl syntax-rules))


      halt
      lets/cc
      read
      read-ll
      ref
      suspend
      wait
      (exports (scheme base))
      (exports (scheme cxr))
      (exports (scheme write)))

   (import
      (owl core)
      (owl boolean)
      (owl list)
      (owl alist)
      (owl rlist)
      (owl list-extra)
      (owl tuple)
      (owl ff)
      (owl io)
      (owl port)
      (owl time)
      (owl lazy)
      (owl math extra)
      (owl string)
      (owl symbol)
      (owl sort)
      (owl fasl)
      (owl function)
      (owl bytevector)
      (owl vector)
      (owl equal)
      (owl random)
      (owl regex)
      (owl render)
      (owl syscall)
      (owl math)
      (owl digest)
      (owl syntax-rules)
      (only (owl compile) suspend)
      (only (owl sexp) read read-ll)
      (scheme base)
      (scheme cxr)
      (scheme write)

      ;; just pull into the fasl
      (owl date)
      (scheme case-lambda)
      (scheme complex)
      (scheme process-context)
      (scheme time)))
