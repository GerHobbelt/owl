(define-library (owl port)
   (export
      port?
      fd->port
      port->fd
      stdin
      stdout
      stderr
      stdio-port?)

   (import
      (owl core)
      (only (owl list) memq))

   (begin

      (define (port? x) (eq? (type x) type-port))
      (define fd->port (H fxxor (create-type type-port)))
      (define port->fd (H fxxor 0))

      (define stdin (fd->port 0))
      (define stdout (fd->port 1))
      (define stderr (fd->port 2))

      (define stdio-port? (C memq (list stdin stdout stderr)))
))
