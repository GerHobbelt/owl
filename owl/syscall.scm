#| doc
Owl System Calls

Typical Scheme programs have a continuation underlying in the execution of a program.
Depending on the implementation strategy, this continuation may be an actual variable
and a function, as is done in Owl, or it is simulated on a more traditional stack-based
system.

Owl has in fact two continuations. One is the normal one you can capture with
call-with-current-continuation, but the other one is threaded even through that
operation. The other continuation is that of the thread scheduler. It is also a
regular program. The main difference in it is that it is expected to be called
initially when the program starts and the second continuation is blank. Calling
the blank continuation is what eventually terminates the whole program.

Normal progams can only access the second continuation via a primop. When this happens,
the continuation of the running program is captured as usual, and the second thread
scheduler continuation is called with the information provided in the call along with
the continuation allowing resuming execution of the program from the corresponding state.

Wrappers for calling the thread scheduler are defined in this library.

|#
(define-library (owl syscall)

   (export
      syscall error interact accept-mail wait-mail next-mail check-mail
      exit-owl release-thread catch-thread set-signal-action
      single-thread? kill link mail
      return-mails fork-named exit-thread exit-owl
      poll-mail-from start-profiling stop-profiling running-threads
      library-exit
      thread thunk->thread)

   (import
      (owl core))

   (begin

      (define (syscall op a b)
         (call/cc (λ (resume) (sys resume op a b))))

      (define (exit-thread value)
         (syscall 2 value value))

      ;; 3 = vm thrown error
      (define (fork-named name thunk)
         (syscall 4 name thunk))

      (define (error reason info)
         (syscall 5 reason info))

      (define (return-mails rmails)
         (syscall 6 rmails rmails))

      (define (running-threads)
         (syscall 8 #false #false))

      (define (mail id msg)
         (syscall 9 id msg))

      (define (kill id)
         (syscall 15 id #false))

      (define (single-thread?)
         (syscall 7 #true #true))

      (define (set-signal-action choice)
         (syscall 12 choice #false))

      (define (catch-thread id)
         (syscall 17 #true id))

      (define (release-thread thread)
         (syscall 17 #false thread))

      (define (exit-owl value)
         (syscall 19 value value) ;; set exit value proposal in thread scheduler
         (exit-thread value))     ;; stop self and leave the rest (io etc) running to completion

      (define (library-exit rval)
         (syscall 24 rval #f))

      (define (wait-mail)           (syscall 13 #false #false))
      (define (check-mail)          (syscall 13 #false #true))

      ;; returns values, so that it can be used directly with lets
      (define (next-mail)
         (let ((envelope (wait-mail)))
            (values (ref envelope 1) (ref envelope 2))))

      (define (accept-mail pred)
         (let loop ((this (wait-mail)) (rev-spam #n))
            (cond
               ((pred this)
                  (return-mails rev-spam) ; return the other mails to mailbox as such
                  this)
               (else
                  (loop (wait-mail) (cons this rev-spam))))))

      ;; wait mail from given thread for a while, giving other threads time (or sleeping) while waiting
      ;; todo: could interact with the sleeper thread to allow vm go to sleep between rounds

      (define (return-from-wait value spam)
         (if (null? spam)
            value
            (begin
               (return-mails spam)
               value)))

      (define (poll-mail-from id rounds default)
         (let loop ((envp (check-mail)) (spam #n) (rounds rounds))
            (cond
               ((not envp)
                  (if (eq? rounds 0)
                     (return-from-wait default spam)
                     ;; no mail, request a thread switch and recurse, at which point all other threads have moved
                     (begin
                        ;(set-ticker 0) ;; FIXME restore this when librarized
                        ;; no bignum math yet at this point
                        (lets ((rounds _ (fx- rounds 1)))
                           (loop (check-mail) spam rounds)))))
               ((eq? (ref envp 1) id)
                  ;; got it
                  (return-from-wait (ref envp 2) spam))
               (else
                  ;; got spam, keep waiting
                  (loop (check-mail) (cons envp spam) rounds)))))

      (define thunk->thread
         (case-lambda
            ((id thunk)
               (fork-named id thunk))
            ((thunk)
               ;; get a fresh name unless otherwise specified
               (fork-named (tuple 'anonimas) thunk))))

      (define-syntax thread
         (syntax-rules ()
            ((thread id val)
               (thunk->thread id (λ () val)))
            ((thread val)
               (thunk->thread (λ () val)))))

      ;; (thread (op . args)) → id
      ;; (wait-thread (thread (op . args)) [default]) → value
      ;; thread scheduler should keep the exit value

      ; Message passing (aka mailing) is asynchronous, and at least
      ; in a one-core environment order-preserving. interact is like
      ; mail, but it blocks the thread until the desired response
      ; arrives. Messages are of the form #(<sender-id> <message>).

      (define (interact whom message)
         (mail whom message)
         (ref (accept-mail (λ (env) (eq? (ref env 1) whom))) 2))

      (define (start-profiling)
         (syscall 20 #true #true))

      (define (stop-profiling)
         (syscall 21 #true #true))

      (define (link id)
         (syscall 23 id id))

))
