#| doc
Thread Scheduler

This library defines the thread controller. It handles activation, suspension
and requested external actions of continuation-based threads. It is much like a
very small kernel.

A thread is simply a function. Owl programs have two continuations, one for the
regular one and one for the thread scheduler under which the function is
running. Every function eventually calls its own final continuation, which
results in the thread being removed from the thread scheduler. It can also call
the second continuation and request operations from this thread scheduler.

The task of the thread scheduler is to run each running thread for a while,
suspend and activate them, and handle message passing.

Threads have a primitive only for asynchronous message passing, but they can
voluntarily wait for a specific kind of response, resulting in synchronous
operation where desired.

|#

(define-library (owl thread)

   (export
      start-thread-controller
      thread-controller
      signal-handler/repl
      signal-handler/halt
      signal-handler/ignore
      report-failure
      try-thunk try)

   (import
      (owl core)
      (owl queue)
      (owl syscall)
      (owl ff)
      (owl function)
      (owl list)
      (owl math)
      (owl tuple)
      (owl string)
      (owl render)
      (owl eval env)
      (owl io)
      (owl port))

   (begin

      (define (bad-syscall id a b c todo done state)
         (system-println "mcp: got bad syscall")
         (values todo done state))

      ; -> state (#false | waked-thread)
      (define (deliver-mail state to envelope)
         (let ((st (get state to #false)))
            (cond
               ((pair? st) ;; currently working, leave a mail to inbox queue
                  (values
                     (fupd state to (qsnoc envelope st))
                     #false))
               ((not st) ;; no current state, message to a new inbox
                  (values
                     (put state to (qcons envelope qnull))
                     #false))
               (else ;; thread was waiting activate it
                  (values
                     (fupd state to qnull) ;; leave an inbox
                     (tuple to (λ () (st envelope)))))))) ;; activate it

      (define (deliver-messages todo done state subs msg tc)
         (if (null? subs)
            (tc tc todo done state)
            (lets ((state waked (deliver-mail state (car subs) msg)))
               (if waked
                  (deliver-messages (cons waked todo) done state (cdr subs) msg tc)
                  (deliver-messages todo done state (cdr subs) msg tc)))))

      (define (eval-break-message from)
          (tuple from (tuple 'breaked)))

      (define (subscribers-of state id)
         (get (get state link-tag empty) id #n))

      ; remove the thread and report to any interested parties about the event
      (define (drop-delivering todo done state id msg tc)
         (let ((subs (subscribers-of state id)))
            (if (null? subs)
               (begin
                  ;; no threads were waiting for something that is being removed, so tell stderr about it
                  ;(print*-to stderr "VM: thread " id " exited")
                  (tc tc todo done (del state id)))
               (deliver-messages todo done
                  (del (fupd state link-tag (del (get state link-tag empty) id)) id)
                  subs msg tc))))

      ;; thread dropping, O(n)
      (define (drop-from-list lst tid) ; -> lst'
         (cond
            ((null? lst) lst)
            ((eq? (ref (car lst) 1) tid) (cdr lst))
            (else
               (cons (car lst)
                  (drop-from-list (cdr lst) tid)))))

      ; drop a possibly running thread and notify linked ones with msg
      (define (drop-thread id todo done state msg tc) ; -> todo' x done' x state'
         (drop-delivering
            (drop-from-list todo id)
            (drop-from-list done id)
            state id msg tc))

      ; l id → #false|thread l', O(n) running threads
      (define (catch-thread l id)
         (if (null? l)
            (values #false l)
            (let ((this (car l)))
               (if (eq? id (ref this 1))
                  (values this (cdr l))
                  (lets ((caught l (catch-thread (cdr l) id)))
                     (values caught (cons this l)))))))

      (define return-value-tag "rval") ; a unique key if thread state ff

      ; mcp syscalls grab the functions from here to transform the state

      (define (signal-handler/halt signals threads state controller)
         (system-println "[signal-halt]")
         (halt 42)) ;; exit owl with a specific return value

      (define mcp-syscalls
         (tuple

            ; 1, runnig and time slice exhausted (the usual suspect, handled directly in scheduler)
            (λ (id a b c todo done state tc)
               ; (system-println "syscall 1 - switch thread")
               (tc tc todo (cons (tuple id a) done) state))

            ; 2, thread finished, drop
            (λ (id a b c todo done state tc)
               ; (system-println "mcp: syscall 2 -- thread finished")
               (drop-delivering todo done state id
                  (tuple id (tuple 'finished a b c)) tc))

            ; 3, vm thrown error
            (λ (id a b c todo done state tc)
               ;(system-println "mcp: syscall 3 -- vm error")
               ;; set crashed exit value proposal
               (let ((state (put state return-value-tag 126)))
                  (drop-delivering todo done state id
                     (tuple id (tuple 'crashed a b c)) tc)))

            ; 4, fork
            (λ (id cont new-id thunk todo done state tc)
               (tc tc
                  (ilist
                     (tuple new-id thunk)
                     (tuple id (λ () (cont new-id)))
                     todo)
                  done state))

            ; 5, user thrown error
            (λ (id a b c todo done state tc)
               ; (system-println "mcp: syscall 5 -- user poof")
               (drop-delivering todo done state id
                  (tuple id (tuple 'error a b c)) tc))

            ;; return mails to my own inbox (in reverse order, newest on top)
            ; 6, (return-mails rl)
            (λ (id cont rmails foo todo done state tc)
               (let ((queue (get state id qnull)))
                  (tc tc (cons (tuple id (λ () (cont 'done))) todo)
                     done (put state id (foldr qsnoc queue rmails)))))

            ; 7, am i the only thread?
            (λ (id cont b c todo done state tc)
               (tc tc
                  (cons (tuple id (λ () (cont (and (null? todo) (null? done))))) todo)
                  done state))

            ; 8, get running thread ids (sans self)
            (λ (id cont b c todo done state tc)
               (let
                  ((ids
                     (append
                        (map (C ref 1) todo)
                        (map (C ref 1) done))))
                  (tc tc
                     (cons
                        (tuple id (λ () (cont ids)))
                        todo)
                     done state)))

            ; 9, send mail
            (λ (id cont to msg todo done state tc)
               (let ((todo (cons (tuple id (λ () (cont 'delivered))) todo)))
                  (lets ((state waked (deliver-mail state to (tuple id msg))))
                     (if waked
                        (tc tc (ilist (car todo) waked (cdr todo)) done state)
                        (tc tc todo done state)))))

            ; 10, breaked - call signal handler
            (λ (id a b c todo done state thread-controller)
               (system-println "syscall 10 - caught signals")
               (let ((all-threads (cons (tuple id a) (append todo done))))
                  ;; tailcall signal handler and pass controller to allow resuming operation
                  ((get state signal-tag signal-handler/halt) ; default to standard mcp
                     b
                     all-threads state thread-controller)))

            ; 11, reset mcp state (usually means exit from mcp repl)
            (λ (id cont threads state xtodo xdone xstate tc)
               ; (system-println "syscall 11 - swapping mcp state")
               (tc tc threads #n state))

            ; 12, set break action
            (λ (id cont choice x todo done state tc)
               (tc tc
                  (cons (tuple id (λ () (cont #true))) todo)
                  done (put state signal-tag choice)))

            ; 13, look for mail in my inbox at state
            (λ (id cont foo nonblock? todo done state tc)
               (lets ((valp queue (quncons (get state id qnull) #false)))
                  (cond
                     (valp      ;; envelope popped from inbox
                        (tc tc (cons (tuple id (λ () (cont valp))) todo) done
                           (fupd state id queue)))
                     (nonblock? ;; just tell there is no mail with #false
                        (tc tc (cons (tuple id (λ () (cont #false))) todo) done state))
                     (else      ;; leave thread continuation waiting
                        (tc tc todo done (put state id cont))))))

            ;; todo: switch memory limit to a hard one in ovm.c
            ; 14, memory limit was exceeded
            (λ (id a b c todo done state tc)
               (system-println "syscall 14 - memlimit exceeded, dropping a thread")
               ; for now, kill the currently active thread (a bit dangerous)
               (drop-delivering todo done state id
                  (tuple id (tuple 'crashed 'memory-limit b c)) tc))

            ; 15, drop local thread
            (λ (id cont target c todo done state tc)
               (drop-thread target
                  (cons (tuple id (λ () (cont (tuple 'killing target)))) todo)
                  done state (tuple target (tuple 'killed-by id)) tc))

            ; 16, wrap the whole world to a thunk
            (λ (id cont path c todo done state tc)
               (let
                  ((resume
                     (λ (args)
                        (tc tc (cons (tuple id (λ () (cont 'resumed))) todo)
                           done state))))
                  (tc tc (cons (tuple id (λ () (cont resume))) todo) done state)))

            ; 17, catch or release a running thread (not touching mailbox etc)
            (λ (id cont catch? info todo done state tc)
               (if catch?
                  (lets
                     ((all (append todo done))
                      (val all (catch-thread all info)))
                     (tc
                        (cons (tuple id (λ () (cont val))) all)
                        #n state))
                  (tc
                     (ilist (tuple id (λ () (cont 'released))) info todo)
                     done state)))

            ; 18, get a list of currently running thread ids
            (λ (id cont b c todo done state tc)
               (lets
                  ((grab (λ (l n) (cons (ref n 1) l)))
                   (ids (fold grab (fold grab #n todo) done)))
                  (tc tc (cons (tuple id (λ () (cont (cons id ids)))) todo) done state)))

            ; 19, set return value proposal
            (λ (id cont b c todo done state tc)
               (tc tc (cons (tuple id (λ () (cont b))) todo) done (put state return-value-tag b)))

            ;;; 20 & 21 change during profiling

            ; 20, start profiling, no-op during profiling returning 'already-profiling
            (λ (id cont b c todo done state tc)
               (tc tc (cons (tuple id (λ () (cont 'already-profiling))) todo) done state))

            ; 21, end profiling, resume old ones, pass profiling info
            (λ (id cont b c todo done state tc)
               (lets
                  ((prof (get state 'prof #false)) ;; ff storing profiling info
                   (tc (get prof 'tc #false))      ;; normal thread scheduler
                   (prof (del prof 'tc)))         ;; give just the collected data to thread
                  (tc tc (cons (tuple id (λ () (cont prof))) todo) done
                     (del state 'prof))))

            ; 22, nestable parallel computation
            (λ (id cont opts c todo done state tc)
               (lets
                  ((por-state (tuple cont opts c)))
                  (tc tc (cons (tuple id por-state) todo) done state)))

            ; 23, link thread
            (λ (id cont target c todo done state tc)
               (lets
                  ((links (get state link-tag empty))
                   (followers (get links target #n))
                   (links
                     (if (memq id followers)
                        links
                        (put links target (cons id followers)))))
                  (tc tc
                     (cons (tuple id (λ () (cont target))) todo)
                     done
                     (put state link-tag links))))

            ;; 24, exit process saving state
            (λ (id cont return-value c todo done state tc)
               (values

                  ;; return from thread scheduler
                  return-value

                  ;; only to be brought back to life later
                  (λ (new-input)
                     (tc tc
                        (cons (tuple id (λ () (cont new-input))) todo)
                        done state))))))

      ;; todo: add deadlock detection here (and other bad terminal waits)
      (define (halt-thread-controller state)
         (get state return-value-tag 0))

      (define (bytecode-of thing default)
         (cond
            ((bytecode? thing) thing)
            ((function? thing) (bytecode-of (ref thing 1) default))
            (else default)))

      ;; store profiling info about this call
      ;; the exec is either a thunk to be run in a thread as a result of
      ;; forking or a syscall being answered, or a vm-generated tuple which
      ;; has arguments for the next function call and the function at the
      ;; *last* slot of the tuple.

      (define (update-state state exec)
         (if (tuple? exec) ;; vm thread suspensions are tuples
            (lets
               ((bcode (bytecode-of (ref exec (tuple-length exec)) 'not-a-function)) ;; identify place based in bytecode which is inert
                (prof (get state 'prof #false))
                (count (get prof bcode 0))
                (prof (put prof bcode (+ count 1)))
                (state (fupd state 'prof prof)))
               state)
            ;; don't record anything for now for the rare thread starts and resumes with syscall results
            state))

      (define (thread-controller self todo done state)
         (if (null? todo)
            (if (null? done)
               (halt-thread-controller state)  ;; nothing left to run
               (self self done #n state))    ;; new scheduler round
            (lets
               ((this todo todo)
                (id st this))
               (lets ((op a b c (run st thread-quantum)))
                  (if (eq? op 1)
                     (self self todo (cons (tuple id a) done) state)
                     ((ref mcp-syscalls op) id a b c todo done state self))))))

      (define (start-thread-controller threads state-alist)
         (thread-controller thread-controller threads
            #n (list->ff state-alist)))

      (define (try-thunk thunk fail-fn id)
         (link id)
         (thunk->thread id thunk)
         (let ((outcome (ref (accept-mail (λ (env) (eq? (ref env 1) id))) 2)))
            (if (eq? (ref outcome 1) 'finished)
               (ref outcome 2)
               (fail-fn outcome))))

      ;; move elsewhere
      (define (report-failure env retval)
         (λ (outcome)
            (tuple-case outcome
               ((crashed opcode a b)
                  (print-to stderr (verbose-vm-error env opcode a b))
                  retval)
               ((error cont reason info)
                  ;; note: these could be restored via cont
                  (print-to stderr
                     (list->string
                        (foldr render '(10) (list "error: " reason info))))
                  retval)
               ((breaked)
                  (print-to stderr ";; stopped by signal")
                  retval)
               (else
                  (print-to stderr (list "bug: " outcome))
                  retval))))

      (define (try thunk retval . envp)
         (try-thunk thunk 
            (report-failure (if (null? envp) empty (car envp)) retval) 
            (list 'try)))

      ;; find a thread id that looks like one started via repl
      (define (repl-eval-thread threads)
         (fold
            (lambda (found x)
               (or found
                  (if (and (pair? (ref x 1))
                          (eq? (car (ref x 1)) 'repl-eval))
                     (ref x 1)
                     found)))
            #f threads))

      ;; signal handler which kills the 'repl-eval thread if there, or repl
      ;; if not, meaning we are just at toplevel minding our own business.
      (define (signal-handler/repl signals threads state controller)
         (let ((eval-thread (repl-eval-thread threads))) ;; warning: can be several
            (if eval-thread
               ;; there is a thread evaling user input, linkely gone awry somehow
               (drop-thread eval-thread threads #n state (eval-break-message eval-thread) controller)
               ;; nothing evaling atm, exit owl
               (begin
                  (system-println "[no eval thread - stopping on signal]")
                  (signal-handler/halt signals threads state controller)))))

      (define (signal-handler/ignore signals threads state controller)
         (system-println "[ignoring signals]")
         (controller controller threads null state))
))
