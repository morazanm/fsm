#lang racket
(require (for-syntax syntax/parse)
         json
         racket/tcp
         "./fsm-functions.rkt")

(provide run-with-prebuilt)

(define DEBUG_MODE #f)
(define ADDRESS "127.0.0.1")
(define PORT 4000)

;; Holds data for a active tcp-thread. This data is help for cleanup
;; thrd -> thread
;; in   -> tcp port
;; out  -> tcp port
(struct listener-thread (thrd in out))

;; clean-up-listener-thread :: listener-thread -> ()
;; Closes the in and out ports as well as kills the thread
(define (clean-up-listener-thread t)
  (tcp-abandon-port (listener-thread-in t))
  (tcp-abandon-port (listener-thread-out t))
  (kill-thread (listener-thread-thrd t)))

; displayln! is a simple macro that prints debug logs when the DEBUG_MODE variable is 
; #t. The first arg is the string to be printed. Any additional args are used to format the string
(define-syntax (displayln! stx)
  (syntax-parse stx
    [(_ s:expr) #`(when DEBUG_MODE (displayln s))]
    [(_ s:expr args:expr ...)
     #`(when DEBUG_MODE (displayln (format s args ...)))]))


(define (send-fsm-protocal data out)
  (displayln! "\n****Sending Data:****\n ~s" data)
  (write-json data out)
  (write eof out)
  (flush-output out))


;; listen-for-input :: ()
;; listens on the specified socket for incomming connections
;; if data is supplied then it is sent when the connection is first established
(define (run-server [pre-computed-data (hash)])
  (displayln (format "FSM Gui server listenting at ~s on port ~s" ADDRESS PORT))
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (define active-threads (make-hash))
  ;; DrRacket throws a break exception when the stop button is pressed.
  ;; We need to make sure to cleanup the TCP-server when this happens so
  ;; the GUI knows the socket was closed.
  (with-handlers ([exn:break? (lambda (_)
                                (for ([t (hash-values active-threads)])
                                  (clean-up-listener-thread t))
                                (tcp-close listener))])
    ;; listen-for-input :: ()
    ;; listens for incomming connections. If a connection is recieved then a thread is spun up
    ;; to handle the request
    (define (listen-for-input)
      (define-values (init-data ns)  (values (hash-ref pre-computed-data 'data '())
                                        (hash-ref pre-computed-data 'namespace (current-namespace))))
      ;; handle-request :: jsexpr(a) -> jsexpr(b)
      ;; Based off of the instruction that is provideded in the incomming jsexpr, dispatches
      ;; to the approperate mapping function
      (define (handle-request input)
        (match (string->symbol (hash-ref input 'instr))
          ['redraw (regenerate-graph (hash-ref input 'data))]
          ['build_machine (build-machine (hash-ref input 'data) ns)]
          ['recompute_inv (recompute-invariant (hash-ref input 'data) ns)]
          ['shut_down eof] ;; we use eof to denote a shutdown
          [_ (error 'handle-request "Invalid instruction given: ~a" (hash-ref input 'instr))]))
      ;; suspend untill a tcp connection is recieved
      (define-values (in out) (tcp-accept listener))
      ;; If we have initial data to send when first connecting with a client send it now
      (unless (null? init-data)
        (displayln! "Sending prebuilt machine")
        (send-fsm-protocal init-data out))
      (define thread-id (gensym))
      ;; Spin up a worker thread to handle the connection with the client and
      ;; repeat above steps.
      (define worker (thread
                      (lambda ()
                        (let loop ()
                          (define hashed-msg (read-json in))
                          (displayln! "\n----Recieved a incomming request: ~s ----"
                                      (if (eof-object? hashed-msg) "eof" (hash-ref hashed-msg 'instr)))
                          (displayln! hashed-msg)
                          (unless (eof-object? hashed-msg)
                            (define outgoing-data (handle-request hashed-msg))
                            (if (eof-object? outgoing-data)
                                (let ((t (hash-ref active-threads thread-id)))
                                  (begin
                                    (hash-remove! active-threads thread-id)
                                    (clean-up-listener-thread t)))
                                (begin
                                  (send-fsm-protocal outgoing-data out)
                                  (loop))))))))

      (hash-set! active-threads thread-id (listener-thread worker in out))
      (listen-for-input))
    (listen-for-input)))


;; run-with-prebuilt :: fsa listof(cons symbol string) namespace -> ()
;; Runs the TCP server and sends the prebuild machine to the GUI. A namespace must be
;; provided so we know the context that the function should be ran in.
(define (run-with-prebuilt fsa invariants ns)
  (define data-to-send (hash 'data (hash-set (fsa->jsexpr fsa invariants)
                                             'hotReload #t)
                             'error (json-null)
                             'responseType "prebuilt_machine"))
  (run-server (hash 'data data-to-send 'namespace ns)))

;; run-without-prebuilt :: ()
;; Runs the TCP server without sending a prebuilt machine to
;; the GUI
(define (run-without-prebuilt) (run-server))
