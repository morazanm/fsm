#lang racket
(require (for-syntax syntax/parse)
         json
         racket/tcp
         "./fsm-functions.rkt")

(provide run-with-prebuilt
         run-without-prebuilt
         run-with-prebuilt-hotload)

(define DEBUG_MODE #f)
(define ADDRESS "127.0.0.1")
(define PORT 4000)

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

;; listen-for-input :: TCP-listener optional(hash)
;; listens on the specified socket for incomming connections
;; if data is supplied then it is sent when the connection is first established
(define (listen-for-input listener [data-hash (hash 'pre-inv '())])
  (define-values (invariants data)  (values (hash-ref data-hash 'pre-inv '())
                                            (hash-ref data-hash 'data '())))
  ;; handle-request :: jsexpr(a) -> jsexpr(b)
  ;; Based off of the instruction that is provideded in the incomming jsexpr, dispatches
  ;; to the approperate mapping function
  (define (handle-request input)
    (match (string->symbol (hash-ref input 'instr))
      ['redraw (regenerate-graph (hash-ref input 'data))]
      ['build_machine (build-machine (hash-ref input 'data) invariants)]
      ['shut_down eof] ;; we use eof to denote a shutdown
      [_ (error 'handle-request "Invalid instruction given: ~a" (hash-ref input 'instr))]))
  (define-values (in out) (tcp-accept listener))
  (when (not (null? data))
    (displayln! "Sending prebuilt machine")
    (send-fsm-protocal data out))
  (thread
   (lambda ()
     (let loop ()
       (define hashed-msg (read-json in))
       (displayln! "\n----Recieved a incomming request: ~s ----"
                   (if (eof-object? hashed-msg) "eof" (hash-ref hashed-msg 'instr)))
       (displayln! hashed-msg)
       (unless (eof-object? hashed-msg)
         (define outgoing-data (handle-request hashed-msg))
         ;(displayln! outgoing-data)
         (if (eof-object? outgoing-data)
             (tcp-close listener)
             (begin
               (send-fsm-protocal outgoing-data out)
               (loop)))))))
  (listen-for-input listener data-hash))


;; run-with-prebuilt-hotload :: fsa listof(cons symbol string) -> ()
;; Runs the TCP server and sends the prebuild machine to the GUI.
(define (run-with-prebuilt-hotload fsa invariants)
  (displayln (format "FSM Gui server listenting at ~s on port ~s" ADDRESS PORT))
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (define data-to-send (hash 'data (hash-set (fsa->jsexpr fsa invariants)
                                             'hotReload #t)
                             'error (json-null)
                             'responseType "prebuilt_machine"))
  (listen-for-input listener (hash 'data data-to-send 'hotload #t)))


;; run-with-prebuilt :: fsa listof(symbol string inv-func) -> ()
;; Runs the TCP server and sends the prebuild machine to the GUI.
(define (run-with-prebuilt fsa invariants)
  (displayln (format "FSM Gui server listenting at ~s on port ~s" ADDRESS PORT))
  (define inv-strings (map (match-lambda [`(,n ,s ,_) (cons n s)]) invariants))
  (define inv-funcs (map (match-lambda [`(,n ,_ ,f) (cons n f)]) invariants))
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (define data-to-send (hash 'data (hash-set (fsa->jsexpr fsa inv-strings)
                                             'hotReload #f)
                             'error (json-null)
                             'responseType "prebuilt_machine"))
  (listen-for-input listener (hash 'data data-to-send 'pre-inv inv-funcs)))

;; run-without-prebuilt :: ()
;; Runs the TCP server without sending a prebuilt machine to
;; the GUI
(define (run-without-prebuilt)
  (displayln (format "FSM Gui server listenting at ~s on port ~s" ADDRESS PORT))
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (listen-for-input listener))
