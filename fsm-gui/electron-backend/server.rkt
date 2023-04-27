#lang racket
(require
 json
 racket/tcp
 "./fsm-functions.rkt")

(define DEBUG_MODE #t)
(define ADDRESS "127.0.0.1")
(define PORT 4000)

(define (debug-displayln msg)
  (when DEBUG_MODE
    (displayln msg)))

(define listener (tcp-listen PORT 4 #f ADDRESS))


;; handle-request : jsexpr -> jsexpr
(define (handle-request input)
  (match (string->symbol(hash-ref input 'instr))
    ['build_machine (build-machine (hash-ref input 'data))]
    [_ (error (format "Invalid instruction given: ~s" (hash-ref input 'instr)))]))


(define (listen-for-input)
  (debug-displayln (format "FSM Gui server listenting on ~s on port ~s" ADDRESS PORT))
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (let loop ()
       (define hashed-msg (read-json in))
       (unless (eof-object? hashed-msg)
         (define outgoing-data (handle-request hashed-msg))
         (write-json outgoing-data out)
         ;;(loop)
         (tcp-close listener) ;;HACK: This should be handled on termination
         ))))
  (listen-for-input))

(listen-for-input)
