#lang racket
(require
  (for-syntax syntax/parse)
  json
  racket/tcp
  "./fsm-functions.rkt")

(define DEBUG_MODE #t)
(define ADDRESS "127.0.0.1")
(define PORT 4000)
(define listener (tcp-listen PORT 4 #t ADDRESS))

; debug-displayln is a simple macro that prints debug logs when the DEBUG_MODE variable is 
; #t. The first arg is the string to be printed. Any additional args are used to format the string
(define-syntax (debug-displayln stx)
  (syntax-parse stx
    [(_ s:expr) #`(when DEBUG_MODE (displayln s))]
    [(_ s:expr args:expr ...)
     #`(when DEBUG_MODE (displayln (format s args ...)))]))

;; write-json-and-flush :: jsexpr out-port -> out->port
;; Write the json expr to the out-port, then flushes the port and returns it
(define (write-json-and-flush jsexpr out)
  (begin
    (write-json jsexpr out)
    (flush-output out)
    out))


;; handle-request : jsexpr -> jsexpr
(define (handle-request input)
  (match (string->symbol (hash-ref input 'instr))
    ['build_machine (build-machine (hash-ref input 'data))]
    [_ (error (format "Invalid instruction given: ~s" (hash-ref input 'instr)))]))


(define (listen-for-input)
  (debug-displayln "FSM Gui server listenting on ~s on port ~s" PORT ADDRESS)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (let loop ()
       (define hashed-msg (read-json in))
       (unless (eof-object? hashed-msg)
         (define outgoing-data (handle-request hashed-msg))
         ;(write-json outgoing-data out)
         ;(flush-output out)
         ;;(loop)
         (tcp-close listener) ;;HACK: This should be handled on termination
         ))))
  #;(listen-for-input))

(listen-for-input)



