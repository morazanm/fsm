#lang racket
(require (for-syntax syntax/parse)
         json
         racket/tcp
         "./fsm-functions.rkt"
         "../../fsm-core/interface.rkt")

(define DEBUG_MODE #t)
(define ADDRESS "127.0.0.1")
(define PORT 4000)
(define listener (tcp-listen PORT 4 #t ADDRESS))

; displayln! is a simple macro that prints debug logs when the DEBUG_MODE variable is 
; #t. The first arg is the string to be printed. Any additional args are used to format the string
(define-syntax (displayln! stx)
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


;; handle-request :: jsexpr(a) -> jsexpr(b)
;; Based off of the instruction that is provideded in the incomming jsexpr, dispatches 
;; to the approperate mapping function
(define (handle-request input)
  (match (string->symbol (hash-ref input 'instr))
    ['build_machine (build-machine (hash-ref input 'data))]
    ['shut_down eof] ;; we use eof to denote a shutdown
    [_ (error (format "Invalid instruction given: ~s" (hash-ref input 'instr)))]))


;; listen-for-input :: optional(jsexpr)
;; listens on the specified socket for incomming connections
;; if data is supplied then it is sent when the connection is first established
(define (listen-for-input [data '()])
  (displayln! "FSM Gui server listenting on ~s on port ~s" PORT ADDRESS)
  (define-values (in out) (tcp-accept listener))
  (when data
    (displayln! "Sending prebuilt machine")
    (write-json-and-flush data out))
  (thread
   (lambda ()
     (let loop ()
       (define hashed-msg (read-json in))
       (displayln! "Recieved a incomming request")
       (unless (eof-object? hashed-msg)
         (define outgoing-data (handle-request hashed-msg))
         (displayln! outgoing-data)
         (if (eof-object? outgoing-data)
             (tcp-close listener)
             (begin
               (write-json outgoing-data out)
               (flush-output out)
               (loop)))))))
  (listen-for-input))


(define (run-with-prebuilt fsa invariants)
  (define data-to-send (hash 'data (fsa->jsexpr fsa invariants)
                             'error (json-null)
                             'responseType "prebuilt_machine"))
  (listen-for-input data-to-send))


(define a*a (make-dfa '(S A F D)     
                      '(a b)     
                      'S        
                      '(F)       
                      '((S a F)
                        (F a F)
                        (S b A)
                        (A a F)
                        (A b A)
                        ;; rules to dead state D
                        (F b D)
                        (D a D)
                        (D b D))'nodead))

(define invariants (list (cons 'S "(lambda (consumed-input) #t)")
                         (cons 'F "(lambda (consumed-input) #f)")
                         (cons 'D "(lambda (consumed-input) #t)")))
  
(run-with-prebuilt a*a invariants)