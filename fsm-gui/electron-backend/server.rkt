#lang racket
(require (for-syntax syntax/parse)
         json
         racket/tcp
         "./fsm-functions.rkt"
         "../../fsm-core/interface.rkt")

(provide run-with-prebuilt run-without-prebuilt)

(define DEBUG_MODE #t)
(define ADDRESS "127.0.0.1")
(define PORT 4000)

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
    [_ (error 'handle-request "Invalid instruction given: ~a" (hash-ref input 'instr))]))


;; listen-for-input :: TCP-listener optional(jsexpr)
;; listens on the specified socket for incomming connections
;; if data is supplied then it is sent when the connection is first established
(define (listen-for-input listener [data '()])
  (define-values (in out) (tcp-accept listener))
  (when (not (null? data))
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
  (listen-for-input listener data))


;; run-with-prebuilt :: fsa listof(cons symbol string)
;; Runs the TCP server and sends the prebuild machine to the
;; GUI.
(define (run-with-prebuilt fsa invariants)
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (displayln! "FSM Gui server listenting on ~s on port ~s" PORT ADDRESS)
  (define data-to-send (hash 'data (fsa->jsexpr fsa invariants)
                             'error (json-null)
                             'responseType "prebuilt_machine"))
  (listen-for-input listener data-to-send))

;; run-without-prebuilt
;; Runs the TCP server without sending a prebuilt machine to
;; the GUI
(define (run-without-prebuilt)
  (define listener (tcp-listen PORT 4 #t ADDRESS))
  (displayln! "FSM Gui server listenting on ~s on port ~s" PORT ADDRESS)
  (listen-for-input listener))




(define pda=2ba (make-ndpda '(S M1 F)
                            '(a b)
                            '(a b)
                            'S
                            '(F)
                            `(((S ,EMP ,EMP) (M1 ,EMP))
                              ((M1 a ,EMP) (M1 (a a)))
                              ((M1 b ,EMP) (M1 (b)))
                              ((M1 a (b)) (M1 (a)))
                              ((M1 a (b b)) (M1 ,EMP))
                              ((M1 b (a)) (M1 ,EMP))
                              ((M1 ,EMP ,EMP) (F ,EMP)))))

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
                         (cons 'D (json-null))))

;(run-without-prebuilt)
(run-with-prebuilt a*a invariants)
