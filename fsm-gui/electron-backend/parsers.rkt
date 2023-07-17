#lang racket
(require json "../../fsm-core/private/constants.rkt")
(provide parse-finals
         parse-start
         parse-states
         parse-alpha
         parse-type
         parse-input
         parse-accept
         parse-rules
         parse-invariants
         parse-stack-alpha)


(define (json-null? v) (equal? v (json-null)))

;; parse-states :: jsexpr(states) -> listof(symbol)
;; returns the states as a list of symbols
(define (parse-states states-jsexpr)
  (map (lambda (s) (string->symbol (hash-ref s 'name))) states-jsexpr))


;; parse-finals :: jsexpr(states) -> listof(symbol)
;; returns the final states as a list of symbols
(define (parse-finals states)
  (map (lambda (s) (string->symbol (hash-ref s 'name)))
       (filter (lambda (s) (or (equal? "startfinal" (hash-ref s 'type))
                               (equal? "accept" (hash-ref s 'type))
                               (equal? "final" (hash-ref s 'type))))
               states)))


;; parse-start :: jsexpr(states) -> symbol | false
;; returns the symbol of the start state if it exists otherwise returns #f
(define/match (parse-start _states)
  [('()) #f]
  [(`(,(hash-table ('name n) ('type t)) ,xs ...))
   (if (or (equal? "start" t) (equal? "startfinal" t))
       (string->symbol n)
       (parse-start xs))])


;; parse-accept :: jsexpr(states) -> symbol | false
;; returns the symbol of the accept state if it exists otherwise returns #f
(define/match (parse-accept _states)
  [('()) #f]
  [(`(,(hash-table ('name n) ('type t)) ,xs ...))
   (if (equal? "accept" t) (string->symbol n) (parse-accept xs))])

;; parse-alpha :: jsexpr -> listof(symbol)
;; returns the alpha as a list of symbols
(define (parse-alpha data)
  (map string->symbol (hash-ref data 'alphabet)))

;; parse-input :: jsexpr -> listof(symbol)
;; returns the input as a los
(define (parse-input jsexpr)
  (map string->symbol (hash-ref jsexpr 'input)))

;; parse-type :: jsexpr -> symbol
;; returns the machine type as a symbol
(define (parse-type data)
  (string->symbol (hash-ref data 'type)))

;; parse-invariants :: jsexpr(state) -> listof(pairof(symbol string))
;; retunrs the invarits as pair contained the state name and a string containing
;; the racket code for the invariant.
(define (parse-invariants jsexpr)
  (map (lambda (s) (cons (string->symbol (hash-ref s 'name))
                         (hash-ref s 'invFunc)))
       (filter (lambda (s) (not (json-null? (hash-ref s 'invFunc))))
               jsexpr)))

;; parse-stack-alpha :: jsexpr symbol -> listof(symbol) | false
;; returns the stack alpha as a list of symbols or false if not a pda
(define (parse-stack-alpha jsexpr type)
  (if (equal? type 'pda)
      (map string->symbol (hash-ref jsexpr 'stackAlpha))
      #f))

;; parse-rules :: jsexpr symbol -> listof(fsm-core-rules)
;; Converts the jsexpr into the fsm-core representation of rules
(define (parse-rules rules type)
  (map (lambda (r)
         (match type
           [(or 'dfa 'ndfa) (list (string->symbol (hash-ref r 'start))
                                  (string->symbol (hash-ref r 'input))
                                  (string->symbol (hash-ref r 'end)))]
           ['pda
            (define popped (hash-ref r 'popped))
            (define pushed (hash-ref r 'pushed))
            (list (list (string->symbol (hash-ref r 'start))
                        (string->symbol (hash-ref r 'input))
                        (if (empty? popped) EMP (map string->symbol popped)))
                  (list (string->symbol (hash-ref r 'end))
                        (if (empty? pushed) EMP (map string->symbol pushed))))]
           [(or 'tm 'tm-language-recognizer)
            (define start-tape (hash-ref r 'startTape))
            (define end-tape (hash-ref r 'endTape))
            ;; NOTE: We represent TM-actions (LM, _, @,) as a string, but a input as a single
            ;; value in a list for parsing reason on the GUI end. Ideally this should be cleaned
            ;; up at a later date.
            (list (list (string->symbol (hash-ref r 'start))
                        (string->symbol (if (list? start-tape) (list-ref start-tape 0) start-tape)))
                  (list (string->symbol (hash-ref r 'end))
                        (string->symbol (if (list? end-tape) (list-ref end-tape 0) end-tape))))]
           [_ (error 'parse-rules "Unsupported machine type ~a" type)]))
       rules))
