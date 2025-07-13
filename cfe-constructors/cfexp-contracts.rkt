#lang racket/base

(require "../fsm-core/private/macros/shared/shared-predicates.rkt"
         "../fsm-core/private/macros/shared/shared-flat-contracts.rkt"
         "../fsm-core/private/macros/error-formatting.rkt"
         "cfexp-structs.rkt"
         racket/contract)

(provide singleton-cfexp/c
         concat-cfexp/c
         union-cfexp/c
         var-cfexp/c
         kleene-cfexp/c
         update-binding!/c)


(define (cfexp? cfe)
  (or (mk-null-cfexp? cfe)
      (mk-empty-cfexp? cfe) 
      (mk-singleton-cfexp? cfe)
      (mk-var-cfexp? cfe) 
      (mk-concat-cfexp? cfe) 
      (mk-union-cfexp? cfe) 
      (mk-kleene-cfexp? cfe)))

(define (format-value-in-message blame value message)
  (format message value))

(define (format-message-only blame value message)
  (format-start-error blame value message))

(define (is-symbol?/c message)
  (make-flat-contract
          #:name 'is-symbol?
          #:first-order symbol?
          #:projection (λ (blame)
                         (λ (symbol)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            symbol
                            message)))))

(define is-struct/c
  (make-flat-contract
   #:name 'is-struct?
   #:first-order struct?
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     "Expected a structure, given")))))

(define (is-cfexp/c message)
  (and/c is-struct/c
         (make-flat-contract
          #:name 'is-cfe?
          #:first-order cfexp?
          #:projection (λ (blame)
                         (λ (x)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            x
                            message))))))

(define is-a-valid-symbol/c
  (make-flat-contract
          #:name 'is-symbol?
          #:first-order valid-state?
          #:projection (λ (blame)
                         (λ (symbol)
                           (current-blame-format format-message-only)
                           (raise-blame-error
                            blame
                            symbol
                            (format "The given symbol, ~a, is not a valid FSM symbol. A valid FSM symbol is either:\n an uppercase letter [A-Z] or an uppercase letter [A-Z], a dash, and number." symbol))))))

(define (is-a-list/c cfe-type)
    (make-flat-contract
     #:name 'is-a-list/c
     #:first-order (lambda (x) (list? x))
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       x
                       (format "~a expects an arbitrary amount of cfes, given" cfe-type))))))

(define (valid-listof/c predicate cfe-type field-name element-name)
  (make-flat-contract
   #:name (string->symbol (format "valid-~a" field-name))
   #:first-order (λ (vals) (andmap predicate vals))
   #:projection (λ (blame)
                  (λ (vals)
                    (define invalid-vals (filter (λ (val) (not (predicate val))) vals))
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     vals
                     (format "~a expected a list of cfes.\nThe following: ~a are not valid ~as in the given ~a"
                             cfe-type
                             invalid-vals
                             element-name
                             field-name))))))

(define valid-singleton-input
  (make-flat-contract
   #:name 'is-valid-singleton-input
   #:first-order (or/c symbol? number?)
   #:projection (λ (blame)
                  (λ (symbol)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     symbol
                     "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given")))))

(define singleton-cfexp/c
  (-> (and/c valid-singleton-input
             (make-flat-contract
              #:name 'is-valid-singleton-exp?
              #:first-order valid-alpha?
              #:projection (λ (blame)
                             (λ (symbol)
                               (current-blame-format format-error)
                               (raise-blame-error
                                blame
                                symbol
                                "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given")))))
      mk-singleton-cfexp?))

(define concat-cfexp/c
  (->* () #:rest (and/c (is-a-list/c "concat-cfexp")
                        (valid-listof/c cfexp? "concat-cfexp" "list of cfes" "cfe"))
      mk-concat-cfexp?))

(define union-cfexp/c
  (->* () #:rest (and/c (is-a-list/c "union-cfexp")
                        (valid-listof/c cfexp? "union-cfexp" "list of cfes" "cfe"))
      mk-union-cfexp?))

;;A contract to determine of the given symbol for a var-cfexp is valid
(define var-cfexp/c
  (-> (and/c (is-symbol?/c "var-cfexp expects a symbol as input, given")
         is-a-valid-symbol/c)
  mk-var-cfexp?))


(define kleene-cfexp/c
  (-> (make-flat-contract
          #:name 'is-cfe-for-kleene?
          #:first-order cfexp?
          #:projection (λ (blame)
                         (λ (symbol)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            symbol
                            "kleene-cfexp expects a cfe as input, given"))))
         mk-kleene-cfexp?))


(define is-var-cfexp?/c
  (and/c (is-cfexp/c "update-binding! expects a cfe as the first input, given")
         (make-flat-contract
          #:name 'is-var-cfexp?
          #:first-order mk-var-cfexp?
          #:projection (λ (blame)
                         (λ (symbol)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            symbol
                            "update-binding! expects a var-cfexp as the first input, given"))))))

(define update-binding!/c
  (-> is-var-cfexp?/c
      (and/c (is-symbol?/c "update-binding! expects a symbol as the second input, given") is-a-valid-symbol/c)
     (is-cfexp/c "update-binding! expects a cfe as the third input, given") 
      void?))