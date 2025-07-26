#lang racket/base

(require "../fsm-core/private/macros/shared/shared-predicates.rkt"
        "../fsm-core/private/macros/rules/rules-flat-contracts.rkt"
         "../fsm-core/private/macros/error-formatting.rkt"
         "../fsm-core/private/grammar-getters.rkt"
         "../fsm-core/private/cfg.rkt"
         "cfexp-structs.rkt"
         racket/contract)

(provide singleton-cfexp/c
         concat-cfexp/c
         union-cfexp/c
         var-cfexp/c
         kleene-cfexp/c
         update-binding!/c
         gen-cfexp-word/c
         valid-state?
         valid-alpha?
         correct-cfg-rules/c
         cfe->cfg/c
         cfg->cfe/c
         pda->cfe/c
         cfe->pda/c
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PREDICATES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;X -> boolean
;;Purpose: Determines if the the given x is a cfexp
(define (cfexp? x)
  (or (mk-null-cfexp? x)
      (mk-empty-cfexp? x) 
      (mk-singleton-cfexp? x)
      (mk-var-cfexp? x) 
      (mk-concat-cfexp? x) 
      (mk-union-cfexp? x) 
      (mk-kleene-cfexp? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONTRACTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string -> flat-contract
;;Purpose: Creates a flat contract to determine if the given list or listof list has at least 2 elements
(define (at-least-two/c message)
  (make-flat-contract
   #:name 'at-least-two?
   #:first-order (λ (x) (or (>= (length x) 2)
                            (>= (length (car x)) 2)))
   #:projection (λ (blame)
                         (λ (symbol)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            symbol
                            (format "~a needs at least 2 cfes, given" message))))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a symbol
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
;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a structure
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

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a cfexp
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

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a valid fsm symbol
(define is-a-valid-symbol/c
  (make-flat-contract
          #:name 'is-symbol?
          #:first-order valid-state?
          #:projection (λ (blame)
                         (λ (val)
                           (current-blame-format format-start-error)
                           (raise-blame-error
                            blame
                            val
                            (format "The given symbol, ~a, is not a valid FSM symbol. A valid FSM symbol is either:\n an uppercase letter [A-Z] or an uppercase letter [A-Z], a dash, and number." val))))))

;;string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a list
(define (is-a-list/c cfe-type)
    (make-flat-contract
     #:name 'is-a-list/c
     #:first-order (lambda (val) (or (list? val) (listof list?)))
     #:projection (lambda (blame)
                    (lambda (val)
                      (current-blame-format format-error)
                      (raise-blame-error
                       blame
                       val
                       (format "~a expects an arbitrary amount of cfes, given" cfe-type))))))

;;(X -> Boolean) string string string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a listof X that satistfies the given predicate
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

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a symbol or natural number
(define valid-singleton-input
  (make-flat-contract
   #:name 'is-valid-singleton-input
   #:first-order (or/c symbol? natural-number/c)
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     val
                     "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given")))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a valid alphabet symbol
(define valid-singleton-exp
  (make-flat-contract
              #:name 'is-valid-singleton-exp?
              #:first-order valid-alpha?
              #:projection (λ (blame)
                             (λ (val)
                               (current-blame-format format-error)
                               (raise-blame-error
                                blame
                                val
                                "singleton-cfexp expected a symbol [a-Z] or number [0-9] as input, given")))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a var-cfexp
(define is-var-cfexp?/c
  (and/c (is-cfexp/c "update-binding! expects a cfe as the first input, given")
         (make-flat-contract
          #:name 'is-var-cfexp?
          #:first-order mk-var-cfexp?
          #:projection (λ (blame)
                         (λ (val)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "update-binding! expects a var-cfexp as the first input, given"))))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is cfexp
(define valid-kleene-exp
  (make-flat-contract
          #:name 'is-cfe-for-kleene?
          #:first-order cfexp?
          #:projection (λ (blame)
                         (λ (val)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "kleene-cfexp expects a cfe as input, given")))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is natural number
(define nat-num/c
  (make-flat-contract
   #:name 'is-natural-number?
   #:first-order natural-number/c
   #:projection (λ (blame)
                  (λ (value)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     value
                     "Expected a natural number, given")))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a cfg
(define (is-ndpda? message)
  (and/c procedure? 
         (make-flat-contract
          #:name 'is-ndpda?
          #:first-order ndpda?
          #:projection (λ (blame)
                         (λ (x)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            x
                            message))))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a pda
(define (is-cfg? message)
  (and/c is-struct/c
         (make-flat-contract
          #:name 'is-cfg?
          #:first-order cfg?
          #:projection (λ (blame)
                         (λ (x)
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            x
                            message))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;COMBINATOR CONTRACTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define singleton-cfexp/c
  (-> (and/c valid-singleton-input
             valid-singleton-exp)
      mk-singleton-cfexp?))

(define concat-cfexp/c
  (->* () #:rest (and/c (is-a-list/c "concat-cfexp")
                        (at-least-two/c "concat-cfexp")
                        (or/c (valid-listof/c cfexp? "concat-cfexp" "list of cfes" "cfe")
                              (valid-listof/c (listof cfexp?) "concat-cfexp" "list of cfes" "cfe")))
      (or/c mk-concat-cfexp?
            mk-empty-cfexp?)))

(define union-cfexp/c
  (->* () #:rest (and/c (is-a-list/c "union-cfexp")
                        (at-least-two/c "union-cfexp")
                        (or/c (valid-listof/c cfexp? "union-cfexp" "list of cfes" "cfe") 
                              (valid-listof/c (listof cfexp?) "union-cfexp" "list of cfes" "cfe")))
       (or/c mk-union-cfexp?
             mk-empty-cfexp?)))

(define var-cfexp/c
  (-> (and/c (is-symbol?/c "var-cfexp expects a symbol as input, given")
         is-a-valid-symbol/c)
  mk-var-cfexp?))

(define kleene-cfexp/c
  (-> valid-kleene-exp
      mk-kleene-cfexp?))

(define update-binding!/c
  (-> is-var-cfexp?/c
      (and/c (is-symbol?/c "update-binding! expects a symbol as the second input, given")
             is-a-valid-symbol/c)
      (is-cfexp/c "update-binding! expects a cfe as the third input, given") 
      void?))

(define gen-cfexp-word/c
  (->* ((is-cfexp/c "gen-cfexp-word expects a cfe as input, given"))
       #:rest (listof nat-num/c)
       (or/c symbol?
             (listof symbol?)
             (listof nat-num/c)
             string?)))

(define cfe->cfg/c
  (-> (is-cfexp/c "cfe->cfg expects a cfe as input, given")
      (is-cfg? "cfe->cfg broke its own contract, should produce a cfg instead of")))

(define cfg->cfe/c
  (-> (is-cfg? "cfg->cfe expects a cfg as input, given")
      (is-cfexp/c "cfg->cfe broke its own contract, should produce a cfexp instead of")))

(define pda->cfe/c
  (-> (is-ndpda? "pda->cfe expects a pda as input, given")
      (is-cfexp/c "pda->cfe broke its own contract, should produce a cfexp instead of")))

(define cfe->pda/c
  (-> (is-cfexp/c "cfe->pda expects a cfe as input, given")
      (is-ndpda? "cfe->pda broke its own contract, should produce a pda instead of")))