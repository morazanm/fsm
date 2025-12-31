#lang racket/base

(require "../macros/shared/shared-predicates.rkt"
         "../macros/error-formatting.rkt"
         racket/bool
         "../cfg-struct.rkt"
         "cfexp-structs.rkt"
         racket/contract)

(provide singleton-cfexp/c
         concat-cfexp/c
         union-cfexp/c
         kleene-cfexp/c
         gen-cfexp-word/c
         cfe->cfg/c
         cfg->cfe/c
         pda->cfe/c
         cfe->pda/c)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CONTRACTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; valid-alpha-string?: any --> boolean
;; purpose: Returns true if the given input is a single alphabet character string,
;;          and false for every other input.
(define (valid-alpha-string? x)
  (define regex-pattern (regexp "^[a-z0-9]$"))
  (not (false? (and (string? x)
                    (regexp-match regex-pattern x)))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a cfexp
(define (is-cfexp/c message)
  (make-flat-contract
   #:name 'is-cfe?
   #:projection (λ (blame)
                  (λ (val)
                    (or (cfexp? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            message))))))))

;;(X -> Boolean) string string string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a listof X that satistfies the given predicate
(define (valid-listof/c predicate cfe-type field-name element-name)
  (make-flat-contract
   #:name (string->symbol (format "valid-~a" field-name))
   #:projection (λ (blame)
                  (λ (vals)
                    (define invalid-vals (filter (λ (val) (not (predicate val))) vals))
                    (or (null? invalid-vals)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            vals
                            (format "~a expected a list of cfes.\nThe following: ~a are not valid ~as in the given ~a"
                                    cfe-type
                                    invalid-vals
                                    element-name
                                    field-name)))))))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a symbol or natural number
(define valid-singleton-input
  (make-flat-contract
   #:name 'is-valid-singleton-input?
   #:projection (λ (blame)
                  (λ (val)
                    (or (valid-alpha-string? val)
                        ((λ ()
                          (current-blame-format format-error)
                               (raise-blame-error
                                blame
                                val
                                "singleton-cfexp expected a string of length one containing [a-z] or [0-9] as input, given"))))))))





;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is cfexp
(define valid-kleene-input
  (make-flat-contract
   #:name 'is-cfe-for-kleene?
   #:projection (λ (blame)
                  (λ (val)
                    (or (cfexp? val)
                        ((λ () (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "kleene-cfexp expects a cfe as input, given"))))))))

;;-> flat-contract
;;Purpose: Creates a flat contract that determines if the input is natural number
(define nat-num/c
  (make-flat-contract
   #:name 'is-natural-number?
   #:projection (λ (blame)
                  (λ (value)
                    (or (natural-number/c value)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            value
                            "gen-cfexp-word expects a natural number as an optional input, given"))))))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a cfg
(define (is-ndpda? message)
  (and/c procedure? 
         (make-flat-contract
          #:name 'is-ndpda?
          #:projection (λ (blame)
                         (λ (val)
                           (or (ndpda? val)
                               ((λ ()
                                  (current-blame-format format-error)
                                  (raise-blame-error
                                   blame
                                   val
                                   message)))))))))

;; string -> flat-contract
;;Purpose: Creates a flat contract that determines if the input is a pda
(define (is-cfg? message)
  (and/c struct?
         (make-flat-contract
          #:name 'is-cfg?
          #:projection (λ (blame)
                         (λ (val)
                           (or (cfg? val)
                               ((λ ()
                                  (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            message)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;COMBINATOR CONTRACTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define singleton-cfexp/c
  (-> valid-singleton-input 
      mk-singleton-cfexp?))

(define concat-cfexp/c
  (->* () #:rest (valid-listof/c cfexp? "concat-cfexp" "list of cfes" "cfe")
       (or/c mk-concat-cfexp?
             mk-singleton-cfexp?
             mk-null-cfexp?
             mk-empty-cfexp?)))

(define union-cfexp/c
  (->* () #:rest (valid-listof/c cfexp? "union-cfexp" "list of cfes" "cfe")       
       (or/c mk-union-cfexp?
             mk-singleton-cfexp?
             mk-null-cfexp?
             mk-empty-cfexp?)))



(define kleene-cfexp/c
  (-> valid-kleene-input
      (or/c mk-kleene-cfexp?
            mk-empty-cfexp?
            mk-null-cfexp?)))



(define gen-cfexp-word/c
  (->* ((is-cfexp/c "gen-cfexp-word expects a cfe as input, given"))
       #:rest (listof nat-num/c)
       (or/c symbol?
             (listof symbol?))))

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