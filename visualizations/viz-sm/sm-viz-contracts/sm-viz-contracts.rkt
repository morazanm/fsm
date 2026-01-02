#lang racket/base

(require racket/contract
         racket/list
         "../../../fsm-core/private/macros/validation/validation-predicates.rkt"
         "../../../fsm-core/private/macros/shared/shared-predicates.rkt"
         "../../../fsm-core/private/macros/error-formatting.rkt"
         "../../../fsm-core/private/constants.rkt")

(provide sm-viz/c)
;;;;;;;;;;;;;;;;ERROR MESSAGE FORMATTERS


;;string string -> string
;;Purpose: Formats the error messages for the optional arguments for sm-viz
(define (keyword-arg-formatter arg-name arg-type)
  (format "The keyword argument, #:~a, expects a ~a as input, given" arg-name arg-type))

;;;;;;;;;;;;;;;;;;;;;PREDICATES

;;X -> boolean
;;Purpose: Determines if the given X is a finite-state machine
(define (machine? x)
  (and (procedure? x)
       (or (dfa? x)
           (ndfa? x)
           (ndpda? x)
           (tm? x)
           (mttm? x))))

;;X -> boolean
;;Purpose: Determines if the given X has an invariant-like structure e.i. (list symbol procedure)
(define (invariant? X)
  ;;list -> Boolean
  ;;Purpose: Determines if the given list is length 2
  (define (length-is-two? L)
    (= (length L) 2))
  (and (list? X)
       (length-is-two? X)
       (symbol? (car X))
       (procedure? (cadr X))))

;;(listof symbol) -> boolean
;;Purpose: Determines if the given (listof symbol) is a valid fsm word)
(define (valid-fsm-word? los)
  ;;X -> boolean
  ;;Purpose: Determines if X is a lowercase roman alphabet symbol, an arabic numeral, LM or BLANK
  (define (valid-alpha? x)
    (define regex-pattern (regexp "^[a-z0-9@_]$"))
    (and (or (symbol? x) (and (number? x) (<= 0 x 9)))
         (not (not (regexp-match regex-pattern
                                 (if (symbol? x)
                                     (symbol->string x)
                                     (number->string x)))))))
  (andmap valid-alpha? los))


;;X -> boolean
;;Purpose: Determines if the given X is one of the colorblind options
(define (valid-palette-input? x)
  (or (eq? x 'default)
      (eq? x 'prot)
      (eq? x 'deut)
      (eq? x 'trit)))

;;;;;;;;;;;;;;;;;;;;;;;CONTRACTS

(define is-machine/c
  (make-flat-contract
   #:name 'is-machine?
   #:projection (λ (blame)
                  (λ (val)
                    (or (machine? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "sm-viz expects a finite state machine as input, given"))))))))




(define is-word/c
  (make-flat-contract
   #:name 'is-word?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (list? val)
                             (valid-fsm-word? val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "sm-viz expects a list of fsm symbols [a-z] or [0-9] as input, given"))))))))


(define (valid-add-dead-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-boolean-for-add-dead?
   #:projection (λ (blame)
                  (λ (val)
                    (or (boolean? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))


(define (valid-headpos-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-natnumber?
   #:projection (λ (blame)
                  (λ (val)
                    (or (natural-number/c val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))


(define (valid-cutoff-input/c arg-name arg-type)
  (define geq-to-one/c (>=/c 1))
  
  (make-flat-contract
   #:name 'is-natnumber?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (natural-number/c val)
                               (geq-to-one/c val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

(define (valid-palette-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-valid-symbol?
   #:projection (λ (blame)
                  (λ (val)
                    (or (valid-palette-input? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

(define valid-invariant-input/c
  (make-flat-contract
   #:name 'is-valid-symbol?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (map list? val)
                             (andmap invariant? val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            (if (list? val)
                                (filter-not invariant? val)
                                val)
                            "Expected an arbitrary amount of (list <state> <invariant>), given"))))))))

;;;;;;;;;;;;;;;;;;;;;CONTRACT COMBINATOR



(define sm-viz/c
  (->* (is-machine/c is-word/c)
       (#:add-dead (valid-add-dead-input/c "add-dead" "boolean")
        #:cut-off (valid-cutoff-input/c "cut-off" "postive integer greater than 0")
        #:head-pos (valid-headpos-input/c "head-pos" "natural number")
        #:palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]"))
       #:rest valid-invariant-input/c
       void?))






