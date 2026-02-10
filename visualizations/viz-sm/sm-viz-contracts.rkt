#lang racket/base

(require racket/contract
         "../../fsm-core/private/macros/shared/shared-predicates.rkt"
         "../../fsm-core/private/macros/error-formatting.rkt"
         "../../fsm-core/private/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;PREDICATES

;;
(define (machine? x)
  (or (dfa? x)
      (ndfa? x)
      (ndpda? x)
      (tm? x)
      (mttm? x)))

(define (valid-fsm-word? los)
  (andmap
   (λ (x) (or (valid-alpha? x)
              (eq? x BLANK)
              (eq? x LM)))
   los))

(define (valid-palette-input? x)
  (or (eq? x 'default)
      (eq? x 'prot)
      (eq? x 'deut)
      (eq? x 'trit)))

;;;;;;;;;;;;;;;;;;;;;;;CONTRACTS

(define is-machine/c
  (make-flat-contract
   #:name 'is-machine?
   #:first-order machine?
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     "sm-viz expects a finite state machine as input, given")))))

(define is-valid-machine/c
  (and/c procedure? is-machine/c))


(define is-word/c
  (make-flat-contract
   #:name 'is-word?
   #:first-order valid-fsm-word?
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     "sm-viz expects a list of fsm symbols as input, given")))))

(define is-valid-word/c
  (and/c list? is-word/c))


(define valid-add-dead-input/c
  (make-flat-contract
   #:name 'is-boolean-for-add-dead?
   #:first-order boolean?
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     "The optional argument, add-dead, expects a boolean as input, given")))))


(define (valid-number-input/c arg-name)
  (make-flat-contract
   #:name 'is-positve-int?
   #:first-order (and/c positive? natural-number/c)
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     (format "The optional argument, ~a, expects a boolean as input, given" arg-name))))))


(define valid-palette-input/c
  (make-flat-contract
   #:name 'is-valid-symbol?
   #:first-order valid-palette-input?
   #:projection (λ (blame)
                  (λ (x)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     x
                     "The optional argument, palette, expects a boolean as input, given")))))

;;;;;;;;;;;;;;;;;;;;;CONTRACT COMBINATOR



(define sm-viz/c
  (->* (is-valid-machine/c is-valid-word/c)
       (#:add-dead valid-add-dead-input/c
        #:cut-off (valid-number-input/c "cut-off")
        #:head-pos (valid-number-input/c "head-pos")
        #:palette valid-palette-input/c)
       void?))





