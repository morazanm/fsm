#lang racket/base
(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt"
         "../fsm-type-predicates.rkt")

(provide warn:fsm:test?
         (struct-out warn:fsm:test:no-vals)
         (struct-out warn:fsm:test:no-cases)
         (struct-out warn:fsm:test:no-fsm-val)
         (struct-out warn:fsm:test:invalid-fsm-val))

(struct warn:fsm:test warn:fsm ())

(struct warn:fsm:test:invalid-fsm-val warn:fsm:test ()
  #:methods gen:printable-fsm-err-vals
  [(define (anonymous-single-failure err-struct)
     (named-single-failure err-struct))
   (define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. ~s is not a valid FSM value that can be tested."
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))))])

(struct warn:fsm:test:no-vals warn:fsm:test ()
  #:methods gen:printable-fsm-err-vals
  [(define (anonymous-single-failure err-struct)
     "Step 2 of the design recipe has not been successfully completed. This unit-test does not contain any cases to test, nor any FSM value to test with."
     )
   (define (named-single-failure err-struct)
     "Step 2 of the design recipe has not been successfully completed. This unit-test does not contain any cases to test, nor any FSM value to test with."
     )])

(struct warn:fsm:test:no-fsm-val warn:fsm:test ()
  #:methods gen:printable-fsm-err-vals
  [(define (anonymous-single-failure err-struct)
   "Step 2 of the design recipe has not been successfully completed. This test does not contain any FSM value to test with.")
   (define (named-single-failure err-struct)
     (anonymous-single-failure err-struct))])

(struct warn:fsm:test:no-cases warn:fsm:test ()
  #:methods gen:printable-fsm-err-vals
  [(define (anonymous-single-failure err-struct)
     (let ([fsm-val-type (whatami? (warn:fsm-fsm-expr err-struct))])
       (if (eq? fsm-val-type 'grammar)
         (format "Step 4 of the design recipe has not been successfully completed. The unit-test for the constructed ~a does not contain any cases to test."
                 fsm-val-type)
         (format "Step 2 of the design recipe has not been successfully completed. The unit-test for the constructed ~a does not contain any cases to test."
                 fsm-val-type))))

   (define (named-single-failure err-struct)
     (let ([fsm-val-type (whatami? (warn:fsm-fsm-expr err-struct))])
       (if (eq? fsm-val-type 'grammar)
         (format "Step 4 of the design recipe has not been successfuly completed. The unit-test for the constructed ~a, ~s, does not contain any cases to test."
                 fsm-val-type
                 (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
         (format "Step 2 of the design recipe has not been successfuly completed. The unit-test for the constructed ~a, ~s, does not contain any cases to test"
                 fsm-val-type
                 (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
         ))
     )])