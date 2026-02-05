#lang racket/base
(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt")

(provide warn:fsm:app:regexp?
         (struct-out warn:fsm:app:regexp:invalid-words)
         (struct-out warn:fsm:app:regexp:accept)
         (struct-out warn:fsm:app:regexp:reject))

(struct warn:fsm:app:regexp warn:fsm:app ())

(struct warn:fsm:app:regexp:invalid-words warn:fsm:app:regexp ()
  #:methods gen:printable-fsm-err-vals
  [])

(struct warn:fsm:app:regexp:accept warn:fsm:app:regexp ()
  #:methods gen:printable-fsm-err-vals
  [])

(struct warn:fsm:app:regexp:reject warn:fsm:app:regexp ()
  #:methods gen:printable-fsm-err-vals
  [])