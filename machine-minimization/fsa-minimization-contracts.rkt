#lang racket/base

(require
  "../fsm-core/private/macros/shared/shared-predicates.rkt"
  "../fsm-core/private/macros/error-formatting.rkt"
  racket/contract)

(provide minimization/c
         minimization-viz/c)


;;string -> contract
;;Purpose: A contract that checks if the input is a dfa or ndfa
(define (valid-minimization-input func-name)
  (make-flat-contract
   #:name  'valid-minimization-input
   #:first-order procedure?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (procedure? val)
                             (or (dfa? val)
                                 (ndfa? val)))
                        ((λ ()
                          (current-blame-format format-error)
                          (raise-blame-error
                           blame
                           val
                           (format "~a expects as input a dfa or ndfa, given" func-name)))))))))

;;Purpose: A contract to check that minimize-dfa only works on ndfas and dfas
(define minimization/c (-> (valid-minimization-input "minimize-dfa") dfa?))

;;Purpose: A contract to check that minimization-viz only works on ndfas and dfas
(define minimization-viz/c (-> (valid-minimization-input "minimization-viz") void))
