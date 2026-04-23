#lang racket/base

(module+ test
(require rackunit
           "../../../../fsm-core/private/macros/error-formatting.rkt"
           "../../../../fsm-core/private/macros/rules/rules-predicates.rkt"
           )
  ;format-incorrect-rule-error tests
  (check-equal? (format-incorrect-rule-error (make-invalid-rule '(A b C)
                                                                '("This is the first error message."
                                                                  "This is the second error message."
                                                                  "This is the third error message.")))
                "Rule (A b C):\n  This is the first error message.\n  This is the second error message.\n  This is the third error message.\n")
  )