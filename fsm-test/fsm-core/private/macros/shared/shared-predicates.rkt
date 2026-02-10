#lang racket/base

(module+ test
  (require rackunit
           "../../../../../fsm-core/private/macros/shared/shared-predicates.rkt"
           "../../../../../fsm-core/private/constants.rkt")
  ;valid-list-of tests
  (check-equal? (valid-list-of '(A B C D E) symbol?) #t)
  (check-equal? (valid-list-of '(A B C 1 D E) symbol?) #f)

  ;valid-state? tests
  (check-equal? (valid-state? 'A) #t)
  (check-equal? (valid-state? 'A-1) #t)
  (check-equal? (valid-state? 'A-123) #t)
  (check-equal? (valid-state? '1) #f)
  (check-equal? (valid-state? 1) #f)
  (check-equal? (valid-state? 'AB) #f)
  (check-equal? (valid-state? DEAD) #t)
  (check-equal? (valid-state? 'A-1ifodsijfodsijf) #f)
  (check-equal? (valid-state? 'ajfdA) #f)

  ;valid-alpha? tests
  (check-equal? (valid-alpha? 'a) #t)
  (check-equal? (valid-alpha? '1) #t)
  (check-equal? (valid-alpha? 1) #t)
  (check-equal? (valid-alpha? 'A) #t)
  (check-equal? (valid-alpha? 'a1) #f)
  (check-equal? (valid-alpha? 'Aa) #f)
  (check-equal? (valid-alpha? EMP) #f)

  ;start-in-states? tests
  (check-equal? ((start-in-states? '(A B C)) 'A) #t)
  (check-equal? ((start-in-states? '(A B C)) 'D) #f)

  ;valid-start? tests
  (check-equal? ((valid-start? '(A B C)) 'A) #t)
  (check-equal? ((valid-start? '(A B C)) 'A-1) #t)
  (check-equal? ((valid-start? '(A B C)) DEAD) #t)
  (check-equal? ((valid-start? '(A B C)) 1) #f)
  (check-equal? ((valid-start? '(A B C)) 'A1) #f)
  (check-equal? ((valid-start? '(A B C)) '(A)) #f)

  ;valid-finals? tests
  (check-equal? ((valid-finals? '(A B C)) '(A)) #t)
  (check-equal? ((valid-finals? '(A B C)) '(A-1)) #f)
  (check-equal? ((valid-finals? '(A B C)) '(D)) #f)

  ;invalid-finals tests
  (check-equal? (invalid-finals '(A B C) '(B)) '())
  (check-equal? (invalid-finals '(A B C) '(D)) '(D))
  (check-equal? (invalid-finals '(A B C) '(B D)) '(D))
  (check-equal? (invalid-finals '(A B C) '(B C 1 D)) '(1 D))

  ;return-duplicates tests
  (check-equal? (return-duplicates '(A A B C)) '(A))
  (check-equal? (return-duplicates '(A A B B B C)) '(A B))
  (check-equal? (return-duplicates '(A B C)) '())    
  )