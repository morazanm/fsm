#lang racket
(require rackunit "../../Macros/factory.rkt" "../test-helpers.rkt")

#| SINGLE LEVEL OF LISTS |#
(define (josh data) 'josh)
(define (marco data) 'marco)
(define (sena data) 'sena)
(define (sach data) 'sach)
(define (other data) 'other)
(define (other2 data) 'other2)
  
(factory fsa
         [(number? number? symbol?) <- other]
         [(_ number? number?) <- josh]
         [(number? _ number?) <- marco]
         [(_ _ _) <- sach]
         [(_ _) <- sena])

(check-equal? (fsa-factory '((hi 2 3))) 'josh "Should return 'josh")
(check-equal? (fsa-factory '((10 1 hello))) 'other "Should return 'other")
(check-equal? (fsa-factory '((hi hi hi))) 'sach "Should return 'sach")
(check-equal? (fsa-factory '((1 hi 1))) 'marco "Should return 'marco")
(check-equal? (fsa-factory '((61 62))) 'sena "Should return 'sena")


#| TWO LEVELS OF LISTS |#
(factory pda
         [((symbol? symbol?)(number? number? symbol?)) <- sena]
         [((_ _)(_ _ _)) <- josh]
         [(number? _ symbol?) <- other2]
         [((number? _ number?) (_ _)) <- marco]
         [((symbol? _ symbol?) (_ _)) <- sach]
         [((_ _ _) (_ _)) <- other])

  
(check-equal? (pda-factory '(((h j) (4 1 j)))) 'sena "Should return 'sena")
(check-equal? (pda-factory '(((1 j) (4 1 j)))) 'josh "Should return 'josh")
(check-equal? (pda-factory '(((1 j 1) (1 j)))) 'marco "Should return 'marco")
(check-equal? (pda-factory '(((j j j) (1 j)))) 'sach "Should return 'sach")
(check-equal? (pda-factory '(((1 j j) (1 j)))) 'other "Should return 'other")
(check-equal? (pda-factory '((1 81 j))) 'other2 "Should return 'other2")
