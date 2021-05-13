#lang racket
(require rackunit "../../Macros/adapter.rkt" "../test-helpers.rkt")

(define (fsa-special-rule-to-string rules)
  (foldl (lambda (v accum) (string-append accum (if (number? v)
                                                    (number->string v)
                                                    (symbol->string v))))
         "SPECIAL "
         rules))
  
(define (fsa-rule-to-string rules)
  (foldl (lambda (v accum) (string-append accum (symbol->string v)))
         ""
         rules))
  
(define (pda-rule-to-string rules)
  (foldl (lambda (v accum) (string-append accum (symbol->string v)))
         ""
         (flatten rules)))
  
(define (tm-rule-to-string rules)
  (foldl (lambda (v accum) (string-append accum (symbol->string v)))
         ""
         (flatten rules)))
  
(adapter graph
         [(_ number? _) <- fsa-special-rule-to-string]
         [(_ _ _) <- fsa-rule-to-string]
         [((_ _ _) (_ _)) <- pda-rule-to-string]
         [((_ _) (_ _)) <- tm-rule-to-string])




(check-equal? (graph-adapter '((A a B) (B a B))) '("AaB" "BaB") "fsa rule should return AaBBaB")
(check-equal? (graph-adapter '((A 1 B) (B a B))) '("SPECIAL A1B" "SPECIAL BaB") "special fsa rule should return AaBHELLO BaB")
(check-equal? (graph-adapter '(((A a A) (A a)) ((B b B) (B b)))) '("AaAAa" "BbBBb") "pda rule should return AaAAaBbBBb")
(check-equal? (graph-adapter '(((A a) (B b)) ((C c) (D d)))) '("AaBb" "CcDd") "tm rule should return AaBbCcDd")