#lang racket
(require "../../FSM-Visualization/components/stateTransitions.rkt" "../../FSM-Visualization/globals.rkt" "../test-helpers.rkt")


(module+ test
  (require rackunit)

  (define getCurrentRule
    (test-suite "Tests getCurRule Function"
                (test-case "PDA"
                           (set-machine-type 'pda)
                           (check-equal? (getCurRule '((F (a a a b b b) ()) (S (a a a b b b) ()))) '((S ε ε) (F ε)))
                           (check-equal? (getCurRule '((F (a b b b) (c c)) (F (a a b b b) (c)))) '((F a ε) (F (c))))
                           (check-equal? (getCurRule '((F (b) (c)) (F (b b) (c c)))) '((F b (c)) (F ε)))
                           (check-equal? (getCurRule '((F () ()) (F (b) (c)))) '((F b (c)) (F ε)))
                           (check-equal? (getCurRule '((F () ()))) '((empty empty empty) (empty empty)))
                           (check-equal? (getCurRule '((M (a a b c b a a) ()) (S (a a b c b a a) ()))) '((S ε ε) (M ε)))
                           (check-equal? (getCurRule '((M (b c b a a) (a a)) (M (a b c b a a) (a)))) '((M a ε) (M (a))))
                           (check-equal? (getCurRule '((M (c b a a) (b a a)) (M (b c b a a) (a a)))) '((M b ε) (M (b)))))))


  (test-all 'verbose
            (getCurrentRule))



  );; end submodule