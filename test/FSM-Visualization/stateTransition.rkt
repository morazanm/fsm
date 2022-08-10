#lang racket
(require "../../FSM-Visualization/components/stateTransitions.rkt"
         "../../FSM-Visualization/globals.rkt"
         "../../constants.rkt"
         "../test-helpers.rkt")


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
                           (check-equal? (getCurRule '((M (c b a a) (b a a)) (M (b c b a a) (a a)))) '((M b ε) (M (b)))))



                (test-case "MTTM"
                           (set-machine-type 'mttm)
                       
                           (check-equal? (getCurRule '((Q (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                                                       (S (0 (_ a a b b c c d d)) (0 (_)) (0 (_)) (0 (_)))))
                                         `((S (,BLANK ,BLANK ,BLANK ,BLANK)) (Q (R R R R)))
                                         "Move Right on tape")
                           ;; Mopve Left
                           (check-equal? (getCurRule '((E (8 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
                                                       (D (9 (_ a a b b c c d d _)) (3 (_ b b _)) (3 (_ c c _)) (3 (_ d d _)))))
                                         `((D (,BLANK ,BLANK ,BLANK ,BLANK)) (E (L L L L)))
                                         "Move Left on tape")
                           ;; Write a
                           (check-equal? (getCurRule '((A (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
                                                       (Q (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))))
                                         `((Q (a ,BLANK ,BLANK ,BLANK)) (A (a ,BLANK ,BLANK ,BLANK)))
                                         "Write a on tape")

                           ;; Buggy transition 1
                           (check-equal? (getCurRule `((E (2 (@ _ a a b b c c d d _)) (1 (_ b b _)) (1 (_ c c _)) (1 (_ d d _)))
                                                       (E (3 (@ _ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))))
                                         `((E (a b c d)) (E (L L L L)))
                                         "Was buggy trans 1"))))
                           



  


  (test-all 'verbose
            (getCurrentRule))



  );; end submodule