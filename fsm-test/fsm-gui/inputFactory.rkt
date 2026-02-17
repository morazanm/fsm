#lang racket/base

(module+ test
  (require rackunit
           "../../fsm-gui/inputFactory.rkt"
           "../test-helpers.rkt")
  
  (define input-rendering
    (test-suite "Checking the inputs for input rendering"
                (test-case "PDA"
                           (check-equal? (create-pda-rule (list (list 'a 'b 'c) (list 'd 'e))) "((a b c) (d e))" "Pop and push are not lists")
                           (check-equal? (create-pda-rule (list (list 'a 'b (list 'J 'K 'L)) (list 'd (list 'D 'E)))) "((a b (J K L)) (d (D E)))" "Pop and push are both lists")
                           (check-equal? (create-pda-rule (list (list 'a 'b (list 'J 'K 'L)) (list 'd 'e))) "((a b (J K L)) (d e))" "Just pop is a list")
                           (check-equal? (create-pda-rule (list (list 'a 'b 'e) (list 'd (list 'D 'E)))) "((a b e) (d (D E)))" "Just push is a list"))
                (test-case "DFA/NDFA"
                           (check-equal? (create-dfa-ndfa-rule (list 'A 'a 'B)) "(A a B)"))))

  (test-all 'verbose
            (input-rendering))
  
  ) ;; end submodule