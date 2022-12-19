#lang racket
(require "../../fsm-gui/inputFactory.rkt" "../test-helpers.rkt" 2htdp/image)


(define dfa-rules '((A b C) (A a B) (B c A) (ds a ds) (ds b ds) (ds c ds) (A c ds) (B a ds) (B b ds) (C a ds) (C b ds) (C c ds)))
(define dfa-rules2 (list (list 'A 'a 'B) (list 'B 'b 'B) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'a 'B) (list 'C 'b 'B) (list 'C 'a 'A) (list 'A 'a 'B) (list 'C 'c 'B)))
(define pda-rules (list (list (list 'DS 'a 'DS) (list 'DS 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a))))

#|
(overlay
                            (rectangle 830 75 "outline" "blue")
                            (ruleFactory dfa-rules 'dfa 0 (list 'B 'b 'B)))


                           (define dfa-rules2 (list (list 'A 'a 'B) (list 'B 'b 'B) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'b 'A) (list 'A 'a 'B) (list 'C 'b 'B) (list 'C 'a 'A) (list 'A 'a 'B) (list 'C 'c 'B)))
                           (overlay
                            (rectangle 830 75 "outline" "blue")
                            (ruleFactory dfa-rules2 'dfa 0 (list 'B 'b 'B)))


                           (define pda-rules (list (list (list 'DS 'a 'DS) (list 'DS 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a)) (list (list 'B 'a (list 'A 'D)) (list 'b 'a))))
                           (overlay
                            (rectangle 830 75 "outline" "blue")
                            (ruleFactory pda-rules 'pda 0 (list (list 'A 'a 'B) (list 'b 'a))))
|#


(module+ test
  (require rackunit rackunit/gui)

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