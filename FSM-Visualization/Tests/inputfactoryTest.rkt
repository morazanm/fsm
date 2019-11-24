#lang racket
(require test-engine/racket-tests 2htdp/image "../inputFactory.rkt")

#| THIS IS A TESTING FILE FOR THE inputFactory class
     written by Joshua Schappel on 11/19/2019
|#



;; *** PDA list->String ***
;; pop and push are not lists 
(check-expect 
 (create-pda-rule (list (list 'a 'b 'c) (list 'd 'e)))
 "((a b c) (d e))")

;; pop and push are both lists 
(check-expect 
 (create-pda-rule (list (list 'a 'b (list 'J 'K 'L)) (list 'd (list 'D 'E))))
 "((a b (J K L)) (d (D E)))")

;; Just pop is a list
(check-expect 
 (create-pda-rule (list (list 'a 'b (list 'J 'K 'L)) (list 'd 'e)))
 "((a b (J K L)) (d e))")

;; Just push is a list
(check-expect 
 (create-pda-rule (list (list 'a 'b 'e) (list 'd (list 'D 'E))))
 "((a b e) (d (D E)))")



;; *** DFA/NDFA list->String ***
(check-expect
 (create-dfa-ndfa-rule (list 'A 'a 'B))
 "(A a B)")

;; *** FACTORY TESTS ***
(define dfa-rules '((A b C) (A a B) (B c A) (ds a ds) (ds b ds) (ds c ds) (A c ds) (B a ds) (B b ds) (C a ds) (C b ds) (C c ds)))

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


(test)