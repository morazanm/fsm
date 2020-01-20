#lang racket
(require test-engine/racket-tests "../components/stateTransitions.rkt" "../globals.rkt")


;; ------ PDAs -------
(set-machine-type 'pda)
(check-expect
 (getCurRule '((F (a a a b b b) ()) (S (a a a b b b) ())))
 '((S e e) (F e)))

(check-expect 
 (getCurRule '((F (a b b b) (c c)) (F (a a b b b) (c))))
 '((F a e) (F (c))))

(check-expect
 (getCurRule '((F (b) (c)) (F (b b) (c c))))
 '((F b (c)) (F e)))

(check-expect
 (getCurRule '((F () ()) (F (b) (c))))
  '((F b (c)) (F e)))
              
(check-expect
 (getCurRule '((F () ())))
 '((empty empty empty) (empty empty)))



(test)