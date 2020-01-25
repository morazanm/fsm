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

(check-expect
 (getCurRule '((M (a a b c b a a) ()) (S (a a b c b a a) ())))
 '((S e e) (M e)))

(check-expect
 (getCurRule '((M (b c b a a) (a a)) (M (a b c b a a) (a))))
 '((M a e) (M (a))))

(check-expect
 (getCurRule '((M (c b a a) (b a a)) (M (b c b a a) (a a))))
 '((M b e) (M (b))))



(test)