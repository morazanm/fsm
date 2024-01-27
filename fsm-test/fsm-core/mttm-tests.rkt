#lang racket
(require "../../main.rkt")



(define ADD (make-mttm '(S A T U V H Q)
                       `(i)
                       'S
                       '(H)
                       `(((S (,BLANK ,BLANK ,BLANK)) (A (R R R)))
                         ((A (i ,BLANK ,BLANK)) (A (,BLANK i ,BLANK)))
                         ((A (,BLANK i ,BLANK)) (A (R R ,BLANK)))
                         ((A (,BLANK ,BLANK ,BLANK)) (T (R ,BLANK ,BLANK)))
                         ((T (i ,BLANK ,BLANK)) (T (,BLANK ,BLANK i)))
                         ((T (,BLANK ,BLANK i)) (T (R ,BLANK R)))
                         ((T (,BLANK ,BLANK ,BLANK)) (Q (L ,BLANK ,BLANK)))
                         ((Q (,BLANK ,BLANK ,BLANK)) (U (L L L)))
                         ((U (,BLANK i i)) (U (i i ,BLANK)))
                         ((U (i i ,BLANK)) (U (L i L)))
                         ((U (,BLANK i ,BLANK)) (V (,BLANK i ,BLANK)))
                         ((V (,BLANK i ,BLANK)) (V (i ,BLANK ,BLANK)))
                         ((V (i ,BLANK ,BLANK)) (V (L L ,BLANK)))
                         ((V (,BLANK ,BLANK ,BLANK)) (H (,BLANK ,BLANK ,BLANK))))
                       3))

;; a^nb^nc^nd^n
;; four tapes
;; PRE: _w_ T1 head starts at position 0 (blank before w)
;;      _
;;      _
;;      _
;; POST: Y for accept and N for reject
;; HOW: Skip a's; copy b's, c's, d's to T2, T3, and T4. move all heaps to
;;      starting blank, match a's, b's, c's, and d's
(define a^nb^nc^nd^n (make-mttm
                      '(S A Y N Q F B C D E)
                      '(a b c d)
                      'S
                      '(Y N)
                      `(;; Move to pos 1 on all heads
                        ((S (,BLANK ,BLANK ,BLANK ,BLANK)) (Q (R R R R)))
                        ;; check for empty
                        ((Q (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((Q (a ,BLANK ,BLANK ,BLANK)) (A (a ,BLANK ,BLANK ,BLANK)))
                        ;; no a's reject
                        ((Q (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ((Q (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ((Q (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Skip the a's on T1
                        ((A (a ,BLANK ,BLANK ,BLANK)) (A (R ,BLANK ,BLANK ,BLANK)))
                        ((A (b ,BLANK ,BLANK ,BLANK)) (B (b ,BLANK ,BLANK ,BLANK)))
                        ;; No b's reject
                        ((A (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((A (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ((A (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the b's on T1 to T2
                        ((B (b ,BLANK ,BLANK ,BLANK)) (B (b b ,BLANK ,BLANK)))
                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((B (b b ,BLANK ,BLANK)) (B (R R ,BLANK ,BLANK)))
                        ((B (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK ,BLANK ,BLANK)))
                        ;; No c's reject
                        ((B (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((B (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the c's on T1 to T3
                        ((C (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK c ,BLANK)))
                        ((C (c ,BLANK c ,BLANK)) (C (R ,BLANK R ,BLANK)))
                        ((C (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK ,BLANK)))
                        ;; No d's reject
                        ((C (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
                        ((C (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((C (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ;; Copy the d's on T1 to T4
                        ((D (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK d)))
                        ((D (d ,BLANK ,BLANK d)) (D (R ,BLANK ,BLANK R)))
                        ((D (,BLANK ,BLANK ,BLANK ,BLANK)) (E (L L L L)))
                        ;; non d reject
                        ((D (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((D (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
                        ((D (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
                        ;; Match a's, b's, c's, and d's to accept. Otherwise, reject if BLANK read and num of BLANK is < 4
                        ((E (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK))) ;; accept
                        ((E (d b c d)) (E (L b c d))) ;; skip d's on T1
                        ((E (c b c d)) (E (L b c d))) ;; skip c's on T1 
                        ((E (b b c d)) (E (L b c d))) ;; skip c's on T1
                        ((E (a b c d)) (E (L L L L))) ;; on match move all heads L
                        ;; Wrong number of blanks reject
                        ((E (,BLANK b c d)) (N (,BLANK b c d)))
                        ((E (a ,BLANK c d)) (N (a ,BLANK c d)))
                        ((E (a b ,BLANK d)) (N (a b ,BLANK d)))
                        ((E (a b c ,BLANK)) (N (a b c ,BLANK)))
                        ((E (,BLANK ,BLANK c d)) (N (,BLANK ,BLANK c d)))
                        ((E (,BLANK b ,BLANK d)) (N (,BLANK b ,BLANK d)))
                        ((E (,BLANK b c ,BLANK)) (N (,BLANK b c ,BLANK)))
                        ((E (a ,BLANK ,BLANK d)) (N (a ,BLANK ,BLANK d)))
                        ((E (a ,BLANK c ,BLANK)) (N (a ,BLANK c ,BLANK)))
                        ((E (a b ,BLANK ,BLANK)) (N (a b ,BLANK ,BLANK)))
                        ((E (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
                        ((E (,BLANK b ,BLANK ,BLANK)) (N (,BLANK b ,BLANK ,BLANK)))
                        ((E (,BLANK ,BLANK c ,BLANK)) (N (,BLANK ,BLANK c ,BLANK)))
                        ((E (,BLANK ,BLANK ,BLANK d)) (N (,BLANK ,BLANK ,BLANK d))))
                      4
                      'Y))

(module+ test 
  (require rackunit rackunit/text-ui)
  
  (check-equal? (sm-states ADD) (list 'S 'A 'T 'U 'V 'H 'Q))
  (check-equal? (sm-sigma ADD) (list 'i))
  (check-equal? (sm-start ADD) 'S)
  (check-equal? (sm-finals ADD) '(H))
  (check-equal? (sm-rules ADD)
                (list
                 (list (list 'S (list '_ '_ '_)) (list 'A (list 'R 'R 'R)))
                 (list (list 'A (list 'i '_ '_)) (list 'A (list '_ 'i '_)))
                 (list (list 'A (list '_ 'i '_)) (list 'A (list 'R 'R '_)))
                 (list (list 'A (list '_ '_ '_)) (list 'T (list 'R '_ '_)))
                 (list (list 'T (list 'i '_ '_)) (list 'T (list '_ '_ 'i)))
                 (list (list 'T (list '_ '_ 'i)) (list 'T (list 'R '_ 'R)))
                 (list (list 'T (list '_ '_ '_)) (list 'Q (list 'L '_ '_)))
                 (list (list 'Q (list '_ '_ '_)) (list 'U (list 'L 'L 'L)))
                 (list (list 'U (list '_ 'i 'i)) (list 'U (list 'i 'i '_)))
                 (list (list 'U (list 'i 'i '_)) (list 'U (list 'L 'i 'L)))
                 (list (list 'U (list '_ 'i '_)) (list 'V (list '_ 'i '_)))
                 (list (list 'V (list '_ 'i '_)) (list 'V (list 'i '_ '_)))
                 (list (list 'V (list 'i '_ '_)) (list 'V (list 'L 'L '_)))
                 (list (list 'V (list '_ '_ '_)) (list 'H (list '_ '_ '_)))))
  (check-equal? (sm-numtapes ADD) 3)
  (check-equal? (sm-apply ADD `(,BLANK i i i ,BLANK i i))
                (list
                 'H
                 (list 0 (list '_ 'i 'i 'i 'i 'i '_ '_))
                 (list 0 (list '_ '_ '_ '_ '_))
                 (list 0 (list '_ '_ '_ '_))))
  (check-equal? (sm-showtransitions ADD `(,BLANK i i i ,BLANK i i))
                (list
                 (list 'S (list 0 (list '_ 'i 'i 'i '_ 'i 'i)) (list 0 (list '_)) (list 0 (list '_)))
                 (list 'A (list 1 (list '_ 'i 'i 'i '_ 'i 'i)) (list 1 (list '_ '_)) (list 1 (list '_ '_)))
                 (list 'A (list 1 (list '_ '_ 'i 'i '_ 'i 'i)) (list 1 (list '_ 'i)) (list 1 (list '_ '_)))
                 (list 'A (list 2 (list '_ '_ 'i 'i '_ 'i 'i)) (list 2 (list '_ 'i '_)) (list 1 (list '_ '_)))
                 (list 'A (list 2 (list '_ '_ '_ 'i '_ 'i 'i)) (list 2 (list '_ 'i 'i)) (list 1 (list '_ '_)))
                 (list 'A (list 3 (list '_ '_ '_ 'i '_ 'i 'i)) (list 3 (list '_ 'i 'i '_)) (list 1 (list '_ '_)))
                 (list 'A (list 3 (list '_ '_ '_ '_ '_ 'i 'i)) (list 3 (list '_ 'i 'i 'i)) (list 1 (list '_ '_)))
                 (list
                  'A
                  (list 4 (list '_ '_ '_ '_ '_ 'i 'i))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 1 (list '_ '_)))
                 (list
                  'T
                  (list 5 (list '_ '_ '_ '_ '_ 'i 'i))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 1 (list '_ '_)))
                 (list
                  'T
                  (list 5 (list '_ '_ '_ '_ '_ '_ 'i))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 1 (list '_ 'i)))
                 (list
                  'T
                  (list 6 (list '_ '_ '_ '_ '_ '_ 'i))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 2 (list '_ 'i '_)))
                 (list
                  'T
                  (list 6 (list '_ '_ '_ '_ '_ '_ '_))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 2 (list '_ 'i 'i)))
                 (list
                  'T
                  (list 7 (list '_ '_ '_ '_ '_ '_ '_ '_))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 3 (list '_ 'i 'i '_)))
                 (list
                  'Q
                  (list 6 (list '_ '_ '_ '_ '_ '_ '_ '_))
                  (list 4 (list '_ 'i 'i 'i '_))
                  (list 3 (list '_ 'i 'i '_)))
                 (list
                  'U
                  (list 5 (list '_ '_ '_ '_ '_ '_ '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 2 (list '_ 'i 'i '_)))
                 (list
                  'U
                  (list 5 (list '_ '_ '_ '_ '_ 'i '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 2 (list '_ 'i '_ '_)))
                 (list
                  'U
                  (list 4 (list '_ '_ '_ '_ '_ 'i '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 1 (list '_ 'i '_ '_)))
                 (list
                  'U
                  (list 4 (list '_ '_ '_ '_ 'i 'i '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 1 (list '_ '_ '_ '_)))
                 (list
                  'U
                  (list 3 (list '_ '_ '_ '_ 'i 'i '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 3 (list '_ '_ '_ '_ 'i 'i '_ '_))
                  (list 3 (list '_ 'i 'i 'i '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 3 (list '_ '_ '_ 'i 'i 'i '_ '_))
                  (list 3 (list '_ 'i 'i '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 2 (list '_ '_ '_ 'i 'i 'i '_ '_))
                  (list 2 (list '_ 'i 'i '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 2 (list '_ '_ 'i 'i 'i 'i '_ '_))
                  (list 2 (list '_ 'i '_ '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 1 (list '_ '_ 'i 'i 'i 'i '_ '_))
                  (list 1 (list '_ 'i '_ '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 1 (list '_ 'i 'i 'i 'i 'i '_ '_))
                  (list 1 (list '_ '_ '_ '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'V
                  (list 0 (list '_ 'i 'i 'i 'i 'i '_ '_))
                  (list 0 (list '_ '_ '_ '_ '_))
                  (list 0 (list '_ '_ '_ '_)))
                 (list
                  'H
                  (list 0 (list '_ 'i 'i 'i 'i 'i '_ '_))
                  (list 0 (list '_ '_ '_ '_ '_))
                  (list 0 (list '_ '_ '_ '_)))))

  (check-equal? (sm-states a^nb^nc^nd^n) (list 'S 'A 'Y 'N 'Q 'F 'B 'C 'D 'E))
  (check-equal? (sm-sigma a^nb^nc^nd^n) (list 'a 'b 'c 'd))
  (check-equal? (sm-start a^nb^nc^nd^n) 'S)
  (check-equal? (sm-finals a^nb^nc^nd^n) '(Y N))
  (check-equal? (sm-rules a^nb^nc^nd^n)
                (list
                 (list (list 'S (list '_ '_ '_ '_)) (list 'Q (list 'R 'R 'R 'R)))
                 (list (list 'Q (list '_ '_ '_ '_)) (list 'Y (list '_ '_ '_ '_)))
                 (list (list 'Q (list 'a '_ '_ '_)) (list 'A (list 'a '_ '_ '_)))
                 (list (list 'Q (list 'b '_ '_ '_)) (list 'N (list 'b '_ '_ '_)))
                 (list (list 'Q (list 'c '_ '_ '_)) (list 'N (list 'c '_ '_ '_)))
                 (list (list 'Q (list 'd '_ '_ '_)) (list 'N (list 'd '_ '_ '_)))
                 (list (list 'A (list 'a '_ '_ '_)) (list 'A (list 'R '_ '_ '_)))
                 (list (list 'A (list 'b '_ '_ '_)) (list 'B (list 'b '_ '_ '_)))
                 (list (list 'A (list '_ '_ '_ '_)) (list 'N (list '_ '_ '_ '_)))
                 (list (list 'A (list 'c '_ '_ '_)) (list 'N (list 'c '_ '_ '_)))
                 (list (list 'A (list 'd '_ '_ '_)) (list 'N (list 'd '_ '_ '_)))
                 (list (list 'B (list 'b '_ '_ '_)) (list 'B (list 'b 'b '_ '_)))
                 (list (list 'B (list '_ '_ '_ '_)) (list 'N (list '_ '_ '_ '_)))
                 (list (list 'B (list 'b 'b '_ '_)) (list 'B (list 'R 'R '_ '_)))
                 (list (list 'B (list 'c '_ '_ '_)) (list 'C (list 'c '_ '_ '_)))
                 (list (list 'B (list 'a '_ '_ '_)) (list 'N (list 'a '_ '_ '_)))
                 (list (list 'B (list 'd '_ '_ '_)) (list 'N (list 'd '_ '_ '_)))
                 (list (list 'C (list 'c '_ '_ '_)) (list 'C (list 'c '_ 'c '_)))
                 (list (list 'C (list 'c '_ 'c '_)) (list 'C (list 'R '_ 'R '_)))
                 (list (list 'C (list 'd '_ '_ '_)) (list 'D (list 'd '_ '_ '_)))
                 (list (list 'C (list '_ '_ '_ '_)) (list 'N (list '_ '_ '_ '_)))
                 (list (list 'C (list 'a '_ '_ '_)) (list 'N (list 'a '_ '_ '_)))
                 (list (list 'C (list 'b '_ '_ '_)) (list 'N (list 'b '_ '_ '_)))
                 (list (list 'D (list 'd '_ '_ '_)) (list 'D (list 'd '_ '_ 'd)))
                 (list (list 'D (list 'd '_ '_ 'd)) (list 'D (list 'R '_ '_ 'R)))
                 (list (list 'D (list '_ '_ '_ '_)) (list 'E (list 'L 'L 'L 'L)))
                 (list (list 'D (list 'a '_ '_ '_)) (list 'N (list 'a '_ '_ '_)))
                 (list (list 'D (list 'b '_ '_ '_)) (list 'N (list 'b '_ '_ '_)))
                 (list (list 'D (list 'c '_ '_ '_)) (list 'N (list 'c '_ '_ '_)))
                 (list (list 'E (list '_ '_ '_ '_)) (list 'Y (list '_ '_ '_ '_)))
                 (list (list 'E (list 'd 'b 'c 'd)) (list 'E (list 'L 'b 'c 'd)))
                 (list (list 'E (list 'c 'b 'c 'd)) (list 'E (list 'L 'b 'c 'd)))
                 (list (list 'E (list 'b 'b 'c 'd)) (list 'E (list 'L 'b 'c 'd)))
                 (list (list 'E (list 'a 'b 'c 'd)) (list 'E (list 'L 'L 'L 'L)))
                 (list (list 'E (list '_ 'b 'c 'd)) (list 'N (list '_ 'b 'c 'd)))
                 (list (list 'E (list 'a '_ 'c 'd)) (list 'N (list 'a '_ 'c 'd)))
                 (list (list 'E (list 'a 'b '_ 'd)) (list 'N (list 'a 'b '_ 'd)))
                 (list (list 'E (list 'a 'b 'c '_)) (list 'N (list 'a 'b 'c '_)))
                 (list (list 'E (list '_ '_ 'c 'd)) (list 'N (list '_ '_ 'c 'd)))
                 (list (list 'E (list '_ 'b '_ 'd)) (list 'N (list '_ 'b '_ 'd)))
                 (list (list 'E (list '_ 'b 'c '_)) (list 'N (list '_ 'b 'c '_)))
                 (list (list 'E (list 'a '_ '_ 'd)) (list 'N (list 'a '_ '_ 'd)))
                 (list (list 'E (list 'a '_ 'c '_)) (list 'N (list 'a '_ 'c '_)))
                 (list (list 'E (list 'a 'b '_ '_)) (list 'N (list 'a 'b '_ '_)))
                 (list (list 'E (list 'a '_ '_ '_)) (list 'N (list 'a '_ '_ '_)))
                 (list (list 'E (list '_ 'b '_ '_)) (list 'N (list '_ 'b '_ '_)))
                 (list (list 'E (list '_ '_ 'c '_)) (list 'N (list '_ '_ 'c '_)))
                 (list (list 'E (list '_ '_ '_ 'd)) (list 'N (list '_ '_ '_ 'd)))))
  (check-equal? (sm-numtapes a^nb^nc^nd^n) 4)
  (check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a a b b c c d d)) 'accept)
  (check-equal? (sm-apply a^nb^nc^nd^n `(,BLANK a a b b c d d)) 'reject)
  (check-equal? (sm-showtransitions a^nb^nc^nd^n `(,BLANK a a b b c c d d))
                (list
                 (list
                  'S
                  (list 0 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 0 (list '_))
                  (list 0 (list '_))
                  (list 0 (list '_)))
                 (list
                  'Q
                  (list 1 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'A
                  (list 1 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'A
                  (list 2 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'A
                  (list 3 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'B
                  (list 3 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'B
                  (list 3 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 1 (list '_ 'b))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'B
                  (list 4 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 2 (list '_ 'b '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'B
                  (list 4 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 2 (list '_ 'b 'b))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'B
                  (list 5 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'C
                  (list 5 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 1 (list '_ '_))
                  (list 1 (list '_ '_)))
                 (list
                  'C
                  (list 5 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 1 (list '_ 'c))
                  (list 1 (list '_ '_)))
                 (list
                  'C
                  (list 6 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c '_))
                  (list 1 (list '_ '_)))
                 (list
                  'C
                  (list 6 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c))
                  (list 1 (list '_ '_)))
                 (list
                  'C
                  (list 7 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 1 (list '_ '_)))
                 (list
                  'D
                  (list 7 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 1 (list '_ '_)))
                 (list
                  'D
                  (list 7 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 1 (list '_ 'd)))
                 (list
                  'D
                  (list 8 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd '_)))
                 (list
                  'D
                  (list 8 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd)))
                 (list
                  'D
                  (list 9 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 3 (list '_ 'b 'b '_))
                  (list 3 (list '_ 'c 'c '_))
                  (list 3 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 8 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 7 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 6 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 5 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 4 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 3 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 2 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 2 (list '_ 'b 'b '_))
                  (list 2 (list '_ 'c 'c '_))
                  (list 2 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 1 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 1 (list '_ 'b 'b '_))
                  (list 1 (list '_ 'c 'c '_))
                  (list 1 (list '_ 'd 'd '_)))
                 (list
                  'E
                  (list 0 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 0 (list '_ 'b 'b '_))
                  (list 0 (list '_ 'c 'c '_))
                  (list 0 (list '_ 'd 'd '_)))
                 (list
                  'Y
                  (list 0 (list '_ 'a 'a 'b 'b 'c 'c 'd 'd '_))
                  (list 0 (list '_ 'b 'b '_))
                  (list 0 (list '_ 'c 'c '_))
                  (list 0 (list '_ 'd 'd '_)))
                 'accept))
  (check-equal? (sm-sameresult? ADD ADD `(,BLANK i i i ,BLANK i i i)) #true)

  ;; Exception Checks Below:
  (check-exn exn:fail? (lambda () (sm-sameresult? ADD a^nb^nc^nd^n `(,BLANK a a b b c c d d))))
  (check-exn  exn:fail? (lambda () (sm-testequiv? ADD ADD 5)))
  (check-exn  exn:fail? (lambda () (sm-test ADD 5)))

  ) ;; end module+ test