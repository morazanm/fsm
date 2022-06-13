;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mttm-tests) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(require fsm)

(require test-engine/racket-tests)

(define ADD (make-mttm '(S A T U V)
                       `(I)
                       'S
                       '(H)
                       `(((S (,BLANK ,BLANK ,BLANK)) (A (R R R)))
                         ((A (I ,BLANK ,BLANK)) (A (,BLANK I ,BLANK)))
                         ((A (,BLANK I ,BLANK)) (A (R R ,BLANK)))
                         ((A (,BLANK ,BLANK ,BLANK)) (T (R ,BLANK ,BLANK)))
                         ((T (I ,BLANK ,BLANK)) (T (,BLANK ,BLANK I)))
                         ((T (,BLANK ,BLANK I)) (T (R ,BLANK R)))
                         ((T (,BLANK ,BLANK ,BLANK)) (Q (L ,BLANK ,BLANK)))
                         ((Q (,BLANK ,BLANK ,BLANK)) (U (L L L)))
                         ((U (,BLANK I I)) (U (I I ,BLANK)))
                         ((U (I I ,BLANK)) (U (L I L)))
                         ((U (,BLANK I ,BLANK)) (V (,BLANK I ,BLANK)))
                         ((V (,BLANK I ,BLANK)) (V (I ,BLANK ,BLANK)))
                         ((V (I ,BLANK ,BLANK)) (V (L L ,BLANK)))
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
                      '(S A Y N)
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
                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
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

(check-expect (sm-getstates ADD) (list 'S 'A 'T 'U 'V))
(check-expect (sm-getalphabet ADD) (list 'I))
(check-expect (sm-getstart ADD) 'S)
(check-expect (sm-getfinals ADD) '(H))
(check-expect (sm-getrules ADD)
              (list
               (list (list 'S (list '_ '_ '_)) (list 'A (list 'R 'R 'R)))
               (list (list 'A (list 'I '_ '_)) (list 'A (list '_ 'I '_)))
               (list (list 'A (list '_ 'I '_)) (list 'A (list 'R 'R '_)))
               (list (list 'A (list '_ '_ '_)) (list 'T (list 'R '_ '_)))
               (list (list 'T (list 'I '_ '_)) (list 'T (list '_ '_ 'I)))
               (list (list 'T (list '_ '_ 'I)) (list 'T (list 'R '_ 'R)))
               (list (list 'T (list '_ '_ '_)) (list 'Q (list 'L '_ '_)))
               (list (list 'Q (list '_ '_ '_)) (list 'U (list 'L 'L 'L)))
               (list (list 'U (list '_ 'I 'I)) (list 'U (list 'I 'I '_)))
               (list (list 'U (list 'I 'I '_)) (list 'U (list 'L 'I 'L)))
               (list (list 'U (list '_ 'I '_)) (list 'V (list '_ 'I '_)))
               (list (list 'V (list '_ 'I '_)) (list 'V (list 'I '_ '_)))
               (list (list 'V (list 'I '_ '_)) (list 'V (list 'L 'L '_)))
               (list (list 'V (list '_ '_ '_)) (list 'H (list '_ '_ '_)))))
(check-expect (sm-getnumtapes ADD) 3)
