;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname testing-sm-rename-states) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))

(require "fsm-main.rkt")
(require test-engine/racket-tests)

(define tm-WriteI (make-tm '(S H) 
                           '(i j k) 
                           (list 
                            (list (list 'S 'i) (list 'H 'i)) 
                            (list (list 'S BLANK) (list 'H 'i))
                            (list (list 'S 'j) (list 'H 'i)) 
                            (list (list 'S 'k) (list 'H 'i)))
                           'S
                           '(H)))

(check-expect (last (sm-showtransitions tm-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 1))
              '(H 1 (i i i _ i i _)))
(check-expect (last (sm-showtransitions tm-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 0))
              '(H 0 (i _ i _ i i _)))                                        

(define tm-rename-sts-WriteI (sm-rename-states (sm-getstates tm-WriteI) tm-WriteI))

(check-expect (cdr (last (sm-showtransitions  tm-rename-sts-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 1)))
              '(1 (i i i _ i i _)))
(check-expect (cdr (last (sm-showtransitions tm-rename-sts-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 0)))
              '(0 (i _ i _ i i _)))
