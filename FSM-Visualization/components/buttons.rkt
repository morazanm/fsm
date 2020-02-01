#lang racket

#|
Created by Joshua Schappel on 12/19/19
  This file contains all the buttons for the visualization tool
|#

(require 2htdp/image "../structs/button.rkt" "../globals.rkt" "buttonFunctions.rkt"
         "./runProgram.rkt" "../genCode.rkt")

(provide
 BUTTON-LIST
 BUTTON-LIST-PDA
 BUTTON-LIST-TM)



#|
-----------------------
Button Declarations
-----------------------
|# 

(define BTN-ADD-STATE (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- CONTROL-BOX-H 25)) addState))
(define BTN-REMOVE-STATE (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- CONTROL-BOX-H 25)) removeState))

(define BTN-ADD-ALPHA (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25)) addAlpha))
(define BTN-REMOVE-ALPHA (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25)) rmvAlpha))
(define BTN-ADD-ALPHA-PDA (button 35 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 175) (- (* 2 CONTROL-BOX-H) 25)) addAlpha))
(define BTN-REMOVE-ALPHA-PDA (button 35 25 "Rmv" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 125) (- (* 2 CONTROL-BOX-H ) 25)) rmvAlpha))

(define BTN-ADD-GAMMA-PDA (button 35 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 75) (- (* 2 CONTROL-BOX-H) 25)) addGamma))
(define BTN-REMOVE-GAMMA-PDA (button 35 25 "Rmv" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 25) (- (* 2 CONTROL-BOX-H ) 25)) rmvGamma))


(define BTN-ADD-START (button 50 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71)) addStart))
(define BTN-REMOVE-START (button 50 25 "Replace" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25)) replaceStart))

(define BTN-ADD-END (button 50 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71)) addEnd))
(define BTN-REMOVE-END (button 50 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 18 #f #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25)) rmvEnd))

(define BTN-ADD-RULES (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25)) addRule))
(define BTN-REMOVE-RULES (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25)) removeRule))
(define BTN-ADD-RULES-PDA (button 70 25 "Add" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 20)) addRule))
(define BTN-REMOVE-RULES-PDA (button 70 25 "Remove" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 24 #f #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 20)) removeRule))


(define BTN-SCROLL-LEFT-RULES (button 30 BOTTOM "🠈" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 35 #f #f (posn 125 (- HEIGHT 37)) scrollbarLeft))
(define BTN-SCROLL-RIGHT-RULES (button 30 BOTTOM "🠊" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 35 #f #f (posn (- WIDTH 215) (- HEIGHT 37)) scrollbarRight))



(define BTN-RUN (button 95 30 "Run" "solid" (make-color 29 153 68) (make-color 29 153 68) 25 #f #f (posn 55 105) runProgram))
(define BTN-HELP (button 25 25 "?" "solid" (make-color 39 168 242) (make-color 39 168 242) 15 #t #f (posn 130 80) openHelp))

(define BTN-NEXT (button 95 30 "NEXT 🠊" "solid" (make-color 116 156 188) (make-color 116 156 188) 25 #f #f (posn 55 140) showNext))
(define BTN-PREV (button 95 30 "🠈 PREV" "solid" (make-color 116 156 188) (make-color 116 156 188) 25 #f #f (posn 55 175) showPrev))
(define BTN-GENCODE (button 95 50 "GEN CODE" "solid" (make-color 240 79 77) (make-color 240 79 77) 30 #f #f (posn 55 220) genCode))

;; pda stack scroll btn's
(define BTN-STACK-UP (button STACK-WIDTH 25 "🠉" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 33 #f #f (posn (- WIDTH 250) 73) stackScrollUp))
(define BTN-STACK-DOWN (button STACK-WIDTH 25 "🠋" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 33 #f #f (posn (- WIDTH 250) (- HEIGHT 87)) stackScrollDown))


(define BTN-SIGMA-ADD (button 40 25 "ADD" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 20 #f #f (posn 30 70) addSigma))
(define BTN-SIGMA-CLEAR (button 40 25 "CLEAR" "solid" CONTROLLER-BUTTON-COLOR CONTROLLER-BUTTON-COLOR 20 #f #f (posn 80 70) clearSigma))




;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES
                          BTN-GENCODE BTN-NEXT BTN-PREV
                          BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                          BTN-RUN BTN-SCROLL-LEFT-RULES
                          BTN-SCROLL-RIGHT-RULES BTN-HELP))

(define BUTTON-LIST-PDA (list BTN-ADD-STATE BTN-REMOVE-STATE
                              BTN-ADD-ALPHA-PDA BTN-REMOVE-ALPHA-PDA
                              BTN-ADD-GAMMA-PDA BTN-REMOVE-GAMMA-PDA
                              BTN-ADD-START BTN-REMOVE-START
                              BTN-ADD-END BTN-REMOVE-END
                              BTN-ADD-RULES-PDA BTN-REMOVE-RULES-PDA
                              BTN-GENCODE BTN-NEXT BTN-PREV
                              BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                              BTN-RUN BTN-SCROLL-LEFT-RULES
                              BTN-SCROLL-RIGHT-RULES BTN-HELP
                              BTN-STACK-UP BTN-STACK-DOWN
                              BTN-SCROLL-RIGHT-RULES BTN-HELP))

(define BUTTON-LIST-TM (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES-PDA BTN-REMOVE-RULES-PDA
                          BTN-GENCODE BTN-NEXT BTN-PREV
                          BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                          BTN-RUN BTN-SCROLL-LEFT-RULES
                          BTN-SCROLL-RIGHT-RULES BTN-HELP))
