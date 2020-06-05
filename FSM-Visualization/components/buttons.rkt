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
 BUTTON-LIST-TM
 BUTTON-LIST-LANG-REC
 BTN-DISPLAY)



#|
-----------------------
Button Declarations
-----------------------
|# 
(define BTN-ADD-STATE (make-button 70 25 (posn (- WIDTH 150) (- CONTROL-BOX-H 25))
                                   #:text "Add"
                                   #:color CONTROLLER-BUTTON-COLOR
                                   #:fntsize 24
                                   #:func addState))

(define BTN-REMOVE-STATE (make-button 70 25 (posn (- WIDTH 50) (- CONTROL-BOX-H 25))
                                      #:text "Remove"
                                      #:color CONTROLLER-BUTTON-COLOR
                                      #:fntsize 24
                                      #:func removeState))

(define BTN-ADD-ALPHA (make-button 70 25 (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25))
                                   #:text "Add"
                                   #:color CONTROLLER-BUTTON-COLOR
                                   #:fntsize 24
                                   #:func addAlpha))

(define BTN-REMOVE-ALPHA (make-button 70 25 (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25))
                                      #:text "Remove"
                                      #:color CONTROLLER-BUTTON-COLOR
                                      #:fntsize 24
                                      #:func rmvAlpha))

(define BTN-ADD-ALPHA-PDA (make-button 35 25 (posn (- WIDTH 175) (- (* 2 CONTROL-BOX-H) 25))
                                       #:text "Add"
                                       #:color CONTROLLER-BUTTON-COLOR
                                       #:func addAlpha))

(define BTN-REMOVE-ALPHA-PDA (make-button 35 25 (posn (- WIDTH 125) (- (* 2 CONTROL-BOX-H ) 25))
                                          #:text "Rmv"
                                          #:color CONTROLLER-BUTTON-COLOR
                                          #:func rmvAlpha))

(define BTN-ADD-GAMMA-PDA (make-button 35 25 (posn (- WIDTH 75) (- (* 2 CONTROL-BOX-H) 25))
                                       #:text "Add"
                                       #:color CONTROLLER-BUTTON-COLOR
                                       #:func addGamma))

(define BTN-REMOVE-GAMMA-PDA (make-button 35 25 (posn (- WIDTH 25) (- (* 2 CONTROL-BOX-H ) 25))
                                          #:text "Rmv"
                                          #:color CONTROLLER-BUTTON-COLOR
                                          #:func rmvGamma))

(define BTN-ADD-START (make-button 50 25 (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71))
                                   #:text "Add"
                                   #:color CONTROLLER-BUTTON-COLOR
                                   #:func addStart))

(define BTN-REMOVE-START (make-button 50 25 (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25))
                                      #:text "Replace"
                                      #:color CONTROLLER-BUTTON-COLOR
                                      #:func replaceStart))

(define BTN-ADD-START-LANG-REC (make-button 35 25 (posn (- WIDTH 175) (- (* 3 CONTROL-BOX-H) 25))
                                            #:text "Add"
                                            #:color CONTROLLER-BUTTON-COLOR
                                            #:func addStart))

(define BTN-REMOVE-START-LANG-REC (make-button 35 25 (posn (- WIDTH 125) (- (* 3 CONTROL-BOX-H) 25))
                                               #:text "Rmv"
                                               #:color CONTROLLER-BUTTON-COLOR
                                               #:func replaceStart))

(define BTN-ADD-END (make-button 50 25 (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71))
                                 #:text "Add"
                                 #:color CONTROLLER-BUTTON-COLOR
                                 #:func addEnd))

(define BTN-REMOVE-END (make-button 50 25 (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25))
                                    #:text "Remove"
                                    #:color CONTROLLER-BUTTON-COLOR
                                    #:func rmvEnd))

(define BTN-ADD-END-TM (make-button 35 25 (posn (- WIDTH 175) (- (* 4 CONTROL-BOX-H) 25))
                                    #:text "Add"
                                    #:color CONTROLLER-BUTTON-COLOR
                                    #:func addEnd))

(define BTN-REMOVE-END-TM (make-button 35 25 (posn (- WIDTH 125) (- (* 4 CONTROL-BOX-H) 25))
                                       #:text "Rmv"
                                       #:color CONTROLLER-BUTTON-COLOR
                                       #:func rmvEnd))

(define BTN-SET-END-LANG-REC (make-button 50 25 (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25))
                                          #:text "Set"
                                          #:color CONTROLLER-BUTTON-COLOR
                                          #:func setAcceptState))

(define BTN-TAPE-INPUT-TM (make-button 50 25 (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25))
                                       #:text "Set"
                                       #:color CONTROLLER-BUTTON-COLOR
                                       #:func setTapePosn))

(define BTN-ADD-RULES (make-button 70 25 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25))
                                   #:text "Add"
                                   #:color CONTROLLER-BUTTON-COLOR
                                   #:fntsize 24
                                   #:func addRule))

(define BTN-REMOVE-RULES (make-button 70 25 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25))
                                      #:text "Remove"
                                      #:color CONTROLLER-BUTTON-COLOR
                                      #:fntsize 24
                                      #:func removeRule))

(define BTN-ADD-RULES-PDA (make-button 70 25 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 20))
                                       #:text "Add"
                                       #:color CONTROLLER-BUTTON-COLOR
                                       #:fntsize 24
                                       #:func addRule))

(define BTN-REMOVE-RULES-PDA (make-button 70 25 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 20))
                                          #:text "Remove"
                                          #:color CONTROLLER-BUTTON-COLOR
                                          #:fntsize 24
                                          #:func removeRule))

(define BTN-TAPE-SCROLL-LEFT (make-button 30 TOP (posn 125 30)
                                          #:text "🠈"
                                          #:color CONTROLLER-BUTTON-COLOR
                                          #:fntsize 35
                                          #:func tapeScrollLeft))

(define BTN-TAPE-SCROLL-RIGHT (make-button 30 TOP (posn (- WIDTH 215) 30)
                                           #:text "🠊"
                                           #:color CONTROLLER-BUTTON-COLOR
                                           #:fntsize 35
                                           #:func tapeScrollRight))

(define BTN-SCROLL-LEFT-RULES (make-button 30 BOTTOM (posn 125 (- HEIGHT 37))
                                           #:text "🠈"
                                           #:color CONTROLLER-BUTTON-COLOR
                                           #:fntsize 35
                                           #:func scrollbarLeft))

(define BTN-SCROLL-RIGHT-RULES (make-button 30 BOTTOM (posn (- WIDTH 215) (- HEIGHT 37))
                                            #:text "🠊"
                                            #:color CONTROLLER-BUTTON-COLOR
                                            #:fntsize 35
                                            #:func scrollbarRight))

(define BTN-RUN (make-button 95 30 (posn 55 105)
                             #:text "Run"
                             #:color (make-color 29 153 68)
                             #:fntsize 25
                             #:func runProgram))

(define BTN-HELP (make-button 25 25 (posn 130 80)
                              #:text "?"
                              #:fntsize 12
                              #:color (make-color 39 168 242)
                              #:round? #t
                              #:func openHelp))

(define BTN-COLOR-BLIND (make-button 25 25 (posn 130 120)
                                     #:text "CB"
                                     #:fntsize 12
                                     #:color (make-color 252 186 3)
                                     #:round? #t
                                     #:func toogleColorBlindMode))

(define BTN-DISPLAY (make-button 30 30 (posn 130 160)
                                 #:text "DGr"
                                 #:color (make-color 61 65 71)
                                 #:fntsize 12
                                 #:round? #t
                                 #:func toggle-display))

(define BTN-NEXT (make-button 40 30 (posn 80 140)
                              #:text "🠊"
                              #:color (make-color 116 156 188)
                              #:fntsize 40
                              #:func showNext))

(define BTN-PREV (make-button 40 30 (posn 30 140)
                              #:text "🠈"
                              #:color (make-color 116 156 188)
                              #:fntsize 40
                              #:func showPrev))

(define BTN-GENCODE (make-button 95 50 (posn 55 190)
                                 #:text "GEN CODE"
                                 #:color (make-color 240 79 77)
                                 #:fntsize 30
                                 #:func genCode))

;; pda stack scroll btn's
(define BTN-STACK-UP (make-button STACK-WIDTH 25 (posn (- WIDTH 250) 73)
                                  #:text "🠉"
                                  #:color STACK-SCROLL-BUTTON-COLOR
                                  #:fntsize 33
                                  #:func stackScrollUp))

(define BTN-STACK-DOWN (make-button STACK-WIDTH 25 (posn (- WIDTH 250) (- HEIGHT 87))
                                    #:text "🠋"
                                    #:color STACK-SCROLL-BUTTON-COLOR
                                    #:fntsize 33
                                    #:func stackScrollDown))

(define BTN-SIGMA-ADD (make-button 40 25 (posn 30 70)
                                   #:text "ADD"
                                   #:color CONTROLLER-BUTTON-COLOR
                                   #:fntsize 20
                                   #:func addSigma))

(define BTN-SIGMA-CLEAR (make-button 40 25 (posn 80 70)
                                     #:text "CLEAR"
                                     #:color CONTROLLER-BUTTON-COLOR
                                     #:fntsize 20
                                     #:func clearSigma))


;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES
                          BTN-GENCODE BTN-NEXT BTN-PREV
                          BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                          BTN-RUN BTN-SCROLL-LEFT-RULES
                          BTN-SCROLL-RIGHT-RULES BTN-HELP
                          BTN-TAPE-SCROLL-LEFT BTN-TAPE-SCROLL-RIGHT
                          BTN-COLOR-BLIND))

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
                              BTN-SCROLL-RIGHT-RULES BTN-HELP
                              BTN-TAPE-SCROLL-LEFT BTN-TAPE-SCROLL-RIGHT
                              BTN-COLOR-BLIND))

(define BUTTON-LIST-TM (list BTN-ADD-STATE BTN-REMOVE-STATE
                             BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                             BTN-ADD-START BTN-REMOVE-START
                             BTN-ADD-END-TM BTN-REMOVE-END-TM
                             BTN-ADD-RULES-PDA BTN-REMOVE-RULES-PDA
                             BTN-GENCODE BTN-NEXT BTN-PREV
                             BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                             BTN-RUN BTN-SCROLL-LEFT-RULES
                             BTN-SCROLL-RIGHT-RULES BTN-HELP
                             BTN-TAPE-SCROLL-LEFT BTN-TAPE-SCROLL-RIGHT
                             BTN-COLOR-BLIND BTN-TAPE-INPUT-TM))

(define BUTTON-LIST-LANG-REC (list BTN-ADD-STATE BTN-REMOVE-STATE
                                   BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                                   BTN-ADD-START-LANG-REC BTN-REMOVE-START-LANG-REC
                                   BTN-ADD-END-TM BTN-REMOVE-END-TM
                                   BTN-ADD-RULES-PDA BTN-REMOVE-RULES-PDA
                                   BTN-GENCODE BTN-NEXT BTN-PREV
                                   BTN-SIGMA-ADD BTN-SIGMA-CLEAR
                                   BTN-RUN BTN-SCROLL-LEFT-RULES
                                   BTN-SCROLL-RIGHT-RULES BTN-HELP
                                   BTN-TAPE-SCROLL-LEFT BTN-TAPE-SCROLL-RIGHT
                                   BTN-COLOR-BLIND BTN-TAPE-INPUT-TM
                                   BTN-SET-END-LANG-REC))
