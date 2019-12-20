#lang racket
#|
Created by Joshua Schappel on 12/19/19
  This file contaisn all the inputs that are rendered on the visualization tool
|#

(require "../structs/input.rkt" "../globals.rkt" "../structs/posn.rkt")


(provide INPUT-LIST INPUT-LIST-PDA)


#|
-----------------------
Textbox Declarations
-----------------------
|# 

(define IPF-STATE (textbox 150 25 INPUT-COLOR INPUT-COLOR "" 5 (posn (- WIDTH 100) (- CONTROL-BOX-H 70)) #f))
(define IPF-ALPHA (textbox 150 25 INPUT-COLOR INPUT-COLOR "" 1 (posn (- WIDTH 100) (- (* 2 CONTROL-BOX-H) 70)) #f))
(define IPF-START (textbox 75 25 INPUT-COLOR INPUT-COLOR "" 5 (posn (- WIDTH 150) (- (* 3 CONTROL-BOX-H) 50)) #f))
(define IPF-END (textbox 75 25 INPUT-COLOR INPUT-COLOR "" 5 (posn (- WIDTH 150) (- (* 4 CONTROL-BOX-H) 50)) #f))
(define IPF-RULE1 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE2 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 70)) #f))
(define IPF-RULE3 (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 70)) #f))


(define IPF-RULE1-PDA (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 80)) #f))
(define IPF-RULE2-PDA (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 80)) #f))
(define IPF-RULE3-PDA (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 80)) #f))
(define IPF-RULE4-PDA (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 75) (- (* 5 CONTROL-BOX-H) 52)) #f))
(define IPF-RULE5-PDA (textbox 40 25 INPUT-COLOR INPUT-COLOR "" 4 (posn (- WIDTH 125) (- (* 5 CONTROL-BOX-H) 52)) #f))


(define IPF-SIGMA (textbox 90 25 INPUT-COLOR INPUT-COLOR "" 8 (posn (/ (/ WIDTH 11) 2) 40) #f))

;;pda related inputs
(define IPF-ALPHA-PDA (textbox 50 25 INPUT-COLOR INPUT-COLOR "" 1 (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 70)) #f))
(define IPF-GAMMA-PDA (textbox 50 25 INPUT-COLOR INPUT-COLOR "" 1 (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H) 70)) #f))

;; INPUT-LIST: A list containing all input fields that are displayed on the scene.
(define INPUT-LIST (list IPF-STATE IPF-ALPHA IPF-START IPF-END IPF-RULE1 IPF-RULE2 IPF-RULE3 IPF-SIGMA))
(define INPUT-LIST-PDA (list IPF-STATE IPF-ALPHA-PDA IPF-START IPF-END IPF-RULE1-PDA IPF-RULE2-PDA IPF-RULE3-PDA IPF-SIGMA IPF-GAMMA-PDA IPF-RULE4-PDA IPF-RULE5-PDA))
