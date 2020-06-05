#lang racket
#|
Created by Joshua Schappel on 12/19/19
  This file contaisn all the inputs that are rendered on the visualization tool
|#

(require "../structs/input.rkt" "../globals.rkt" "../structs/posn.rkt" "buttonFunctions.rkt" "../genCode.rkt")


(provide
 INPUT-LIST
 INPUT-LIST-PDA
 INPUT-LIST-TM
 INPUT-LIST-LANG-REC)


#|
-----------------------
Textbox Declarations
-----------------------
|# 
(define IPF-STATE (make-textbox 150 25 (posn (- WIDTH 100) (- CONTROL-BOX-H 70))
                                #:limit 5
                                #:func addState))

(define IPF-ALPHA (make-textbox 150 25 (posn (- WIDTH 100) (- (* 2 CONTROL-BOX-H) 70))
                                #:limit 3
                                #:func addAlpha))

(define IPF-START (make-textbox 75 27 (posn (- WIDTH 150) (- (* 3 CONTROL-BOX-H) 50))
                                #:limit 5
                                #:func addStart))

(define IPF-START-LANG-REC (make-textbox 75 25 (posn (- WIDTH 150) (- (* 3 CONTROL-BOX-H) 75))
                                         #:limit 5
                                         #:func addStart))

(define IPF-END (make-textbox 75 25 (posn (- WIDTH 150) (- (* 4 CONTROL-BOX-H) 50))
                              #:limit 5
                              #:func addEnd))

(define IPF-END-TM (make-textbox 75 25 (posn (- WIDTH 150) (- (* 4 CONTROL-BOX-H) 75))
                                 #:limit 5
                                 #:func addEnd))

(define IPF-RULE1 (make-textbox 40 25 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 70))
                                #:limit 4
                                #:func NULL-FUNCTION))

(define IPF-RULE2 (make-textbox 40 25 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 70))
                                #:limit 4
                                #:func NULL-FUNCTION))

(define IPF-RULE3 (make-textbox 40 25 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 70))
                                #:limit 4
                                #:func NULL-FUNCTION))

(define IPF-RULE1-PDA (make-textbox 40 25 (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 80))
                                    #:limit 4
                                    #:func NULL-FUNCTION))

(define IPF-RULE2-PDA (make-textbox 40 25 (posn (- WIDTH 100) (- (* 5 CONTROL-BOX-H) 80))
                                    #:limit 4
                                    #:func NULL-FUNCTION))

(define IPF-RULE3-PDA (make-textbox 40 25 (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 80))
                                    #:limit 4
                                    #:func NULL-FUNCTION))

(define IPF-RULE4-PDA (make-textbox 40 25 (posn (- WIDTH 75) (- (* 5 CONTROL-BOX-H) 52))
                                    #:limit 4
                                    #:func NULL-FUNCTION))

(define IPF-RULE5-PDA (make-textbox 40 25 (posn (- WIDTH 125) (- (* 5 CONTROL-BOX-H) 52))
                                    #:limit 4
                                    #:func NULL-FUNCTION))

(define IPF-RULE1-TM (make-textbox 50 25 (posn (- WIDTH 135) (- (* 5 CONTROL-BOX-H) 80))
                                   #:limit 5
                                   #:func NULL-FUNCTION))

(define IPF-RULE2-TM (make-textbox 50 25 (posn (- WIDTH 65) (- (* 5 CONTROL-BOX-H) 80))
                                   #:limit 5
                                   #:func NULL-FUNCTION))

(define IPF-RULE3-TM (make-textbox 50 25 (posn (- WIDTH 135) (- (* 5 CONTROL-BOX-H) 52))
                                   #:limit 5
                                   #:func NULL-FUNCTION))

(define IPF-RULE4-TM (make-textbox 50 25 (posn (- WIDTH 65) (- (* 5 CONTROL-BOX-H) 52))
                                   #:limit 5
                                   #:func NULL-FUNCTION))

(define IPF-SIGMA (make-textbox 100 25 (posn (/ (/ WIDTH 11) 2) 40)
                          #:color INPUT-COLOR
                          #:limit 14
                          #:func addSigma))


(define GEN-CODE (make-textbox 100 25 (posn (/ (/ WIDTH 11) 2) 235)
                          #:color INPUT-COLOR
                          #:limit 14
                          #:func genCode))



;;pda related inputs
(define IPF-ALPHA-PDA (make-textbox 50 25 (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 70))
                                    #:limit 3
                                    #:func addAlpha))

(define IPF-GAMMA-PDA (make-textbox 50 25 (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H) 70))
                                    #:limit 3
                                    #:func addGamma))


;; Other
(define IPF-TAPE-INDEX (make-textbox 75 25 (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 60))
                                     #:limit 5
                                     #:func setTapePosn))

(define IPF-USER-DEFINED-LANG-REC (make-textbox 75 25 (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 60))
                                                #:limit 5
                                                #:func setAcceptState))


;; INPUT-LIST: A list containing all input fields that are displayed on the scene.
(define INPUT-LIST (list IPF-STATE IPF-ALPHA
                         IPF-START IPF-END
                         IPF-RULE1 IPF-RULE2
                         IPF-RULE3 IPF-SIGMA GEN-CODE))

(define INPUT-LIST-PDA (list IPF-STATE IPF-ALPHA-PDA
                             IPF-START IPF-END
                             IPF-RULE1-PDA IPF-RULE2-PDA
                             IPF-RULE3-PDA IPF-SIGMA
                             IPF-GAMMA-PDA IPF-RULE4-PDA
                             IPF-RULE5-PDA GEN-CODE))


(define INPUT-LIST-TM (list IPF-STATE IPF-ALPHA
                            IPF-START IPF-END-TM
                            IPF-RULE1-TM IPF-RULE2-TM
                            IPF-RULE3-TM IPF-SIGMA
                            IPF-RULE4-TM IPF-TAPE-INDEX
                            GEN-CODE))


(define INPUT-LIST-LANG-REC (list IPF-STATE IPF-ALPHA
                                  IPF-START-LANG-REC IPF-END-TM
                                  IPF-RULE1-TM IPF-RULE2-TM
                                  IPF-RULE3-TM IPF-SIGMA
                                  IPF-RULE4-TM IPF-TAPE-INDEX
                                  IPF-USER-DEFINED-LANG-REC
                                  GEN-CODE))