#lang racket
#|
Created by Joshua Schappel on 12/19/19
   This file contains global variables for the visualization tool and its components
|#

(require 2htdp/image)
(provide (all-defined-out))

;; -- VERSION --
(define VERSION "VERSION 1.6")

;; Determines if the graph representation should be displayed
;; vaild modes are:
;; - graph
;; - control
;; - tape (mttm only!!)
(define VIEW-MODE 'control)


;; -- DIMENTIONS --
(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
(define BOTTOM(/ HEIGHT 8)) ;; The height of the rule display box
(define STACK-WIDTH 100) ;; The width of the control stack image for pdas
(define TOP (/ HEIGHT 10)) ;; The height of the top input display box
;; img scaling below
(define h 450)
(define w 800)
(define h-pda 300)
(define w-pda 600)
(define SMALL-IMG-SCALE 1.3)

;; -- GUI COLORS --
(define COLOR-BLIND-MODE false)
(define START-STATE-COLOR (make-color 6 142 60)) ;; Color of circle that surrounds start state
(define END-STATE-COLOR (make-color 219 9 9)) ;; Color of circle that surrounds an end state
(define MSG-ERROR (make-color 255 0 0)) ;; Color of an error message
(define MSG-SUCCESS (make-color 65 122 67)) ;; Color if a success message
(define MSG-CAUTION (make-color 252 156 10)) ;; Color of a caution message
(define OUTLINE-COLOR "blue") ;; The color of the box outlineing
(define TAPE-HIGHLIGHT-COLOR "red") ;; The color of the failed inv.
(define TAPE-HIGHLIGHT-COLOR-CB (make-color 123 50 148)) ;; Color for failed invariant for color blind mode
(define ARROW-RULE-COLOR "red") ;; the color of the rule rendered above the arrow
(define DEFAULT-ARROW-COLOR "black") ;; the color of the arrow without invariants

;; -- INV-COLORS --
(define TRUE-INV (make-color 0 171 3)) ;; Color for passed invariant (green)
(define TRUE-INV-HEX "#00ab037F")
(define TRUE-INV-CB (make-color 1 133 113)) ;; Color for passed invarant for color blind mode
(define TRUE-INV-CB-HEX "#0185717F")
(define CAUTION-INV (make-color 255 234 3)) ;; Color for a invalid invariant return 
(define CAUTION-INV-HEX "#ffea037F")


(define FALSE-INV (make-color 245 35 20)) ;; Color for failed invariant (red)
(define FALSE-INV-HEX "#f523147F")
(define FALSE-INV-CB (make-color 123 50 148)) ;; Color for failed invariant for color blind mode
(define FALSE-INV-CB-HEX "#7b32947F")


;; -- BUTTONS COLORS --
(define CONTROLLER-BUTTON-COLOR (make-color 33 93 222)) ;; Color of button and center dot
(define CONTROLLER-BUTTON-COLOR-HEX "#215dde")
(define STACK-SCROLL-BUTTON-COLOR (make-color 135 204 222)) ;; Color of the stack buttons for pda's
(define DEFAULT-BTN-COLOR (make-color 100 200 100)) ;; The default color for a button
;; THIS FUNCTION IS JUST A PLACEHOLDER
;;  This function is Just a placeholder for biulding out the GUI
(define NULL-FUNCTION (lambda (w) w))

;; -- INPUTS --
(define INPUT-COLOR (make-color 141 144 145)) ;; The color of an input field
(define DEFAULT-IMP-COLOR (make-color 141 144 145))
;; -- CONTROL STACK --
(define R 175)
(define inner-R (- R 50))
(define CENTER-CIRCLE (circle 5 "solid" CONTROLLER-BUTTON-COLOR))


;; -- OTHER --
(define PLACEHOLDER '$k$) ;; default return value for a inv function. 
(define TRUE-FUNCTION (lambda (v) PLACEHOLDER)) ;; The default function for a state variable
(define PDA-TRUE-FUNCTION (lambda (v c) PLACEHOLDER)) ;; The default function for a state variable
(define TM-TRUE-FUNCTION (lambda (v c) PLACEHOLDER))  ;; The default function for a state variable
(define MTTM-TRUE-FUNCTION (lambda (tapes) PLACEHOLDER)) ;; The default function for a state variable
(define MACHINE-TYPE null) ;; The type of machine (pda, ndfa, ..)
(define TM-ORIGIONAL-TAPE '()) ;; set-tm-og-tape
(define TM-ORIGIONAL-TAPE-POSN 0) ;; the initial tape position set by the user. Defualts to 0



;; -- MUTATORS --
(define CURRENT-RULE '(null null null)) ;; The current rule that the machine is following
(define CURRENT-STATE null) ;; The current state that the machine is in


;; -- Scrollbars --
(define MTTM-TAPE-INDEX 0) ;; the start of the current tapes to be displayed
(define TAPE-INDEX-BOTTOM -1) ;; The current tape input that is being used
(define INIT-INDEX-BOTTOM 0) ;; The initail index of the scrollbar
(define STACK-INDEX 0) ;; The index of the stack scroll bar. The index is the first item to be rendered
(define TAPE-INDEX 0) ;; The index of the input scroll bar. The index is the first item to be rendered (works for mmtm's as well)
(define TAPE-RENDER-LIMIT 26) ;; The maximum amount of tape input that can be rendered at a time


;; -- RULE SCROLL BAR --
(define BOX-HEIGHT 75) ;; The hight of the box around a rule
(define BOX-PADDING 25) ;; The padding around the box (The left and right padding is hald the BOX-PADDING)
(define FONT-SIZE 20) ;; The font size of a rule
(define HIGHTLIGHT-RULE CONTROLLER-BUTTON-COLOR) ;; The color of a hightlighted rule
(define HIGHLIGHT-RULE-HEX CONTROLLER-BUTTON-COLOR-HEX) ;; The color of a hightlighted rule
(define DEFAULT-RULE "black") ;; Default color of a rule
(define RULE-BOX-COLOR "gray") ;; The color of the box around a rule


;; -- INPUT FACTORY --
(define DFA-NDFA_NUMBER 8) ;; The number of dfa's/ndfa's to render in the rule box
(define PDA_NUMBER 4) ;; The number of pda's to render in the rule box
(define TM_NUMBER 5) ;; The number of tm's to render in the rule box

;; -- PDA STACK --
(define STACK-LIST '()) ;; The stack list for a pda
(define STACK-LIMIT 14) ;; The number of elements from the stack that is allowed rendered on the screen at a time

;; Pops n elements off the stack
(define (pop-stack num)
  (define (new-stack-index)
    (define cur-len (length (drop-right STACK-LIST STACK-INDEX)))
    (if (< cur-len STACK-LIMIT)
        (let ((new-posn (- STACK-INDEX (- STACK-LIMIT cur-len))))
          (if (< new-posn 0) 0 new-posn))
        STACK-INDEX))
  (cond
    [(zero? num)
     ;; Once all pops have been finished we need to move the stack index so it will
     ;; always show the STACK-LIMIT number of elements
     (begin
       (when (> STACK-INDEX 0)
         (set! STACK-INDEX (new-stack-index)))
       '())]
    [(empty? STACK-LIST) (error "There are not any more elements to pop")]
    [else
     (begin
       (set! STACK-LIST (cdr STACK-LIST))
       (pop-stack (sub1 num)))]))

;; Pushes elements onto the stack and returns true when complete
(define (push-stack aList)
  (cond
    [(empty? aList)#t]
    [else
     (begin
       (set! STACK-LIST (cons (car aList) STACK-LIST))
       (push-stack (cdr aList)))]))

;; Resets the stack to be empty
(define (reset-stack)
  (set! STACK-LIST '()))


;; -- SETTERS --
(define (set-tape-render-limit value)
  (set! TAPE-RENDER-LIMIT value))

(define (set-tape-index-bottom value)
  (set! TAPE-INDEX-BOTTOM value))

(define (set-tm-og-tape tape)
  (set! TM-ORIGIONAL-TAPE tape))

(define (set-tm-og-tape-posn posn)
  (set! TM-ORIGIONAL-TAPE-POSN posn))

(define (set-init-index-bottom value)
  (set! INIT-INDEX-BOTTOM value))

(define (set-machine-type type)
  (set! MACHINE-TYPE type))

(define (set-tape-view-scroll-bar index)
  (set! MTTM-TAPE-INDEX index))

(define (set-stack-index num)
  (set! STACK-INDEX num))

(define (set-tape-index value)
  (set! TAPE-INDEX value))

(define (set-view-mode mode)
  (if (or (eq? mode 'control)
          (eq? mode 'graph)
          (eq? mode 'tape))
      (set! VIEW-MODE mode)
      (error (format "Internal Viz tool error. Invalid mode supplief ~s"
                     (symbol->string mode)))))

(define (toggle-color-blind-mode)
  (set! COLOR-BLIND-MODE (not COLOR-BLIND-MODE)))

;; Helper function to convert a value to a string
(define (stringify-value input) (if (number? input) (number->string input) (symbol->string input)))
