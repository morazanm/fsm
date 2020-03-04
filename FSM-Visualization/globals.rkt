#lang racket
#|
Created by Joshua Schappel on 12/19/19
   This file contains global variables for the visualization tool and its components
|#

(require 2htdp/image)
(provide (all-defined-out))

;; -- VERSION --
(define VERSION "BETA 2.0")


;; -- DIMENTIONS --
(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
(define BOTTOM(/ HEIGHT 8)) ;; The height of the rule display box
(define STACK-WIDTH 100) ;; The width of the control stack image for pdas
(define TOP (/ HEIGHT 10)) ;; The height of the top input display box


;; -- GUI COLORS --
(define COLOR-BLIND-MODE false)
(define START-STATE-COLOR (make-color 6 142 60)) ;; Color of circle that surrounds start state
(define END-STATE-COLOR (make-color 219 9 9)) ;; Color of circle that surrounds an end state
(define MSG-ERROR (make-color 255 0 0)) ;; Color of an error message
(define MSG-SUCCESS (make-color 65 122 67)) ;; Color if a success message
(define MSG-CAUTION (make-color 252 156 10)) ;; Color of a caution message


;; -- OTHER --
(define TRUE-FUNCTION (lambda (v) '())) ;; The default function for a state variable
(define PDA-TRUE-FUNCTION (lambda (v c) '())) ;; The default function for a state variable
(define TM-TRUE-FUNCTION (lambda (v c) '()))  ;; The default function for a state variable
(define MACHINE-TYPE null) ;; The type of machine (pda, ndfa, ..)


;; -- MUTATORS --
(define CURRENT-RULE '(null null null)) ;; The current rule that the machine is following
(define CURRENT-STATE null) ;; The current state that the machine is in

;; -- Scrollbars --
(define TAPE-INDEX-BOTTOM -1) ;; The current tape input that is being used
(define INIT-INDEX-BOTTOM 0) ;; The initail index of the scrollbar
(define STACK-INDEX 0) ;; The index of the stack scroll bar. The index is the first item to be rendered
(define TAPE-INDEX 0) ;; The index of the input scroll bar. The index is the first item to be rendered
(define TAPE-RENDER-LIMIT 26) ;; The maximum amount of tape input that can be rendered at a time

;; -- INPUTS --
(define INPUT-COLOR (make-color 186 190 191)) ;; The color of an input field


;; -- BUTTONS --
(define CONTROLLER-BUTTON-COLOR (make-color 33 93 222)) ;; Color of button and center dot
(define STACK-SCROLL-BUTTON-COLOR (make-color 135 204 222)) ;; Color of the stack buttons for pda's

;; -- INPUT FACTORY --
(define DFA-NDFA_NUMBER 8) ;; The number of dfa's/ndfa's to render in the rule box
(define PDA_NUMBER 4) ;; The number of pda's to render in the rule box
(define TM_NUMBER 5) ;; The number of tm's to render in the rule box

;; -- PDA STACK --
(define STACK-LIST '()) ;; The stack list for a pda
(define STACK-LIMIT 14) ;; The number of elements from the stack that is allowed rendered on the screen at a time

;; Pops n elements off the stack
(define (pop-stack num)
  (cond
    [(zero? num) '()]
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


;; -- SETTERS --
(define (set-tape-index-bottom value)
  (set! TAPE-INDEX-BOTTOM value))

(define (set-init-index-bottom value)
  (set! INIT-INDEX-BOTTOM value))

(define (set-machine-type type)
  (set! MACHINE-TYPE type))

(define (set-stack-index num)
  (set! STACK-INDEX num))

(define (set-tape-index value)
  (set! TAPE-INDEX value))

(define (toggle-color-blind-mode)
  (set! COLOR-BLIND-MODE (not COLOR-BLIND-MODE)))
