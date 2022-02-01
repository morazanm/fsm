#lang racket

#|
This file was created by Joshua Schappel on 12/19/19
  This file contains the world struct for the visualization tool
|#

(require "msgWindow.rkt" "posn.rkt" "../globals.rkt")

(provide
 (struct-out world)
 initialize-world
 create-new-world-input
 create-new-world-input-empty
 create-new-world-button
 redraw-world
 redraw-world-with-msg)


;; world: The world for the GUI
;; - machine: A machine structure for the world
;; - tape-position: The current position on the tape
;; - cur-rule: the current rule that is in use
;; - cur-state: The current state that the machine is in
;; - button-list: A list containing all buttons to be rendered on the GUI
;; - input-list: A list containing all the input-fields to be rendered on the GUI
;; - processed-config-list: All the transitions that have already been processed by the machine
;; - unprocessed-config-list: All the transitions that still have to be processed by the machine
;; - error msg: A msgWindow structure that will be rendered on the screen if not null.
;; - scroll-bar-index: An integer that represents the first position in the rule list to be rendered on the screen.
(struct world (fsm-machine
               tape-position
               cur-rule
               cur-state
               button-list
               input-list
               processed-config-list
               unporcessed-config-list
               error-msg
               scroll-bar-index) #:transparent)



#|
---------------------------
WORLD INITIALIZATION FUNCTIONS
---------------------------
|#

;; initialize-world: fsm-machine string list-of-buttons list-of-inputs -> world
;; Purpose: returns the proper world structure based on the given inputs.
(define (initialize-world machine window-msg btn-list input-list)
  (world
   machine
   0
   CURRENT-RULE
   CURRENT-STATE
   btn-list
   input-list
   '()
   '()
   window-msg
   INIT-INDEX-BOTTOM))

     




#|
---------------------------
WORLD DRAWING FUNCTIONS
---------------------------
|# 

;; create-new-world-input: world list-of-input-fields -> world
;; Purpose: Creates a new world to handle the list-of-input-fields changes
(define (create-new-world-input a-world loi)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         loi (world-processed-config-list a-world)(world-unporcessed-config-list a-world) (world-error-msg a-world) (world-scroll-bar-index a-world)))

;; create-new-world-input: world list-of-input-fields machine (optional) -> world
;; Purpose: Creates a new world to handle the list-of-input-fields changes AND sets the processed and unprocesseed lists to empty
(define (create-new-world-input-empty a-world loi . args)
  (cond
    [(empty? args)
     (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) null (world-button-list a-world)
            loi '()'() (world-error-msg a-world) (world-scroll-bar-index a-world))]
    [else
     (world (car args) (world-tape-position a-world) (world-cur-rule a-world) null (world-button-list a-world)
            loi '()'() (world-error-msg a-world) (world-scroll-bar-index a-world))]))

;; create-new-world-button: world list-of-button-fields -> world
;; Purpose: Creates a new world to handle the list-of-button-fields changes
(define (create-new-world-button a-world lob)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) lob
         (world-input-list a-world) (world-processed-config-list a-world) (world-unporcessed-config-list a-world) (world-error-msg a-world)
         (world-scroll-bar-index a-world)))

;; redraw-world: world -> world
;; redraws the same world as before
(define (redraw-world a-world)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         (world-input-list a-world) (world-processed-config-list a-world)(world-unporcessed-config-list a-world) (world-error-msg a-world)
         (world-scroll-bar-index a-world)))

;; redraw-world-with-msg: world string string color -> world
;; Purpose: redraws the same world with a message
(define (redraw-world-with-msg a-world msg-body msg-header msg-color)
  (world (world-fsm-machine a-world) (world-tape-position a-world) (world-cur-rule a-world) (world-cur-state a-world) (world-button-list a-world)
         (world-input-list a-world) (world-processed-config-list a-world)(world-unporcessed-config-list a-world)
         (msgWindow msg-body msg-header (posn (/ WIDTH 2) (/ HEIGHT 2)) msg-color) (world-scroll-bar-index a-world)))