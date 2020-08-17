#lang racket
(require 2htdp/image 2htdp/universe "posn.rkt" "../globals.rkt")

;; ------- button.rkt -------
;; This file contains the functionality and structure for buttons


;; Export necessary functions/structures
(provide
 (struct-out button)
 (struct-out posn)
 make-button
 draw-button
 run-function
 button-pressed?
 set-inactive-button
 set-active-button)

(define TINT-FACTOR .5) ;; The number that determins the level of tint an active button will have
(define TEXT-COLOR (make-color 255 255 255)) ;; Color of button text


;; button: A structurre that represents a button
;; - width: integer representing the width of the button
;; - height: integer representing the height of the button
;; - text: String represting the text to go in the button
;; - type: String representing the type (solid, outline...)
;; - color: color-struct that represents the rgb of a color
;; - orColor: color-struct that represents the origional color of the button
;; - fontSize: Natural Number that represents the font size
;; - rounded?: Boolean representing the shape of the button. #t if rounded.
;; - active: boolean, ALWAYS SET TO FALSE.
;; - location: posn that represents the location for the button
;; - onClick: A function to be run if the button is pressed
(struct button (width height text type color orColor fontSize rounded? active location onClick))


(define (make-button width height posn #:text[text ""] #:color[color DEFAULT-BTN-COLOR]
                     #:fntsize[size 18] #:round?[round #f] #:func[func NULL-FUNCTION] #:style[style "solid"])
  (button width height text style color color size round #f posn func))

;; draw-button: button posn scene -> scene
;; Purpose: Draws a given button onto the scene
(define (draw-button btn scn)
  (place-image (create-button btn) (posn-x (button-location btn)) (posn-y (button-location btn)) scn))

;; create-button: button -> Image
;; Purpose: creates an image that represents the button and then returns it
(define (create-button btn)
  (cond
    [(button-rounded? btn) (overlay (text (string-upcase (button-text btn)) (button-fontSize btn) TEXT-COLOR)
                                    (ellipse (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]
    [else 
     (overlay (scale .5 (text (string-upcase (button-text btn)) (button-fontSize btn) TEXT-COLOR))
              (rectangle (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]))

;; button-pressed? x y -> boolean
;; Purpose: Determins if a specific button was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the button. If so returns true
(define (button-pressed? mouse-x mouse-y btn)
  (cond
    [(and (and (> mouse-x (- (posn-x (button-location btn)) (/ (button-width btn) 2)))
               (< mouse-x (+ (posn-x (button-location btn)) (/ (button-width btn) 2))))
          (and (> mouse-y (- (posn-y (button-location btn)) (/ (button-height btn) 2)))
               (< mouse-y (+ (posn-y (button-location btn)) (/ (button-height btn) 2)))))
     #t]
    [else #f]))

;; run-function: button world -> procedure
;; Purpose: Runs a buttons onClick function if the function has one. Else returns null
(define (run-function btn a-world)
  (cond
    [(procedure? (button-onClick btn)) ((button-onClick btn) a-world)]
    [else null]))

;; set-active-button: button -> button
;; Purpose: Sets a button to active
(define (set-active-button btn)
  (button (button-width btn) (button-height btn) (button-text btn) (button-type btn) (active-color (button-orColor btn)) (button-orColor btn) (button-fontSize btn) (button-rounded? btn) #t (button-location btn) (button-onClick btn)))

;; set-inactive-button: button -> button
;; Purpose: Sets a button to inactive
(define (set-inactive-button btn)
  (button (button-width btn) (button-height btn) (button-text btn) (button-type btn) (button-orColor btn) (button-orColor btn) (button-fontSize btn) (button-rounded? btn) #f (button-location btn) (button-onClick btn)))

;; active-color: color -> color
;; Purpose: given a color will shade the color so it becomes active
(define (active-color c)
  (make-color
   (inexact->exact (truncate (+ (color-red c) (* (- 255 (color-red c)) TINT-FACTOR))))
   (inexact->exact (truncate (+ (color-green c) (* (- 255 (color-green c)) TINT-FACTOR))))
   (inexact->exact (truncate (+ (color-blue c) (* (- 255 (color-blue c)) TINT-FACTOR))))))
