#lang racket
(require 2htdp/image 2htdp/universe "posn.rkt" "../globals.rkt")

;; ------- input.rkt -------
;; This file contains the functionality for an input field


;; Export necessary functions/structures
(provide
 (struct-out textbox)
 draw-textbox
 textbox-pressed?
 add-text
 remove-text
 set-active
 set-inactive
 call-proc
 make-textbox
 is-active?)

(define TINT-FACTOR .5) ;; The number to change a color


;; textbox: A structure that represents a GUI Input box
;; - width: integer, the width of the textbox
;; - height: integer, the height of the textbox
;; - color: (color-struct), current(active/inactive) color for the textbox. Set to same as orColor
;; - orColor: (color-struct), The default color of the textbox
;; - text: string, the text to be displayed
;; - charLength: integer that represents the max amount of characters allowed in the textbox
;; - location: posn-struct, the position of the textbox on the scene
;; - active: boolean, ALWAYS SET TO FALSE.
;; - func: procedure, a procedure associated with the textbox
(struct textbox (width height color orColor text charLength location active func))


;; default constructor for a textbox
(define (make-textbox width height posn #:color[color DEFAULT-IMP-COLOR] #:limit[limit 10] #:func[func (void)])
  (textbox width height color color "" limit posn #f func))


;; draw-button: textbox scene -> scene
;; Purpose: Draws a given textbox onto the scene
(define (draw-textbox tBox scn)
  (place-image (overlay
                (text (textbox-text tBox) 12 "black")
                (rectangle (textbox-width tBox) (textbox-height tBox) "solid" (textbox-color tBox)))
               (posn-x (textbox-location tBox)) (posn-y (textbox-location tBox)) scn))

;; textbox-pressed? x y -> boolean
;; Purpose: Determins if a specific textbox was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the textbox. If so returns true
(define (textbox-pressed? mouse-x mouse-y tbox)
  (cond
    [(and (and (> mouse-x (- (posn-x (textbox-location tbox)) (/ (textbox-width tbox) 2)))
               (< mouse-x (+ (posn-x (textbox-location tbox)) (/ (textbox-width tbox) 2))))
          (and (> mouse-y (- (posn-y (textbox-location tbox)) (/ (textbox-height tbox) 2)))
               (< mouse-y (+ (posn-y (textbox-location tbox)) (/ (textbox-height tbox) 2)))))
     #t]
    [else #f]))


;; add-text: textbox string -> textbox
;; Purpose: Adds text onto an existing textbox
(define (add-text tbox msg)
  (cond
    [(> (+ (string-length msg) (string-length (textbox-text tbox))) (textbox-charLength tbox))
     (textbox (textbox-width tbox) (textbox-height tbox) (textbox-color tbox) (textbox-orColor tbox)
              (textbox-text tbox) (textbox-charLength tbox) (textbox-location tbox) (textbox-active tbox) (textbox-func tbox))]
    [else (textbox
           (textbox-width tbox) (textbox-height tbox) (textbox-color tbox) (textbox-orColor tbox) (string-append (textbox-text tbox) msg)
           (textbox-charLength tbox) (textbox-location tbox) (textbox-active tbox) (textbox-func tbox))]))

;; remove-text: textbox int -> textbox
;; Purpose: Removes a specified amount of text from a textbox 
(define (remove-text tbox num)
  (letrec(
          ;; rmv-text: string num -> string
          ;; Purpose: removes a gven number of text from a string. It the stiring is empty or the amount to remove is greater then the string length, will return an empty string.
          (rmv-text (lambda (text num)
                      (cond
                        [(>= (string-length text) num) (substring (textbox-text tbox) 0 (- (string-length (textbox-text tbox)) num))]
                        [else (substring (textbox-text tbox) 0 0)]))))
    (textbox (textbox-width tbox) (textbox-height tbox) (textbox-color tbox) (textbox-orColor tbox) (rmv-text (textbox-text tbox) num)
             (textbox-charLength tbox) (textbox-location tbox) (textbox-active tbox) (textbox-func tbox))))

;; set-active: textbox -> textbox
;; Purpose: Sets a textbox to active
(define (set-active tbox)
  (textbox (textbox-width tbox) (textbox-height tbox) (active-color (textbox-orColor tbox)) (textbox-orColor tbox)
           (textbox-text tbox) (textbox-charLength tbox) (textbox-location tbox) #t (textbox-func tbox)))

;; set-inactive: textbox -> textbox
;; Purpose: Sets a textbox to inactive
(define (set-inactive tbox)
  (textbox (textbox-width tbox) (textbox-height tbox) (textbox-orColor tbox)
           (textbox-orColor tbox) (textbox-text tbox) (textbox-charLength tbox) (textbox-location tbox) #f (textbox-func tbox)))

;; active-color: color -> color
;; Purpose: given a color will shade the color so it becomes active
(define (active-color c)
  (make-color
   (inexact->exact (truncate (+ (color-red c) (* (- 255 (color-red c)) TINT-FACTOR))))
   (inexact->exact (truncate (+ (color-green c) (* (- 255 (color-green c)) TINT-FACTOR))))
   (inexact->exact (truncate (+ (color-blue c) (* (- 255 (color-blue c)) TINT-FACTOR))))))

;; call-proc: textbox world -> world
;; calls the procedure associated with the textbox
(define (call-proc tbox w)
  (if (equal? (void) (textbox-func tbox))
      w
      ((textbox-func tbox) w)))

(define (is-active? tbox)
  (textbox-active tbox))