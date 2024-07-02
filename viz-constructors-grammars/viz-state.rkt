#lang racket
(provide (struct-out viz-state)
         (struct-out informative-messages)
         (struct-out instructions-graphic))

;; viz-state is a structure that has
;; imgs - graph images
;; curr-image - current image to be drawn to the screen
;; image-posn - position of the graph image
;; scale-factor - mulitplicative factor used to scale the image while zooming
;; scale-factor-cap - maximum value for scale-factor
;; scale-factor-floor - minimum value for scale-factor
;; curr-mouse-posn - position of the mouse
;; dest-mouse-posn - position where the mouse is dragged
;; mouse-pressed - boolean indicating whether the mouse is pressed
;; click-buffer - an accumulator that prevents a function from being spammed when the user holds down a mouse click
;; informative-messages - a struct that contains all the state and functions necessary to render the informative messages
;; instructions-graphic - a struct that contains all the state and functions necessary to render the instructions graphic
(struct viz-state (imgs curr-image image-posn
                        ;; physical-screen-width physical-screen-height
                        ;; scale to screen size and finally stop having windows being too big or small?
                        ;; seems mostly harmless since we could default back to original values if shell commands fail for any reason
                        scale-factor scale-factor-cap scale-factor-floor
                        prev-mouse-posn curr-mouse-posn mouse-pressed
                        click-buffer informative-messages instructions-graphic))

;; informative-messages is a structure that has
;; draw-component - Function that renders an image of the informative messages
;; component-state - A struct that holds all the data needed to render the informative messages
;; bounding-limits - A struct that contains the bounding coordinates of the informative messages
(struct informative-messages (draw-component component-state bounding-limits))


;; img - The static image to be rendered as the instructions
;; bounding-limits - A struct that contains the bounding coordinates of the instructions graphics
(struct instructions-graphic (img bounding-limits))
