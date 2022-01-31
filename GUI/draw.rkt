#lang racket
(require 2htdp/image 2htdp/universe "./structs/state.rkt" "./structs/posn.rkt" "./structs/world.rkt"
         "./structs/machine.rkt" "./globals.rkt")

(provide
 draw-tester
 draw-gui)

(define (draw-tester m)
  (draw (new world% [machine m]) (empty-scene WIDTH HEIGHT "white")))

(define (draw-gui world width height)
  (save-image (draw world (empty-scene width height "white") width height)
              "./tmp.png")
  (displayln "Redrawing Image")
  "./tmp.png")



(define (draw world scene width height)
  (define machine (get-field machine world))
  (define X0 (/ width 2)) ;;TODO: Add the pda's size in... maybe
  (define (get-x theta rad) (truncate (+ (* rad (cos (degrees->radians theta))) X0)))
  (define Y0 (/ height 2))
  (define (get-y theta rad) (truncate (+ (* rad (sin (degrees->radians theta))) Y0)))
  (define deg-shift (match (machine-state-list machine)
                      [`() 0]
                      [l  (/ 360 (length l))]))

  (define/match (find-state-pos list index)
    [(`() _) (void)] ;; TODO(jschappel): Ask sach about this... This is strange.... 
    [(`(,f ,r ...) i) (begin
                        (set-fsm-state-posn! f (posn
                                                (get-x (* deg-shift i) R)
                                                (get-y (* deg-shift i) R)))
                        (find-state-pos r (add1 i)))])

  ;; draw-states :: listOfStates -> number -> scene -> scene
  ;; This function is part of the legacy code. It is in change of drawing the state
  ;;  names in a circle on the screen. It also addes, the state type identifiers around
  ;;  the circle. 
  (define (draw-states l i s)
    (define (isEndState? name) (ormap (lambda(x) (equal? name x))
                                      (machine-final-state-list machine)))
    (begin
      (find-state-pos (machine-state-list machine) 0)
      (match l
        [`() s]
        [`(,(fsm-state name func pos) ,r ...)
         #:when (and (eq? name (machine-start-state machine))
                     (isEndState? name))
         (place-image (overlay (text (symbol->string name) 25 "black")
                               (circle 21 "outline" START-STATE-COLOR)
                               (circle 25 "outline" END-STATE-COLOR)
                               (circle 30 "outline" END-STATE-COLOR)
                               (cond
                                 [(equal? MACHINE-TYPE 'tm-language-recognizer)
                                  (if (equal? name
                                              (lang-rec-machine-accept-state machine))
                                      (circle 35 "outline" (make-color 123 80 217))
                                      empty-image)]
                                 [else empty-image]))
                      (posn-x pos)
                      (posn-y pos)
                      (draw-states r (add1 i) s))]
        [`(,(fsm-state name func pos) ,r ...)
         #:when (equal? name (machine-start-state machine))
         (place-image (overlay (text (symbol->string name) 25 "black")
                               (circle 25 "outline" START-STATE-COLOR))
                      (posn-x pos)
                      (posn-y pos)
                      (draw-states r (add1 i) s))]
        [`(,(fsm-state name func pos) ,r ...)
         #:when (isEndState? name)
         (place-image (overlay (text (symbol->string name) 20 "black")
                               (overlay
                                (circle 20 "outline" END-STATE-COLOR)
                                (circle 25 "outline" END-STATE-COLOR)
                                (cond
                                  [(equal? MACHINE-TYPE 'tm-language-recognizer)
                                   (if (equal? name
                                               (lang-rec-machine-accept-state machine))
                                       (circle 30 "outline" (make-color 123 80 217))
                                       empty-image)]
                                  [else empty-image])))
                      (posn-x pos)
                      (posn-y pos)
                      (draw-states r (add1 i) s))]
        [`(,(fsm-state name func pos) ,r ...)
         (place-image (text  (symbol->string name) 25 "black")
                      (posn-x pos)
                      (posn-y pos)
                      (draw-states r (add1 i) s))])))

  (draw-states (machine-state-list machine) 0 scene))