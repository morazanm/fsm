#lang racket
(require
  2htdp/image
  "./structs/state.rkt"
  "./structs/posn.rkt"
  "./structs/world.rkt"
  "./structs/machine.rkt"
  "./stateTransitions.rkt"
  "./globals.rkt"
  "./inv.rkt")

(provide
 draw-gui)

#|

IMPORTANT: Alot of the code in this file is legacy code that could us a rewite. With that
           said I have tried to go though touch up the syntax where I believe I will not
           break anything and added comments where possiable. In the future there should be
           a large scale rewrite of how image drawling is handled

|#


(define (draw-gui world width height)
  (save-image (draw world (empty-scene width height "white") width height)
              "./tmp.png")
  (displayln "Redrawing Image")
  "./tmp.png")


(define (draw world scene width height)
  (define machine (get-field machine world))
  (define X0 (/ width 2))
  (define Y0 (/ height 2))
  (define (get-x theta rad) (truncate (+ (* rad (cos (degrees->radians theta))) X0)))
  (define (get-y theta rad) (truncate (+ (* rad (sin (degrees->radians theta))) Y0)))
  (define deg-shift (match (machine-state-list machine)
                      [`() 0]
                      [l  (/ 360 (length l))]))

  ;; find-state-pos :: listOfStates index -> ()
  ;; This is legacy code that updates the states position. This is used to rearrage the
  ;; states location on the circle when a new state is added/removed
  (define/match (find-state-pos list index)
    [(`() _) (void)] ;; TODO(jschappel): Ask sach about this... This is strange.... 
    [(`(,f ,r ...) i) (begin
                        (set-fsm-state-posn! f (posn
                                                (get-x (* deg-shift i) R)
                                                (get-y (* deg-shift i) R)))
                        (find-state-pos r (add1 i)))])

  ;; get-state-index :: list-of-states  -> symbol -> num -> num
  ;; Finds the index of the given state in the list of states. Note that a
  ;; state can not be repeated in the list.
  (define (get-state-index los s accum)
    (cond
      [(empty? los) -1] ;; this case should never be reached
      [(equal? (fsm-state-name (car los)) s) accum]
      [else (get-state-index (cdr los) s (add1 accum))]))

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

  ;; determin-input-symbol :: rule -> symbol
  ;; This is legacy code that get the current rule that a transiton is on
  ;;  this rule will be rendered on the arrow
  ;; NOTE: the conditional checks are very strange, but I dont want to mess with in
  ;;  in cases it breaks something elsewhere.
  (define (determin-input-symbol cur-rule)
    (match (machine-type machine)
      ['pda (define rule (get-field cur-rule world))
            (cond
              [(or (equal? rule '((empty empty empty) (empty empty)))
                   (equal? rule '(null null null)))'||]
              [else
               (cadar rule)])]
      ['tm (if (or
                (equal? cur-rule 'null)
                (equal? cur-rule '((empty empty) (empty empty))))
               '||
               '||)]
      ['tm-language-recognizer (if (or
                                    (equal? cur-rule 'null)
                                    (equal? cur-rule '((empty empty) (empty empty))))
                                   '||
                                   '||)]
      [_ (if (or (equal? 'null cur-rule) (equal? 'empty cur-rule))
             '||
             (cadr (get-field cur-rule world)))]))

  ;; determin-prev-rule :: rule -> rule
  ;; This is legacy code, get the previos rule that the machine transitioned on
  (define (determin-prev-rule rule)
    (define c-rule (getCurRule rule (get-field type world)))
    (match (machine-type machine)
      ['pda (caar c-rule)]
      ['tm (caar c-rule)]
      ['tm-language-recognizer (caar c-rule)]
      [_ (car c-rule)]))


  ;; draw-inner-with-prev :: image
  ;; This is legacy code that draws the arrows that are used to show the
  ;;  transitions
  (define (draw-inner-with-prev)
    (define cur-rule (get-field cur-rule world))
    (define cur-state (get-field cur-state world))
    (define state-list (machine-state-list (get-field machine world)))
    (define index (get-state-index state-list cur-state 0))
    (overlay
     CENTER-CIRCLE
     (inner-circle1 (- 360 (* (get-state-index state-list cur-state 0) deg-shift))
                    (determin-input-symbol (cadr cur-rule)) index)
     (inner-circle2 (- 360 (* (get-state-index
                               state-list
                               (determin-prev-rule (get-field processed-config-list world)) 0)
                              deg-shift)))
     (circle inner-R "outline" "transparent")))

  ;; draw-inner-no-prev :: image
  ;; Purpose: Creates the inner circle that contains the arrows
  (define (draw-inner-no-prev)
    (define cur-state (get-field cur-state world))
    (define state-list (machine-state-list (get-field machine world)))
    (define index (get-state-index state-list cur-state 0))
    (overlay
     CENTER-CIRCLE
     (inner-circle1 (- 360 (* index deg-shift))
                    (determin-input-symbol (cadr (get-field cur-rule world)))
                    index)
     (circle inner-R "outline" "transparent")))


  ;; inner-circle1 :: num -> symbol -> num -> image
  ;; Purpose: draws an arrow with the given symbol above it and then rotates it by the given degrees
  (define (inner-circle1 deg sym index)
    (define cur-state (get-field cur-state world))
    (define state-color (determin-inv
                         (get-field machine world)
                         cur-state
                         (get-field tape-position world)))
    ;; arrow: none -> image
    ;; Purpose: draws a arrow
    (define (arrow)       
      (overlay/offset 
       (text (symbol->string sym) 18 ARROW-RULE-COLOR)
       15 15
       (beside/align "center"
                     (rectangle (- inner-R 15) 5 "solid" state-color)
                     (rotate 270 (triangle 15 "solid" state-color)))))

    ;; down-arrow: none -> image
    ;; Purpose: creates an upside-down arrow
    (define (down-arrow)
      (overlay/offset 
       (rotate 180 (text (symbol->string sym) 18 ARROW-RULE-COLOR))
       15 -15
       (beside/align "center"
                     (rectangle (- inner-R 15) 5 "solid" state-color)
                     (rotate 270 (triangle 15 "solid" state-color)))))
    (cond
      ;; if if the rotate deg is > 90 and < 180, if so then use the upside-down arrow
      [(and (> deg 90) (< deg 270))
       (rotate deg (overlay/offset
                    (down-arrow)
                    -65 -8
                    (circle inner-R "outline" "transparent")))]
      [else
       (rotate deg (overlay/offset
                    (arrow)
                    -65 8
                    (circle inner-R "outline" "transparent")))]))

  ;; inner-circle2: num -> image
  ;; Purpose: Draws a doted line and rotates it by the given degreese
  (define (inner-circle2 deg)
    (define (dot-line)
      (beside
       (line (- inner-R 10) 0 (pen "gray" 5 "short-dash" "butt" "bevel"))
       (circle 5 "solid" "gray")))
    (rotate deg (overlay/align "right" "center"
                               (dot-line)
                               (circle (+ inner-R 10) "outline" "transparent"))))

  

  ;; Check if the inner circle needs to be drawn
  (cond
    [(or (null? (get-field cur-state world))
         (empty? (get-field processed-config-list world)))
     (place-image CENTER-CIRCLE X0 Y0
                  (draw-states (machine-state-list machine) 0 scene))]
    [else
     ;; see if there is a previous state
     (cond
       [(empty? (cdr (get-field processed-config-list world))) ;; there is not a prev state
        (place-image (draw-inner-no-prev) X0 Y0 
                     (draw-states (machine-state-list machine) 0 scene))]
       [else ;; there is a prev state
        (place-image (draw-inner-with-prev) X0 Y0 
                     (draw-states (machine-state-list machine) 0 scene))])]))