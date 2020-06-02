#lang racket
#| *** FSM Graphical User Interface ***
    Developed by: Marco T. Morazan, Joshua Schappel, and Sachin Mahashabde in 2019. (names in no particular order)
    Goal: Build a GUI for the fsm library in order to help students be able to visualize the machines that the library
            has to offer.
|#

(require 2htdp/image 2htdp/universe "../fsm-main.rkt" "inputFactory.rkt" "./structs/msgWindow.rkt"
         "./structs/button.rkt" "./structs/posn.rkt" "./structs/state.rkt"
         "./structs/input.rkt" "./structs/machine.rkt" "./structs/world.rkt"
         "./structs/world.rkt" "./components/inputFields.rkt" "globals.rkt"
         "./components/buttons.rkt" "./components/stateTransitions.rkt" "./graphViz/main.rkt"
         "inv.rkt")

(provide visualize marco)

;; GLOBAL VALIRABLES FOR FILE
(define MAIN-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene


#|
-----------------------
Initialize World
-----------------------
|# 

;; build-world: machine type msgWindow(optional) -> world
;; Purpose: Creates the initail world with the given machine
(define (build-world m type . msg)
  (letrec (
           (graphviz (system "dot -V"))
           (messageWin (if (null? msg) ;; Determine if a message should be rendered duing on create
                           null
                           (car msg)))
           
           ;; determine-input-list: none -> list-of-input-fields
           ;; Purpose: Determins which input list to use
           (determine-input-list (lambda ()
                                   (case type
                                     [(pda) INPUT-LIST-PDA]
                                     [(tm) INPUT-LIST-TM]
                                     [(tm-language-recognizer) INPUT-LIST-LANG-REC]
                                     [else INPUT-LIST])))

           ;; determine-button-list: none -> list-of-buttons
           ;; Purpose: Determins which button list to use
           (determine-button-list (lambda()
                                    (case type
                                      [(pda) BUTTON-LIST-PDA]
                                      [(tm) BUTTON-LIST-TM]
                                      [(tm-language-recognizer) BUTTON-LIST-LANG-REC]
                                      [else BUTTON-LIST]))))

    (initialize-world m
                      messageWin
                      (if graphviz
                          (cons BTN-DISPLAY (determine-button-list))
                          (determine-button-list))
                      (determine-input-list))))



#|
-----------------------
Cmd Functions
-----------------------
|# 

;; visualize: fsm-machine -> void
;; Purpose: allows a user to pre-load a machine
(define (visualize fsm-machine . args)
  (letrec ((run-program (lambda (w)
                          (big-bang
                              w
                            (name (string-append
                                   (symbol->string (machine-type (world-fsm-machine w)))
                                   ": "
                                   VERSION))
                            (on-draw draw-world)
                            (on-mouse process-mouse-event)
                            (on-key process-key)))))
    
    ;; check if it is a pre-made machine or a brand new one
    (cond
      [(symbol? fsm-machine) ;; Brand new machine
       (case fsm-machine
         [(dfa) (begin
                  (set-machine-type 'dfa)
                  (run-program (build-world (machine '() null '() '() '() '() 'dfa ) 'dfa))
                  (void))]
         [(ndfa) (begin
                   (set-machine-type 'ndfa)
                   (run-program (build-world (machine '() null '() '() '() '() 'ndfa ) 'ndfa))
                   (void))]
         [(pda) (begin
                  (set-machine-type 'pda)
                  (run-program (build-world (pda-machine '() null '() '() '() '() 'pda '()) 'pda))
                  (void))]
         [(tm) (begin
                 (set-machine-type 'tm)
                 (run-program (build-world (tm-machine '() null '() '() `(,LM) '() 'tm 0 ) 'tm))
                 (void))]
         [(tm-language-recognizer) (begin
                                     (set-machine-type 'tm-language-recognizer)
                                     (run-program (build-world (lang-rec-machine '() null '() '() `(,LM) '() 'tm-language-recognizer 0 '||) 'tm-language-recognizer))
                                     (void))]
         [else (error (format "~s is not a valid machine type" fsm-machine))])]

      ;; --- Pre-made with no predicates ---
      [(empty? args)
       (case (sm-type fsm-machine) 
         [(dfa) (begin
                  (set-machine-type 'dfa)
                  (run-program
                   (build-world
                    (machine (map (lambda (x) (fsm-state x TRUE-FUNCTION (posn 0 0))) (sm-getstates fsm-machine))
                             (sm-getstart fsm-machine)
                             (sm-getfinals fsm-machine)
                             (reverse (sm-getrules fsm-machine))
                             '() (sm-getalphabet fsm-machine)
                             (sm-type fsm-machine))
                    (sm-type fsm-machine)
                    (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation."
                               "dfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                  (void))]
         
         [(ndfa) (begin
                   (set-machine-type 'ndfa)
                   (run-program
                    (build-world
                     (machine (map (lambda (x) (fsm-state x TRUE-FUNCTION (posn 0 0))) (sm-getstates fsm-machine))
                              (sm-getstart fsm-machine)
                              (sm-getfinals fsm-machine)
                              (reverse (sm-getrules fsm-machine))
                              '() (sm-getalphabet fsm-machine)
                              (sm-type fsm-machine))
                     (sm-type fsm-machine)
                     (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation."
                                "ndfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                   (void))]
         
         [(pda) (begin
                  (set-machine-type 'pda)
                  (run-program
                   (build-world
                    (pda-machine (map (lambda (x) (fsm-state x PDA-TRUE-FUNCTION (posn 0 0))) (sm-getstates fsm-machine))
                                 (sm-getstart fsm-machine)
                                 (sm-getfinals fsm-machine)
                                 (reverse (sm-getrules fsm-machine))
                                 '() (sm-getalphabet fsm-machine)
                                 (sm-type fsm-machine)
                                 (sm-getstackalphabet fsm-machine))
                    (sm-type fsm-machine)
                    (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation."
                               "pda" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                  (void))]
         
         [(tm) (begin
                 (set-machine-type 'tm)
                 (run-program
                  (build-world
                   (tm-machine (map (lambda (x) (fsm-state x TM-TRUE-FUNCTION (posn 0 0))) (sm-getstates fsm-machine))
                               (sm-getstart fsm-machine)
                               (sm-getfinals fsm-machine)
                               (reverse (sm-getrules fsm-machine))
                               `(,LM) (sm-getalphabet fsm-machine)
                               (sm-type fsm-machine)
                               0)
                   (sm-type fsm-machine)
                   (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation."
                              "tm" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                 (void))]
         
         [(tm-language-recognizer) (begin
                                     (set-machine-type 'tm-language-recognizer)
                                     (run-program
                                      (build-world
                                       (lang-rec-machine (map (lambda (x) (fsm-state x TM-TRUE-FUNCTION (posn 0 0))) (sm-getstates fsm-machine))
                                                         (sm-getstart fsm-machine)
                                                         (sm-getfinals fsm-machine)
                                                         (reverse (sm-getrules fsm-machine))
                                                         `(,LM) (sm-getalphabet fsm-machine)
                                                         (sm-type fsm-machine)
                                                         0
                                                         (sm-getaccept fsm-machine))
                                       (sm-type fsm-machine)
                                       (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation."
                                                  "tm-language-recognizer" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                                     (void))])]

      ;; --- Pre-made with predicates (invariants) ---
      [else
       (letrec ((state-list (sm-getstates fsm-machine))

                ;; get-member symbol list-of-procedure -> procedure
                ;; Purpose: determins if the given symbol is in the procedure
                (get-member (lambda (s los)
                              (cond
                                [(empty? los) '()]
                                [(equal? (caar los) s) (car los)]
                                [else (get-member s (cdr los))]))))

         (case (sm-type fsm-machine)
           [(dfa)
            (begin
              (set-machine-type 'dfa)
              (run-program
               (build-world
                (machine  (map (lambda (x)
                                 (let ((temp (get-member x args)))
                                   (if (empty? temp)
                                       (fsm-state x TRUE-FUNCTION (posn 0 0))
                                       (fsm-state x (cadr temp) (posn 0 0))))) state-list)
                          (sm-getstart fsm-machine)
                          (sm-getfinals fsm-machine)
                          (reverse (sm-getrules fsm-machine))
                          '()
                          (sm-getalphabet fsm-machine)
                          (sm-type fsm-machine))
                (sm-type fsm-machine)
                (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation." "dfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
              (void))]
           
           [(pda)
            (begin
              (set-machine-type 'pda)
              (run-program
               (build-world
                (pda-machine (map (lambda (x)
                                    (let ((temp (get-member x args)))
                                      (if (empty? temp)
                                          (fsm-state x PDA-TRUE-FUNCTION (posn 0 0))
                                          (fsm-state x (cadr temp) (posn 0 0))))) state-list)
                             (sm-getstart fsm-machine)
                             (sm-getfinals fsm-machine)
                             (reverse (sm-getrules fsm-machine))
                             '()
                             (sm-getalphabet fsm-machine)
                             (sm-type fsm-machine)
                             (sm-getstackalphabet fsm-machine))
                (sm-type fsm-machine)
                (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation." "pda" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
              (void))]
           
           [(tm) (begin
                   (begin
                     (set-machine-type 'tm)
                     (run-program
                      (build-world
                       (tm-machine (map (lambda (x)
                                          (let ((temp (get-member x args)))
                                            (if (empty? temp)
                                                (fsm-state x TM-TRUE-FUNCTION (posn 0 0))
                                                (fsm-state x (cadr temp) (posn 0 0))))) state-list)
                                   (sm-getstart fsm-machine)
                                   (sm-getfinals fsm-machine)
                                   (reverse (sm-getrules fsm-machine))
                                   `(,LM)
                                   (sm-getalphabet fsm-machine)
                                   (sm-type fsm-machine)
                                   0)
                       (sm-type fsm-machine)
                       (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation." "tm" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                     (void))
                   )]

           [(tm-language-recognizer) (begin
                                       (begin
                                         (set-machine-type 'tm-language-recognizer)
                                         (run-program
                                          (build-world
                                           (lang-rec-machine (map (lambda (x)
                                                                    (let ((temp (get-member x args)))
                                                                      (if (empty? temp)
                                                                          (fsm-state x TM-TRUE-FUNCTION (posn 0 0))
                                                                          (fsm-state x (cadr temp) (posn 0 0))))) state-list)
                                                             (sm-getstart fsm-machine)
                                                             (sm-getfinals fsm-machine)
                                                             (reverse (sm-getrules fsm-machine))
                                                             `(,LM)
                                                             (sm-getalphabet fsm-machine)
                                                             (sm-type fsm-machine)
                                                             0
                                                             (sm-getaccept fsm-machine))
                                           (sm-type fsm-machine)
                                           (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation." "tm-language-recognizer" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
                                         (void))
                                       )]
           
           [(ndfa)
            (begin
              (set-machine-type 'ndfa)
              (run-program
               (build-world
                (machine  (map (lambda (x)
                                 (let ((temp (get-member x args)))
                                   (if (empty? temp)
                                       (fsm-state x TRUE-FUNCTION (posn 0 0))
                                       (fsm-state x (cadr temp) (posn 0 0))))) state-list)
                          (sm-getstart fsm-machine)
                          (sm-getfinals fsm-machine)
                          (reverse (sm-getrules fsm-machine))
                          '()
                          (sm-getalphabet fsm-machine)
                          (sm-type fsm-machine))
                (sm-type fsm-machine)
                (msgWindow "The pre-made machine was added to the program. Please add variables to the Tape Input and then press 'Run' to start simulation." "ndfa" (posn (/ WIDTH 2) (/ HEIGHT 2)) MSG-SUCCESS)))
              (void))]))])))



#|
-----------------------
Scene Rendering
-----------------------
|# 

;; draw-main-img: world scene -> scene
;; Purpose: Draws the main GUI image
(define (draw-main-img w s)
  (letrec
      (
       (X0  (if (equal? MACHINE-TYPE 'pda)
                (/ (+ (/ WIDTH 11) (- WIDTH 300)) 2)
                (/ (+ (/ WIDTH 11) (- WIDTH 200)) 2)))
       (Y0 (/ (+ TOP (- HEIGHT BOTTOM)) 2))
       (deg-shift (if (empty? (machine-state-list (world-fsm-machine w)))
                      0
                      (/ 360 (length (machine-state-list (world-fsm-machine w))))))
       
       (state-list (machine-state-list (world-fsm-machine w))) ;; The list of states in the world
       
       (get-x (lambda (theta rad) (truncate (+ (* rad (cos (degrees->radians theta))) X0))))
                
       (get-y(lambda (theta rad)
               (truncate (+ (* rad (sin (degrees->radians theta))) Y0))))
       
       (current-index (if (null? (world-cur-state w))
                          0
                          (index-of (map (lambda (x)
                                           (fsm-state-name x)) (machine-state-list (world-fsm-machine w)))
                                    (world-cur-state w))))
       
       (tip-x (get-x (* deg-shift current-index) inner-R))
       (tip-y (get-y (* deg-shift current-index) inner-R))
       (the-arrow(rotate 180 (triangle 15 "solid" "tan")))
       (find-state-pos
        (λ(l i) (if (empty? l) (void)
                    (begin
                      (set-fsm-state-posn! (car l) (posn (get-x (* deg-shift i) R) (get-y (* deg-shift i) R)))
                      (find-state-pos (cdr l) (add1 i))))))
        
       ;;draw-states: list-of-states index scene -> scene
       ;; Purpose: Draws the states onto the GUI
       (draw-states (lambda (l i s)
                      (begin
                        (find-state-pos (machine-state-list (world-fsm-machine w)) 0)
                        
                        (cond[(empty? l) s]
                             [(and (equal? (fsm-state-name (car l)) (machine-start-state (world-fsm-machine w)))
                                   (ormap (lambda(x) (equal? (fsm-state-name (car l)) x))
                                          (machine-final-state-list (world-fsm-machine w))))

                              ;; **
                              (place-image(overlay (text (symbol->string (fsm-state-name (car l))) 25 "black")
                                                   (circle 21 "outline" START-STATE-COLOR)
                                                   (circle 25 "outline" END-STATE-COLOR)
                                                   (circle 30 "outline" END-STATE-COLOR)
                                                   (cond
                                                     [(equal? MACHINE-TYPE 'tm-language-recognizer)
                                                      (if (equal? (fsm-state-name (car l))
                                                                  (lang-rec-machine-accept-state (world-fsm-machine w)))
                                                          (circle 35 "outline" (make-color 123 80 217))
                                                          empty-image)]
                                                     [else empty-image]))
                                          (posn-x (fsm-state-posn (car l)))
                                          (posn-y (fsm-state-posn (car l)))
                                          (draw-states(cdr l) (add1 i) s))]
                             
                             [(equal? (fsm-state-name (car l)) (machine-start-state (world-fsm-machine w)))
                              (place-image(overlay (text (symbol->string (fsm-state-name (car l))) 25 "black")
                                                   (circle 25 "outline" START-STATE-COLOR))
                                          (posn-x (fsm-state-posn (car l)))
                                          (posn-y (fsm-state-posn (car l)))
                                          (draw-states(cdr l) (add1 i) s))]

                             ;; **
                             [(ormap (lambda(x) (equal? (fsm-state-name (car l)) x)) (machine-final-state-list (world-fsm-machine w)))
                              (place-image (overlay (text (symbol->string (fsm-state-name (car l))) 20 "black")
                                                    (overlay
                                                     (circle 20 "outline" END-STATE-COLOR)
                                                     (circle 25 "outline" END-STATE-COLOR)
                                                     (cond
                                                       [(equal? MACHINE-TYPE 'tm-language-recognizer)
                                                        (if (equal? (fsm-state-name (car l))
                                                                    (lang-rec-machine-accept-state (world-fsm-machine w)))
                                                            (circle 30 "outline" (make-color 123 80 217))
                                                            empty-image)]
                                                       [else empty-image])))
                                           (posn-x (fsm-state-posn (car l)))
                                           (posn-y (fsm-state-posn (car l)))
                                           (draw-states (cdr l) (add1 i) s))]
                             [else (place-image (text  (symbol->string (fsm-state-name (car l))) 25 "black")
                                                (posn-x (fsm-state-posn  (car l)))
                                                (posn-y (fsm-state-posn (car l)))
                                                (draw-states (cdr l) (add1 i) s))]))))

       (determin-input-symbol (lambda (cur-rule)
                                (case MACHINE-TYPE
                                  [(pda)
                                   (let((rule (world-cur-rule w)))
                                     (cond
                                       [(or (equal? rule '((empty empty empty) (empty empty)))
                                            (equal? rule '(null null null)))'||]
                                       [else
                                        (cadar rule)]))]
                                  [(tm)
                                   (if (or
                                        (equal? cur-rule 'null)
                                        (equal? cur-rule '((empty empty) (empty empty))))
                                       '||
                                       '||)]
                                  [(tm-language-recognizer)
                                   (if (or
                                        (equal? cur-rule 'null)
                                        (equal? cur-rule '((empty empty) (empty empty))))
                                       '||
                                       '||)]
                                   
                                  [else
                                   (if (or (equal? 'null cur-rule) (equal? 'empty cur-rule))
                                       '||
                                       (cadr (world-cur-rule w)))])))

       (determim-prev-rule (lambda (rule)
                             (let ((c-rule (getCurRule rule)))
                               (case MACHINE-TYPE
                                 [(pda) (caar c-rule)]
                                 [(tm) (caar c-rule)]
                                 [(tm-language-recognizer) (caar c-rule)]
                                 [else
                                  (car c-rule)]))))

       ;; draw-inner-with-prev: none -> image
       ;; Purpose: Creates the inner circle that contains the arrows and the prevous state pointer
       (draw-inner-with-prev (lambda()
                               (letrec ((index (get-state-index state-list (world-cur-state w) 0)))
                                 (overlay
                                  CENTER-CIRCLE
                                  (inner-circle1 (- 360 (* (get-state-index state-list (world-cur-state w) 0) deg-shift))
                                                 (determin-input-symbol (cadr (world-cur-rule w))) index)
                                  (inner-circle2 (- 360 (* (get-state-index
                                                            state-list
                                                            (determim-prev-rule (world-processed-config-list w)) 0)
                                                           deg-shift)))
                                  (circle inner-R "outline" "transparent")))))

       ;; draw-inner-with-prev: none -> image
       ;; Purpose: Creates the inner circle that contains the arrows
       (draw-inner-no-prev (lambda()
                             (letrec ((index (get-state-index state-list (world-cur-state w) 0)))
                               (overlay
                                CENTER-CIRCLE
                                (inner-circle1 (- 360 (* index deg-shift))
                                               (determin-input-symbol (cadr (world-cur-rule w)))
                                               index)
                                (circle inner-R "outline" "transparent")))))
       
       ;; inner-circle1: num symbol num -> image
       ;; Purpose: draws an arrow with the given symbol above it and then rotates it by the given degreese
       (inner-circle1 (lambda(deg sym index)
                        ;;(println sym)
                        (letrec
                            (
                             (state-color (determin-inv
                                           (world-fsm-machine w)
                                           (world-cur-state w)))
                             ;; arrow: none -> image
                             ;; Purpose: draws a arrow
                             (arrow (lambda ()
                                     
                                      (overlay/offset 
                                       (text (symbol->string sym) 18 ARROW-RULE-COLOR)
                                       15 15
                                       (beside/align "center"
                                                     (rectangle (- inner-R 15) 5 "solid" state-color)
                                                     (rotate 270 (triangle 15 "solid" state-color))))))

                             ;; down-arrow: none -> image
                             ;; Purpose: creates an upside-down arrow
                             (down-arrow (lambda ()
                                           (overlay/offset 
                                            (rotate 180 (text (symbol->string sym) 18 ARROW-RULE-COLOR))
                                            15 -15
                                            (beside/align "center"
                                                          (rectangle (- inner-R 15) 5 "solid" state-color)
                                                          (rotate 270 (triangle 15 "solid" state-color)))))))
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
                                          (circle inner-R "outline" "transparent")))]))))

       ;; inner-circle2: num -> image
       ;; Purpose: Draws a doted line and rotates it by the given degreese
       (inner-circle2 (lambda (deg)
                        (letrec
                            ((dot-line (lambda ()
                                         (beside
                                          (line (- inner-R 10) 0 (pen "gray" 5 "short-dash" "butt" "bevel"))
                                          (circle 5 "solid" "gray")))))
                          (rotate deg (overlay/align "right" "center"
                                                     (dot-line)
                                                     (circle (+ inner-R 10) "outline" "transparent"))))))
       
       ;; get-sate-index: list-of-states symbol num -> num
       ;; Purpose: finds the index of the given state in the list of states. Note that a
       ;;     state can not be repeated in the list.
       (get-state-index (lambda (los s accum)
                          (cond
                            [(empty? los) -1] ;; this case should never be reached
                            [(equal? (fsm-state-name (car los)) s) accum]
                            [else (get-state-index (cdr los) s (add1 accum))]))))
                            
    ;; Check if the inner circle needs to be drawn
    (cond
      [(or (null? (world-cur-state w)) (empty? (world-processed-config-list w)))
       (place-image CENTER-CIRCLE X0 Y0
                    (draw-states (machine-state-list (world-fsm-machine w)) 0 s))]
      [else
       ;; see if there is a previous state
       (cond
         [(empty? (cdr (world-processed-config-list w))) ;; there is not a prev state
          (place-image (draw-inner-no-prev) X0 Y0 
                       (draw-states (machine-state-list (world-fsm-machine w)) 0 s))]
         [else ;; there is a prev state
          (place-image (draw-inner-with-prev) X0 Y0 
                       (draw-states (machine-state-list (world-fsm-machine w)) 0 s))])])))


;; draw-world: world -> world
;; Purpose: draws the world every time on-draw is called
(define (draw-world w)
  (letrec(

          (X0  (if (equal? MACHINE-TYPE 'pda)
                   (/ (+ (/ WIDTH 11) (- WIDTH 300)) 2)
                   (/ (+ (/ WIDTH 11) (- WIDTH 200)) 2)))
          (Y0 (/ (+ TOP (- HEIGHT BOTTOM)) 2))

          (machine (world-fsm-machine w))
          ;; draw-input-list: list-of-inputs sceen -> sceen
          ;; Purpose: draws every input structure from the list onto the given sceen
          (draw-input-list (lambda (loi scn)
                             (cond
                               [(empty? loi) scn]
                               [else (draw-textbox (car loi) (draw-input-list (cdr loi) scn))])))
          
          ;; draw-button-list: list-of-buttons sceen -> sceen
          ;; Purpose: draws every button structure from the list onto the given sceen
          (draw-button-list (lambda (lob scn)
                              (cond
                                [(empty? lob) scn]
                                [else (draw-button (car lob) (draw-button-list (cdr lob) scn))])))
          
          ;; draw-error-msg: msgWindow sceen -> sceen
          ;; Purpose: renders the error message onto the screen if there is one.
          (draw-error-msg (lambda (window scn)
                            (cond
                              [(null? window) scn]
                              [else (draw-window window scn WIDTH HEIGHT)])))

          ;; determin-gui-draw: none -> image
          ;; purpose: determins if the tm input position should be passed to the create-gui-right function.
          ;;  This is only true if we are dealing with a tm, Otherwise we pass nothing.
          (determin-gui-draw (lambda ()
                               (case MACHINE-TYPE
                                 [(tm) (create-gui-right (tm-machine-tape-posn machine))]
                                 [(tm-language-recognizer)
                                  (create-gui-right (tm-machine-tape-posn machine) (lang-rec-machine-accept-state machine))]
                                 [else (create-gui-right)])))

          ;;draws the images with an arrow
          (with-arrow (place-image (determin-gui-draw) (- WIDTH 100) (/ HEIGHT 2)
                                   (place-image (create-gui-top (world-fsm-machine w) (world-cur-rule w)) (/ WIDTH 2) (/ TOP 2)
                                                (place-image (create-gui-bottom (machine-rule-list (world-fsm-machine w)) (world-cur-rule w) (world-scroll-bar-index w)) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                                             (draw-button-list (world-button-list w)
                                                                               (draw-input-list (world-input-list w)
                                                                                                (place-image (if (equal? (machine-type machine) 'pda)
                                                                                                                 (create-gui-left
                                                                                                                  (machine-alpha-list machine)
                                                                                                                  (machine-type machine)
                                                                                                                  (pda-machine-stack-alpha-list (world-fsm-machine w)))
                                                                                                                 (create-gui-left
                                                                                                                  (machine-alpha-list machine)
                                                                                                                  (machine-type machine))) (/ (/ WIDTH 11) 2) (/ (- HEIGHT BOTTOM) 2) MAIN-SCENE)))))))
          ;;draws the images without an arrow
          (no-arrow (place-image (determin-gui-draw) (- WIDTH 100) (/ HEIGHT 2)
                                 (place-image (create-gui-top (world-fsm-machine w) (world-cur-rule w)) (/ WIDTH 2) (/ TOP 2)
                                              (place-image (create-gui-bottom (machine-rule-list (world-fsm-machine w)) (world-cur-rule w) (world-scroll-bar-index w)) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                                           (draw-button-list (world-button-list w)
                                                                             (draw-input-list (world-input-list w)
                                                                                              (place-image (if (equal? (machine-type machine) 'pda)
                                                                                                               (create-gui-left
                                                                                                                (machine-alpha-list machine)
                                                                                                                (machine-type machine)
                                                                                                                (pda-machine-stack-alpha-list (world-fsm-machine w)))
                                                                                                               (create-gui-left
                                                                                                                (machine-alpha-list machine)
                                                                                                                (machine-type machine))) (/ (/ WIDTH 11) 2) (/ (- HEIGHT BOTTOM) 2) MAIN-SCENE))))))))
    (cond [IS-GRAPH?   (begin
                         ;;(rectangle 800 450 "solid" "green")
                         #|

(place-image (create-png
                                                                         machine
                                                                         (not (empty? (world-processed-config-list w)))
                                                                         (world-cur-state w)
                                                                         (world-cur-rule w)) X0 Y0  no-arrow)

|#
                         (draw-error-msg (world-error-msg w) (place-image (create-png
                                                                         machine
                                                                         (not (empty? (world-processed-config-list w)))
                                                                         (world-cur-state w)
                                                                         (world-cur-rule w)) X0 Y0  no-arrow)))] ;;graphviz here  
          [else (if (not (null? (world-cur-state w)))
                    (draw-error-msg (world-error-msg w)(draw-main-img w no-arrow))                                                                                                                   
                    (draw-error-msg (world-error-msg w) (draw-main-img w with-arrow)))])))
#|
-----------------------
TOP GUI RENDERING
-----------------------
|# 

;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay/align "right" "top"
                 (control-header3 "Tape Input")
                 (rectangle (/ WIDTH 11) TOP "outline" "transparent")))


;; los-top-label: list-of-sigma (tape input) rule int -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label los cur-rule rectWidth)
  (letrec (

           ;; Gets the tape inptu that needs to be rendered on the screen
           (input-to-render
            (cond
              [(> (length los) TAPE-RENDER-LIMIT)
               (let ((c (drop los TAPE-INDEX)))
                 (take c TAPE-RENDER-LIMIT))]
              [else
               los]))

           
           
           ;; list-2-img: list-of-sigma (tape input) int -> image
           ;; Purpose: Converts the tape input into image that overlays the tape in the center
           (list-2-img (lambda (los accum)
                         (cond
                           [(empty? los) empty-image]
                           [(equal? 1 (length los)) (tape-box (car los) 24 accum)]
                           [else
                            (beside
                             (tape-box (car los) 24 accum)
                             (list-2-img (cdr los) (add1 accum)))
                            ])))

           ;; tape-box: string int int -> image
           ;; Purpose: given a string, will overlay the text onto a image
           (tape-box (lambda (sigma fnt-size index)
                       (cond
                         ;; Check if the sigmas are equal and that it is the right index in the tape input
                         [(<= index TAPE-INDEX-BOTTOM)
                          ;;(and (equal? sigma (cadr cur-rule)) (equal? index TAPE-INDEX-BOTTOM))
                          (overlay
                           (text (symbol->string sigma) fnt-size "gray")
                           (rectangle rectWidth TOP "outline" "transparent"))]
                         [else
                          (overlay
                           (text (symbol->string sigma) fnt-size "Black")
                           (rectangle rectWidth TOP "outline" "transparent"))]))))

    (overlay
     (overlay
      (rectangle (- (- WIDTH (/ WIDTH 11)) 260) TOP "outline" "transparent") ;; this rectangle includes the width of the scroll bars
      (list-2-img input-to-render TAPE-INDEX))
     (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" OUTLINE-COLOR))))


;; tm-los-top input-list current-rule tm-tape-index
(define (tm-los-top los cur-rule tape-index rectWidth)
  (letrec (
           ;; Gets the tape inptu that needs to be rendered on the screen
           (input-to-render
            (cond
              [(> (length los) TAPE-RENDER-LIMIT)
               (let ((c (drop los TAPE-INDEX)))
                 (take c TAPE-RENDER-LIMIT))]
              [else
               los]))

           ;; list-2-img: list-of-sigma (tape input) int -> image
           ;; Purpose: Converts the tape input into image that overlays the tape in the center
           (list-2-img (lambda (los accum)
                         (cond
                           [(empty? los) empty-image]
                           [(equal? 1 (length los)) (tape-box (car los) 24 accum)]
                           [else
                            (beside
                             (tape-box (car los) 24 accum)
                             (list-2-img (cdr los) (add1 accum)))])))


           (input-box (lambda (input highlight? fnt-size)
                        (let ((color (if highlight? TAPE-HIGHLIGHT-COLOR "black")))
                          (overlay
                           (text (symbol->string input) fnt-size color)
                           (rectangle rectWidth (* TOP .75) "outline" OUTLINE-COLOR)))))

           (index-box (lambda (index)
                        (overlay
                         (text (number->string index) 10 "black")
                         (rectangle rectWidth (* TOP .25) "outline" OUTLINE-COLOR))))
                       
                       
                        

           ;; tape-box: string int int -> image
           ;; Purpose: given a string, will overlay the text onto a image
           (tape-box (lambda (sigma fnt-size index)
                       (cond
                         ;; Check if the input is the current hightlighed one
                         [(equal? index tape-index)
                          (overlay
                           (above
                            (input-box sigma #t fnt-size)
                            (index-box index))
                           (rectangle rectWidth TOP "outline" "transparent"))]
                         [else
                          (overlay
                           (above
                            (input-box sigma #f fnt-size)
                            (index-box index))
                           (rectangle rectWidth TOP "outline" "transparent"))]))))
    (overlay
     (overlay/align "left" "middle"
                    (rectangle (- (- WIDTH (/ WIDTH 11)) 260) TOP "outline" "transparent") ;; this rectangle includes the width of the scroll bars
                    (list-2-img input-to-render TAPE-INDEX))
     (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" OUTLINE-COLOR))))

           

           




;; create-gui-top: machine rule cur-tm-index (optional) -> image
;; Creates the top of the gui layout
(define (create-gui-top m cur-rule)
  (let ((input-list (machine-sigma-list m)))
    (case MACHINE-TYPE
      [(tm) (overlay/align "left" "middle"
                           (beside
                            (top-input-label)
                            (tm-los-top input-list cur-rule (tm-machine-tape-posn m) 32))
                           (rectangle WIDTH TOP "outline" "transparent"))]
      [(tm-language-recognizer) (overlay/align "left" "middle"
                                               (beside
                                                (top-input-label)
                                                (tm-los-top input-list cur-rule (tm-machine-tape-posn m) 32))
                                               (rectangle WIDTH TOP "outline" "transparent"))]
      [else
       (overlay/align "left" "middle"
                      (beside
                       (top-input-label)
                       (los-top-label input-list cur-rule 31))
                      (rectangle WIDTH TOP "outline" "transparent"))])))



#|
-----------------------
BOTTOM GUI RENDERING
-----------------------
|# 

;; create-gui-bottom: list-of-rules rule int -> image
;; Purpose: Creates the bottom of the gui layout
(define (create-gui-bottom lor cur-rule scroll-index)
  (cond
    [(empty? lor) (overlay/align "left" "middle"
                                 (align-items
                                  (rules-bottom-label)
                                  (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" OUTLINE-COLOR))
                                 (rectangle WIDTH BOTTOM "outline" "transparent"))]
    [else 
     (overlay/align "left" "middle"
                    (align-items
                     (rules-bottom-label)
                     (lor-bottom-label lor 83 cur-rule scroll-index))
                    (rectangle WIDTH BOTTOM "outline" "transparent"))]))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules:") 24 "Black")
   (rectangle (/ WIDTH 11) BOTTOM "outline" OUTLINE-COLOR)))


;; align-items image image -> image
;; Purpose: Aligns 2 images next to each other
(define (align-items item1 item2)
  (beside
   item1
   item2))

;; lor-bottom-label: list-of-rules int rule int -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label lor rectWidth cur-rule scroll-index)
  (overlay
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" OUTLINE-COLOR)
   (overlay
    (rectangle (- (- (- WIDTH (/ WIDTH 11)) 200) 60) BOTTOM "outline" "transparent")
    (ruleFactory (list-tail (reverse lor) scroll-index) MACHINE-TYPE scroll-index cur-rule))))



;; draw-verticle list int int int -> image
;; Purpose: draws a list vertically, where every element in the list is rendered below each other
(define (draw-verticle loa fnt-size width height)
  (letrec (
           ;; determin-letter-render: symbol -> string
           ;; Purpose: some characters can also be part of the alphabet so we render there symbol
           (determin-letter-render (lambda (letter)
                                     (cond
                                       [(equal? 'LM letter) (symbol->string '@)]
                                       [else (symbol->string letter)])))
           
           ;; t-box: string int -> image
           ;; Purpose: Creates a box for the sting to be placed in
           (t-box (lambda (a-string fnt-size)
                    (overlay
                     (text (determin-letter-render a-string) fnt-size "Black")
                     (rectangle width height "outline" "transparent")))))
    (cond
      [(empty? loa) (rectangle 10 10 "outline" "transparent")]
      [(<= (length loa) 1) (t-box (car loa) fnt-size)]
      [else (above
             (t-box (car loa) fnt-size)
             (draw-verticle (cdr loa) fnt-size width height))])))


#|
-----------------------
LEFT GUI RENDERING
-----------------------
|# 


;; create-gui-left: list of alpha type list-of-gamma(optional) -> image
;; Purpose: Creates the img for the left hand side of the gui
(define (create-gui-left loa type . log)
  (letrec (
           ;; create-alpha-control: list of alpha -> image
           (create-alpha-control (lambda (loa)
                                   (letrec (
                                            (title1-width (/ WIDTH 11)) ;; The width of the title for all machines besides pda's
                                            (title2-width (/ (/ WIDTH 11) 2))) ;; The title width for pdas
                                     
                                     ;; Determine if the gamma needs to be drawin or not.
                                     (cond
                                       [(empty? log)
                                        (overlay/align "right" "top"
                                                       (rectangle (/ WIDTH 11) (- (/ HEIGHT 2) 30) "outline" OUTLINE-COLOR)
                                                       (above
                                                        (control-header2 "Σ" title1-width 18)
                                                        (draw-verticle loa 14 title1-width 14)))]
                                       [else
                                        (overlay/align "right" "top"
                                                       (rectangle (/ WIDTH 11) (- (/ HEIGHT 2) 30) "outline" OUTLINE-COLOR)
                                                       (beside/align "top"
                                                                     (above
                                                                      (control-header2 "Σ" title2-width 18)
                                                                      (draw-verticle loa 14 title2-width 14))
                                                                     (above
                                                                      (control-header2 "Γ" title2-width 18)
                                                                      (draw-verticle (car log) 14 title2-width 14))))])))))
    
    (overlay/align "left" "bottom"
                   (rectangle (/ WIDTH 11) (- HEIGHT BOTTOM) "outline" OUTLINE-COLOR)
                   (create-alpha-control loa))))







#|
-----------------------
RIGHT GUI RENDERING
-----------------------
|# 

;; create-gui-right: tm-tape-positon (oprional) -> image
;; Purpose: creates the left conrol panel for the 
(define (create-gui-right . args)
  (letrec (
           ;; state-right-control: null -> image
           ;; Purpose: Creates the state control panel
           (state-right-control (lambda ()
                                  (overlay/align "left" "top"
                                                 (control-header "State Options")
                                                 (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR))))

                 
           ;; sigma-right-control: none -> image
           ;; Purpose: Creates the alpha control panel
           (sigma-right-control (lambda ()
                                  ;; render the proper display
                                  (cond
                                    [(equal? MACHINE-TYPE 'pda)
                                     (letrec (
                                              ;; draw-left: none -> img
                                              ;; Purpose: Draws the alpha add option
                                              (draw-left (lambda ()
                                                           (overlay/align "left" "top"
                                                                          (rectangle 100 CONTROL-BOX-H "outline" "transparent")
                                                                          (control-header4 "Alpha"))))
                                              ;; draw-right: none -> img
                                              ;; Purpose: Draws the Gamma add options
                                              (draw-right (lambda ()
                                                            (overlay/align "left" "top"
                                                                           (rectangle 100 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                                           (control-header4 "Gamma")))))
                                       (overlay
                                        (beside
                                         (draw-left)
                                         (draw-right))
                                        (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)))]
                                    [else
                                     (overlay/align "left" "top"
                                                    (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                    (control-header "Alpha Options"))])))


           ;; start-right-control: none -> image
           ;; Purpose: Creates the start control panel
           (start-right-control (lambda ()
                                  (cond
                                    [(equal? MACHINE-TYPE 'tm-language-recognizer)
                                     (letrec (
                                              ;; draw-left: none -> img
                                              ;; Purpose: draws the message that telles the user the current position
                                              (draw-tape-index (lambda ()
                                                                 (let ([state (symbol->string (cadr args))])
                                                                   (overlay
                                                                    (beside
                                                                     (text "Current: " 11 (make-color 94 36 23))
                                                                     (text state 14 (make-color 227 153 43)))
                                                                    (rectangle 100 25 "outline" "transparent")))))
                                            
                                              ;; draw-left: none -> img
                                              ;; Purpose: Draws the end add/remove option
                                              (draw-left (lambda ()
                                                           (overlay/align "left" "top"
                                                                          (rectangle 100 CONTROL-BOX-H "outline" "transparent")
                                                                          (control-header5 "Start State"))))
                                              ;; draw-right: none -> img
                                              ;; Purpose: Draws the tape index option
                                              (draw-right (lambda ()
                                                            (overlay/align "left" "top"
                                                                           (rectangle 100 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                                           (above
                                                                            (control-header5 "Accept State")
                                                                            (draw-tape-index))))))
                                       (overlay
                                        (beside
                                         (draw-left)
                                         (draw-right))
                                        (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)))]
                                    [else
                                     (overlay/align "left" "top"
                                                    (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                    (control-header "Start State"))])))


           ;; end-right-control: none -> image
           ;; Purpose: Creates the end control panel
           (end-right-control (lambda ()
                                ;; render the proper display
                                (cond
                                  [(or (equal? MACHINE-TYPE 'tm)
                                       (equal? MACHINE-TYPE 'tm-language-recognizer))
                                   (letrec (
                                            ;; draw-left: none -> img
                                            ;; Purpose: draws the message that telles the user the current position
                                            (draw-tape-index (lambda ()
                                                               (let ([msg (string-append "Current posn:"
                                                                                         (number->string (car args)))])
                                                                 (overlay
                                                                  (text msg 11 (make-color 94 36 23))
                                                                  (rectangle 100 25 "outline" "transparent")))))
                                            
                                            ;; draw-left: none -> img
                                            ;; Purpose: Draws the end add/remove option
                                            (draw-left (lambda ()
                                                         (overlay/align "left" "top"
                                                                        (rectangle 100 CONTROL-BOX-H "outline" "transparent")
                                                                        (control-header4 "End State"))))
                                            ;; draw-right: none -> img
                                            ;; Purpose: Draws the tape index option
                                            (draw-right (lambda ()
                                                          (overlay/align "left" "top"
                                                                         (rectangle 100 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                                         (above
                                                                          (control-header4 "Tape Posn")
                                                                          (draw-tape-index))))))
                                     (overlay
                                      (beside
                                       (draw-left)
                                       (draw-right))
                                      (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)))]
                                  [else
                                   (overlay/align "left" "top"
                                                  (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                  (control-header "End State"))])))


           ;; rule-right-control: none -> image
           ;; Purpose: Creates the rule control panel
           (rule-right-control (lambda ()
                                 (overlay/align "left" "top"
                                                (rectangle 200 CONTROL-BOX-H "outline" OUTLINE-COLOR)
                                                (control-header "Add Rules"))))

           ;; pda-stack: stack-list -> image
           ;; Purpose: creates the control stack image for pdas
           (pda-stack (lambda ()
                        (overlay/align "left" "middle"
                                       (above/align "left"
                                                    (rectangle STACK-WIDTH TOP "outline" "transparent") ;; The top
                                                    (overlay ;; This overlays the stack inside the scroll bar buttons
                                                     (rectangle STACK-WIDTH (- HEIGHT (+ BOTTOM TOP)) "outline" OUTLINE-COLOR)
                                                     (pda-populate-stack))
                                                    (rectangle STACK-WIDTH BOTTOM "outline" "transparent")) ;; the bottom
                                       (rectangle STACK-WIDTH HEIGHT "outline" "transparent"))))

           ;; pda-populate-stack: list -> image
           ;; Purpose: Converts the pda stack into an image to be rendered on the screen
           (pda-populate-stack (lambda ()
                                 (let* (
                                        (rev-stack (reverse STACK-LIST)) ;; No longer in use!!
                                        (len (length STACK-LIST)) ;; Then length of the stack list

                                        (curList ;; the list starting with the STACK-INDEX
                                         (if (> len STACK-LIMIT)
                                             (let ((c (drop-right STACK-LIST STACK-INDEX)))
                                               (take-right c STACK-LIMIT))
                                             STACK-LIST)))                      
                                   (overlay/align "left" "bottom"
                                                  (draw-verticle curList 14 100 29)
                                                  (rectangle STACK-WIDTH (- (- HEIGHT (+ BOTTOM TOP)) 50) "outline" OUTLINE-COLOR)))))

           ;; construct-image: none -> image
           ;;; Purpose: Builds the propper image based on the machine type
           (construct-image (lambda ()
                              (let ((full-width (+ 300 STACK-WIDTH)) ;; the width of the combined images
                                    ;; The right control block that deals with machine minipulation
                                    (control (overlay/align "left" "top"
                                                            (above/align "left"
                                                                         (state-right-control)
                                                                         (sigma-right-control)
                                                                         (start-right-control)
                                                                         (end-right-control)
                                                                         (rule-right-control))
                                                            (rectangle 200 HEIGHT "outline" "gray"))))
                                (case MACHINE-TYPE
                                  [(pda) (overlay/align "left" "top"
                                                        (beside/align "top"
                                                                      (pda-stack)
                                                                      control)
                                                        (rectangle full-width HEIGHT "outline" "transparent"))]
                                  [else control])))))

    (construct-image)))

#|
-----------------------------
ADDITIONAL DRAW FUNCTIONS
-----------------------------
|# 


;; control-header: string -> image
;; Purpose: Creates a header label for right control panel
(define (control-header msg)
  (overlay
   (text (string-upcase msg) 14 "Black")
   (rectangle 200 25 "outline" "transparent")))


;; control-header2: string int int -> image
;; Purpose: Creates a header label that is for the left gui panel
(define (control-header2 msg width ftn-size)
  (overlay
   (text (string-upcase msg) ftn-size "Black")
   (rectangle width 40 "outline" "transparent")))


(define (control-header3 msg)
  (overlay
   (text (string-upcase msg) 14 "Black")
   (rectangle (/ WIDTH 11) 25 "outline" "transparent")))

(define (control-header4 msg)
  (overlay
   (text (string-upcase msg) 14 "Black")
   (rectangle 100 25 "outline" "transparent")))

(define (control-header5 msg)
  (overlay
   (text (string-upcase msg) 12 "Black")
   (rectangle 100 25 "outline" "transparent")))


;; scale-text-to-image image image integer (between 0 and 1) -> image
;; Purpose: Scales the text of an image to not be larger then the image it is overlayed on
(define (scale-text-to-image text img sc)
  (let ((newScale (- sc .2)))
    (cond
      [(> (image-width text) (image-width img))
       (scale-text-to-image (scale newScale text) img 1)]
      [else (overlay (scale sc text) img)])))


#|
------------------
EVENT HANDLERS
------------------
|# 

;; process-mouse-event: world integer integer string --> world
;; Purpose: processes a users mouse event
(define (process-mouse-event w x y me)
  (letrec
      ;; Check-and-set: list-of-input-fields mouse-x mouse-y -> list-of-input-fields
      ;; Purpose: sets the input fields to active or inactive depending on where the mouse click happens
      ((check-and-set (lambda (loi x y)
                        (cond
                          [(empty? loi) '()]
                          [(textbox-pressed? x y (car loi))
                           (cond
                             [(equal? (textbox-active (car loi)) #t) (cons (car loi) (check-and-set (cdr loi) x y))]
                             [else (cons (set-active (car loi)) (check-and-set (cdr loi) x y))])]
                          [else
                           (cond
                             [(equal? (textbox-active (car loi)) #t) (cons (set-inactive (car loi)) (check-and-set (cdr loi) x y))]
                             [else (cons (car loi) (check-and-set (cdr loi) x y))])])))
       
       ;; check-button-list: list-of-buttons mouse-x mosue-y -> button
       ;; Purpose: Iterates over a list of buttons and checks if one was pressed. If so then returns the button otherwise
       ;; it returns null.
       (check-button-list (lambda (lob x y)
                            (cond
                              [(empty? lob) null]
                              [(button-pressed? x y (car lob)) (car lob)]
                              [else (check-button-list (cdr lob) x y)])))

       ;; active-button-list: list-of-buttons mouse-x mouse-y -> list-of-buttons
       ;; Purpose: Creates a new list of buttons where the click button is set to active
       (active-button-list (lambda (lob x y)
                             (cond
                               [(empty? lob) '()]
                               [(button-pressed? x y (car lob)) (cons (set-active-button (car lob)) (active-button-list (cdr lob) x y))]
                               [else (cons (car lob) (active-button-list (cdr lob) x y))])))
       
       ;; checkButtonStates: list-of-states mouse-x mouse-y -> null or state
       ;; Purpose: Returns the state that was pressed on the GUI
       (checkButtonStates (lambda (los x y)
                            (cond
                              [(empty? los) null]
                              [(fsm-state-pressed? x y (car los)) (car los)]
                              [else (checkButtonStates (cdr los) x y)]))))
    (cond
      [(string=? me "button-down")
       (cond
         ;; See if there is an error to be displayed. If so disable all buttons and inputs
         [(not (null? (world-error-msg w)))
          (cond
            [(equal? (exit-pressed? x y (world-error-msg w) WIDTH HEIGHT) #t) (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                                                                     (world-cur-state w) (world-button-list w) (world-input-list w)
                                                                                     (world-processed-config-list w) (world-unporcessed-config-list w) null
                                                                                     (world-scroll-bar-index w))]
            [else (redraw-world w)])]

         ;; Check if a state was pressed
         [(not (null? (checkButtonStates (machine-state-list (world-fsm-machine w)) x y)))
          (begin
            (println
             (string-append
              "State: "
              (symbol->string (fsm-state-name (checkButtonStates (machine-state-list (world-fsm-machine w)) x y)))
              " was pressed"))
            (redraw-world w))]

         ;; Check if a button or input was pressed
         [else (begin
                 (define buttonPressed (check-button-list (world-button-list w) x y))
                 (cond
                   [(not (null? buttonPressed)) (run-function buttonPressed (create-new-world-button w (active-button-list (world-button-list w) x y)))]
                   [else (create-new-world-input w (check-and-set (world-input-list w) x y))]))])]
      [(string=? me "button-up")
       (create-new-world-button w (map (lambda (x) (set-inactive-button x)) (world-button-list w)))]
      [else (redraw-world w)])))




;; process-key: world key-> world
;; Purpose: processes a key users key event
(define (process-key w k)
  (letrec
      ((check-and-add (lambda (loi action)
                        (cond
                          [(empty? loi) '()]
                          [(equal? (textbox-active (car loi)) #t)
                           (cond
                             [(equal? action #t) (cons (add-text (car loi) k) (check-and-add (cdr loi) action))]
                             [else (cons (remove-text (car loi) 1) (check-and-add (cdr loi) action))])]
                          [else (cons (car loi) (check-and-add (cdr loi) action))]))))
    (cond
      [(and (equal? 1 (string-length k)) (or (or (key=? k "-") (key=? k " "))(string<=? "a" (string-downcase k) "z") (string<=? "0" (string-downcase k) "9")))
       (create-new-world-input w (check-and-add (world-input-list w) #t))]
      [(key=? k "\r") (let ([active-textbox (filter (lambda (tbox) (is-active? tbox)) (world-input-list w))])
                        (if (not (empty? active-textbox))
                            (call-proc (car active-textbox) w)
                            w))]
      [(key=? k "\b") (create-new-world-input w (check-and-add (world-input-list w) #f))]
      [else w])))

;; SHHHH you found the easteregg
(define (marco)
  (begin
    (println "♫♪♫")
    (println "Just a functional guy...")
    (println "♫♪♫ BUM BUM BUM BUM ♫♪♫")
    (println "living in an imperative world!!!")
    (println "He choose to use the #lang Racket, for the functional power..")
    (println "♫♪♫")))

