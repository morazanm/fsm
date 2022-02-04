#lang racket/gui
(require (for-syntax syntax/parse)
         2htdp/image
         framework
         "./structs/machine.rkt"
         "./structs/world.rkt"
         "./structs/posn.rkt"
         "./graphViz/main.rkt"
         "./globals.rkt"
         "./checkMachine.rkt"
         "./draw.rkt"
         "./stateTransitions.rkt"
         "../fsm-main.rkt")

(provide kick-off-gui)


(define MAX-ALPHABET 14)


;; delete-children :: Object -> ()
;; This function will remove all children on a given object
(define (delete-children obj)
  (for ([child (send obj get-children)])
    (send obj delete-child child)))

;; kick-off-gui :: world -> GUI
(define (kick-off-gui machine)
  (color-prefs:white-on-black)
  (define world (new world% [machine machine]))

  ;; event-dispatcher symbol -> value -> ()
  ;; The event dispactech is in change of all events that need to be handled by
  ;; the GUI. This allows for one central control point that can be used to reference
  ;; how events are handled
  (define (event-dispatcher event value)
    (define isActive (eq? 'active (get-field mode world)))

    ;;rerenders everything
    (define (full-re-render)
      (remake-image)
      (delete-children rule-display) (render-rule-list)
      (delete-children tape-display) (render-tape-list))
    
    ;; sets a machine to the idle state and handles necessary redraws
    (define (setIdle)
      (send world setMode 'idle)
      (send world reset)
      (delete-children inner-alpha-display) (render-alpha-list)
      (delete-children rule-display) (render-rule-list)
      (delete-children tape-display) (render-tape-list)
      (remake-image))

    ;; sets a world to the active state and handles necessary redraws
    (define (setActive)
      (send world setMode 'active)
      ;; re-render necessary fields
      (delete-children inner-alpha-display) (render-alpha-list)
      (delete-children rule-display) (render-rule-list)
      (delete-children tape-display) (render-tape-list)
      (remake-image))

    ;; can-go-next :: string
    ;; Determins if there is an error message that needs to be rendered
    ;; when hitting the 'next' button
    (define (can-go-next)
      (define unpros-list (get-field unprocessed-config-list world))
      (cond
        [(not isActive) "You must build your machine before you can continue. Please press 'Run' to proceed."]
        ;; sm-showtransitions for tm and lang rec can sometimes return a string,
        ;; if they do then we have an error and need to render the message as set to idle
        ;; till we fix the error
        [(and (not (empty? unpros-list))
              (string? (car unpros-list))) (car unpros-list)]
        [else ""]))
    (match event
      ['addAlpha (begin
                   (send world addAlpha value)
                   (delete-children inner-alpha-display)
                   (when isActive
                     (setIdle))
                   (render-alpha-list))]
      ['removeAlpha (begin
                      (send world removeAlpha value)
                      (setIdle))]
      ['addStart (begin
                   (send world addStart value)
                   (setIdle) ;; regardless if the world's mode we need to redraw so just call setIdle
                   )]
      ['removeStart (begin
                      (define needsRedraw (send world removeStart value))
                      (when needsRedraw
                        (setIdle)))]
      ['addState (begin
                   (define needsRedraw (send world addState value))
                   (displayln needsRedraw)
                   (when needsRedraw
                     (setIdle)))]
      ['removeState (begin
                      (define needsRedraw (send world removeState value))
                      (when needsRedraw
                        (setIdle)))]
      ['addEnd (begin
                 (define needsRedraw (send world addEnd value))
                 (when needsRedraw
                   (setIdle)))]      
      ['removeEnd (begin
                    (define needsRedraw (send world removeEnd value))
                    (when needsRedraw
                      (setIdle)))]
      ['addRule (begin
                  (define needsRedraw (send world addRule value))
                  (when needsRedraw
                    (when isActive
                      (setIdle))
                    (when (eq? 'graphviz (get-field view-mode world))
                      (remake-image))
                    (delete-children rule-display)
                    (render-rule-list)))]
      ['removeRule (begin
                     (define needsRedraw (send world removeRule value))
                     (when needsRedraw
                       (when isActive
                         (setIdle))
                       (when (eq? 'graphviz (get-field view-mode world))
                         (remake-image))
                       (delete-children rule-display)
                       (render-rule-list)))]
      ['clearTape (begin
                    (send world clearTape)
                    (when isActive
                      (setIdle))
                    (delete-children tape-display)
                    (render-tape-list))]
      ['addTape (begin
                  (define allValid (send world addTape value))
                  (if (not allValid)
                      (begin
                        (send tape-error-win show #t))
                      (begin
                        (when isActive
                          (setIdle))
                        (delete-children tape-display)
                        (render-tape-list))))]
      ['runProgram (begin
                     (define msg (check-for-errors (get-field machine world)))
                     (if (not (eq? msg ""))
                         (begin
                           (send machine-error-win-text set-label msg)
                           (send machine-error-win show #t))
                         (begin
                           (match-let ([(new-machine-vars unpros pros new-machine) (build-new-interal-machine
                                                                                    (get-field machine world))])
                             (send world setMachine new-machine)
                             (send world setCurState (machine-start-state new-machine))
                             (send world setUnprocessedList unpros)
                             (send world setProcessedList pros)
                             (setActive)
                             (send machine-success-win show #t)))))]
      ['setMode (match value
                  ['control (begin
                              (send world setViewMode value)
                              (remake-image))]
                  ['graphviz (begin
                               (if (eq? #f (get-field has-gviz world))
                                   (begin
                                     (send graph-view check #f)
                                     (send machine-error-win-text set-label "You must first download Graphviz to use this feature:\nhttps://graphviz.org/download/")
                                     (send machine-error-win show #t)
                                     (remake-image))
                                   (begin
                                     (send control-view check #f)
                                     (send world setViewMode value)
                                     (remake-image))))])]
                                     
      ['goNext (begin
                 (define msg (can-go-next))
                 (if (not (equal? "" msg))
                     (begin
                       (send machine-error-win-text set-label msg)
                       (send machine-error-win show #t))
                     (begin
                       (let [(msg (at-end-msg world))]
                         ;; see if we are at the end, if so then render approperate msg
                         (if (not (equal? msg ""))
                             (begin
                               (send machine-end-win-text set-label msg)
                               (send machine-end-win show #t))
                             (begin
                               ;; Now we can determine the next state of the machine
                               (goNext world)
                               (full-re-render)))))))]
      ['goPrev (begin
                 (define msg (can-go-next))
                 (if (not (equal? "" msg))
                     (begin
                       (send machine-error-win-text set-label msg)
                       (send machine-error-win show #t))
                     (begin
                       (let [(msg (at-beginning-msg world))]
                         (if (not (equal? "" msg))
                             (begin
                               (send machine-end-win-text set-label msg)
                               (send machine-end-win show #t))
                             (begin
                               (goPrev world)
                               (full-re-render)))))))]
                 
      [_ (error (format "Invalid event to dispatch on '~s'" (symbol->string event)))]))



  
  ;; ***************************** BELOW ARE THE GUI OBJECTS *****************************

  (define WELCOME-MSG "FSM Visualization Tool: ")

  ; Make a frame by instantiating the frame% class
  (define frame (new frame%
                     [width 1400]
                     [height 700]
                     [style '(fullscreen-button)]
                     [label (string-append WELCOME-MSG (symbol->string (get-field type world)))]))

  ;; ----------------------------- MENU BAR -----------------------------
  (define menu-bar (new menu-bar% [parent frame]))

  (define file-menu (new menu% [label "File"] [parent menu-bar]))
  (define save-menu-item (new menu-item% [label "Save"] [parent file-menu] [callback (lambda (v x) (void))]))
  (define load-menu-item (new menu-item% [label "Load"] [parent file-menu] [callback (lambda (v x) (void))]))


  (define machine-menu (new menu% [label "Machine"] [parent menu-bar] [help-string "Change machine type"]))
  (define dfa-menu-item (new menu-item%
                             [label "Dfa"]
                             [parent machine-menu]
                             [callback (lambda (btn event) (send frame set-label (string-append WELCOME-MSG "Dfa")))]))
  (define ndfa-menu-item (new menu-item%
                              [label "Ndfa"]
                              [parent machine-menu]
                              [callback (lambda (btn event) (send frame set-label (string-append WELCOME-MSG "Ndfa")))]))
  (define pda-menu-item (new menu-item%
                             [label "Pda"]
                             [parent machine-menu]
                             [callback (lambda (btn event) (send frame set-label (string-append WELCOME-MSG "Pda")))]))
  (define tm-menu-item (new menu-item%
                            [label "Turing Machine"]
                            [parent machine-menu]
                            [callback (lambda (btn event) (send frame set-label (string-append WELCOME-MSG "Turing Machine")))]))

  (define window-menu (new menu% [label "Window"] [parent menu-bar]))
  (define color-blind-menu-item (new menu-item% [label "Toggle Color Blind Mode"] [parent window-menu] [callback (lambda (v x) (void))]))

  (define mode-menu (new menu% [label "Mode"] [parent menu-bar]))
  (define control-view (new checkable-menu-item%
                            [label "Control"]
                            [parent mode-menu]
                            [checked #t]
                            [callback (lambda (button event)
                                        (send control-view check #t)
                                        (send graph-view check #f) 
                                        (event-dispatcher 'setMode 'control))]))

  (define graph-view (new checkable-menu-item%
                          [label "Graph"]
                          [parent mode-menu]
                          [checked #f]
                          [callback (lambda (button event)
                                      (send graph-view check #t)
                                      ;; We will set control-view to not checked in the dispatcher to take
                                      ;; care of the case where g-viz is not installed
                                      (event-dispatcher 'setMode 'graphviz))]))
  

  (define help-menu (new menu% [label "Help"] [parent menu-bar]))
  (define viz-tool-menu-item (new menu-item% [label "User Guide"] [parent help-menu] [callback (lambda (v x) (void))]))
  (define fsm-doc-menu-item (new menu-item% [label "FSM Documentation"] [parent help-menu] [callback (lambda (v x) (void))]))
  (define racekt-doc-menu-item (new menu-item% [label "Racket Documentation"] [parent help-menu] [callback (lambda (v x) (void))]))
  (define about-menu-item (new menu-item% [label "About"] [parent help-menu] [callback (lambda (v x) (void))]))


  (define top-level (new horizontal-panel%
                         [parent frame]))

  ;;***********************************************************************************************************
  ;;********************************************** LEFT SIDE BELOW ********************************************
  ;;***********************************************************************************************************
  ;;------------------------------------------------------------------------------------------------------------
  ;; Tape
  ;;------------------------------------------------------------------------------------------------------------
  (define left-side (new vertical-panel%
                         [parent top-level]
                         [stretchable-width #f]
                         [style (list 'border)]
                         [alignment '(center top)]))


  (define tape-panel (new vertical-panel%
                          [parent left-side]
                          [style (list 'border)]
                          [stretchable-height #f]
                          [border 10]
                          [alignment '(center center)]))
  (new message% [parent tape-panel] [label "Tape"])
  (define tape-field (new text-field% [parent tape-panel] [label ""]))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel-tape (new horizontal-panel%
                          [parent tape-panel]
                          [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel-tape]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor (send tape-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? text-value ""))
                     (send editor erase)
                     (event-dispatcher 'addTape (map
                                                 string->symbol
                                                 (string-split text-value)))))])
  (new button%
       [parent panel-tape]
       [label "Clear"]
       [callback (lambda (button event)
                   (event-dispatcher 'clearTape 'noAction))])
  (when (system-position-ok-before-cancel?)
    (send panel-tape change-children reverse))
  

  (define control-panel (new vertical-panel%
                             [parent left-side]
                             [border 5]
                             [style '(border)]
                             [stretchable-height #f]
                             [alignment '(center center)]))

  
  (new button% [parent control-panel]
       [label "Run"]
       [min-width 120]
       [min-height 10]
       [callback (lambda (button event)
                   (event-dispatcher 'runProgram 'noAction))])


  (new button% [parent control-panel] [label "Gen Code"] [min-width 120] [min-height 50])



  (define fwd-rev-panel (new horizontal-panel%
                             [parent control-panel]
                             [alignment '(center center)]))
  (new button% [parent fwd-rev-panel]
       [label "🠈"]
       [callback (lambda (button event)
                   (event-dispatcher 'goPrev 'noAction))])
  (new button% [parent fwd-rev-panel]
       [label "🠊"]
       [callback (lambda (button event)
                   (event-dispatcher 'goNext 'noAction))])

  (define sig-font (make-font #:size 16
                              #:family 'swiss
                              #:weight 'bold
                              #:underlined? #t))

  (define alpha-display (new vertical-panel%
                             [parent left-side]
                             [min-height 200]
                             [style '(border)]
                             [border 5]
                             [alignment '(center top)]))
  (new message% [parent alpha-display] [label "Σ"] [font sig-font])

  (define inner-alpha-display (new vertical-panel%
                                   [parent alpha-display]
                                   [style '(vscroll)]
                                   [alignment '(center top)]))
  ;; render-alpha-list :: listOfSymbol -> msg
  ;; Renders the alphabet on the screen
  (define (render-alpha-list)
    (for ([alpha (machine-alpha-list (get-field machine world))])
      (new message% [parent inner-alpha-display] [enabled #t]
           [label (symbol->string alpha)])))
  (define alpha-list (render-alpha-list));;(machine-alpha-list (world2-fsm-machine world)));; TODO:(Jschappel) Maybe move this?


  ;;***********************************************************************************************************
  ;;********************************************** Center *****************************************************
  ;;***********************************************************************************************************
  (define myfont (make-font #:size 16
                            #:family 'swiss))

  (define center-side (new vertical-panel%
                           [parent top-level]
                           [style (list 'border)]
                           [min-width 700]
                           [min-height 300]
                           [alignment '(center center)]))

  (define tape-display (new horizontal-panel%
                            [parent center-side]
                            [min-height 60]
                            [style '(hscroll border)]
                            [spacing 10]
                            [stretchable-height #f]
                            [alignment '(center center)]))

  (define (render-tape-list)
    (define sigma-list (machine-sigma-list (get-field machine world)))
    (for ([input sigma-list]
          [index (in-range (length sigma-list))])
      (if (> index (get-field tape-position world))
          (new message%
               [parent tape-display]
               [font myfont]
               [enabled #t]
               [label (symbol->string input)])
          (new message%
               [parent tape-display]
               [font myfont]
               [enabled #t]
               [color "Purple"]
               [label (symbol->string input)]))))



  (define machine-panel (new panel%
                             [min-height 180]
                             [parent center-side]))


  (define firstTime? #t)
  (define image null)
  ;; remake-image :: path

  (define machine-view (new canvas%
                            [parent machine-panel]
                            [paint-callback
                             (lambda (canvas dc)
                               (when firstTime?
                                 (set! firstTime? #f)
                                 (remake-image))
                               (send dc draw-bitmap image
                                     (- (/ (send machine-panel get-width) 2)
                                        (/ (send image get-width)  2))
                                     (-
                                      (/ (send machine-panel get-height) 2)
                                      (/ (send image get-height) 2)))
                               (make-screen-bitmap 100 100))]))

  (define (remake-image)
    (define-values (width height) (send machine-view get-scaled-client-size))
    (begin
      (match (get-field view-mode world)
        ['graphviz (begin
                     (save-image (world->graph-png
                                  (not (empty? (get-field processed-config-list world)))
                                  world
                                  width
                                  height)
                                 "tmp.png")
                     (set! image (read-bitmap "tmp.png" 'png)))]
        ['control (set! image (read-bitmap (draw-gui world width height)))])
      (send machine-view refresh-now)))


                 


  (define rule-display (new horizontal-panel%
                            [parent center-side]
                            [min-height 60]
                            [style '(hscroll border)]
                            [spacing 10]
                            [border 10]
                            [stretchable-height #f]
                            [alignment '(center center)]))

  (define  (render-rule-list)
    (for ([i (machine-rule-list (get-field machine world))])
      (define o (open-output-string))
      (write i o)
      (if (equal? i (get-field cur-rule world))    
          (new message%
               [parent rule-display]
               [font myfont]
               [color "Purple"]
               [enabled #t]
               [label (get-output-string o)])
          (new message%
               [parent rule-display]
               [font myfont]
               [enabled #t]
               [label (get-output-string o)]))))
  (render-rule-list)



  ;;***********************************************************************************************************
  ;;********************************************** RIGHT SIDE BELOW *******************************************
  ;;***********************************************************************************************************
  ;;------------------------------------------------------------------------------------------------------------
  ;; State (Add/Remove)
  ;;------------------------------------------------------------------------------------------------------------
  (define right-side (new vertical-panel%
                          [parent top-level]
                          [stretchable-width #f]
                          [min-width 50]
                          [style (list 'border)]
                          [alignment '(right center)]))


  (define state-panel (new vertical-panel%
                           [parent right-side]
                           [style (list 'border)]
                           [border 10]
                           [alignment '(center center)]))
  (new message% [parent state-panel] [label "State Options"])
  (define state-text-field (new text-field% [parent state-panel] [label ""]))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel (new horizontal-panel% [parent state-panel]
                     [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor (send state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? text-value ""))
                     (send editor erase)
                     (event-dispatcher 'addState (string->symbol text-value))))])
                
  (new button%
       [parent panel]
       [label "Remove"]
       [callback (lambda (button event)
                   (define editor (send state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? text-value ""))
                     (send editor erase)
                     (event-dispatcher 'removeState (string->symbol text-value))))])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))


  ;;------------------------------------------------------------------------------------------------------------
  ;; Alpha
  ;;------------------------------------------------------------------------------------------------------------
  (define alpha-panel (new vertical-panel%
                           [parent right-side]
                           [style (list 'border)]
                           [border 10]
                           [alignment '(center center)]))
  (new message% [parent alpha-panel] [label "Alpha Options"])
  (define alpha-text-field (new text-field%
                                [parent alpha-panel]
                                [label ""]))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel4 (new horizontal-panel% [parent alpha-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel4]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor (send alpha-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (and (not (eq? text-value ""))
                              (<= (+ 1 (length (send world get-machine-alpha-list))) MAX-ALPHABET))
                     (begin
                       (send editor erase)
                       (event-dispatcher 'addAlpha (string->symbol text-value)))))])

  (new button%
       [parent panel4]
       [label "Remove"]
       [callback (lambda (button event)
                   (define editor (send alpha-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? text-value ""))
                     (begin
                       (send editor erase)
                       (event-dispatcher 'removeAlpha (string->symbol text-value)))))])

  (when (system-position-ok-before-cancel?)
    (send panel4 change-children reverse))

  ; IMPT!!!: Commented out code is for future addition
  ;(define alpha-gamma-panel (new horizontal-panel% [parent left-side] [alignment '(center center)]))
  ;(define alpha-panel (new vertical-panel%
  ;                        [parent alpha-gamma-panel]
  ;                        [style (list 'border)]
  ;                        [border 10]
  ;                        [alignment '(center center)]))
  ;(new message% [parent alpha-panel] [label "Alpha"])
  ;(new text-field% [parent alpha-panel] [label ""])
  ;(define panel4 (new horizontal-panel% [parent alpha-panel]
  ;                    [alignment '(center center)]))
  ;; Add Cancel and Ok buttons to the horizontal panel
  ;(new button% [parent panel4] [label "Add"])
  ;(new button% [parent panel4] [label "Remove"])
  ;(when (system-position-ok-before-cancel?)
  ;  (send panel change-children reverse))
  ;
  ;(define gamma-panel (new vertical-panel%
  ;                        [parent alpha-gamma-panel]
  ;                        [style (list 'border)]
  ;                        [border 10]
  ;                        [alignment '(center center)]))
  ;(new message% [parent gamma-panel] [label "Gamma"])
  ;(new text-field% [parent gamma-panel] [label ""])
  ;(define panel5 (new horizontal-panel% [parent gamma-panel]
  ;                    [alignment '(center center)]))
  ;; Add Cancel and Ok buttons to the horizontal panel
  ;(new button% [parent panel5] [label "Add"])
  ;(new button% [parent panel5] [label "Remove"])
  ;(when (system-position-ok-before-cancel?)
  ;  (send panel change-children reverse))
  

  ;;------------------------------------------------------------------------------------------------------------
  ;; StartState
  ;;------------------------------------------------------------------------------------------------------------
  (define start-panel (new vertical-panel%
                           [parent right-side]
                           [style (list 'border)]
                           [border 10]
                           [alignment '(center center)]))
  (new message% [parent start-panel] [label "Start State"])
  (define start-state-text-field (new text-field% [parent start-panel] [label ""]))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel2 (new horizontal-panel% [parent start-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel2]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor (send start-state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? "" text-value))
                     (send editor erase)
                     (event-dispatcher 'addStart (string->symbol text-value))))])
  (new button%
       [parent panel2]
       [label "Remove"]
       [callback (lambda (button event)
                   (define editor (send start-state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? "" text-value))
                     (send editor erase)
                     (event-dispatcher 'removeStart (string->symbol text-value))))])
  (when (system-position-ok-before-cancel?)
    (send panel2 change-children reverse))


  ;;------------------------------------------------------------------------------------------------------------
  ;; End State
  ;;------------------------------------------------------------------------------------------------------------
  (define end-panel (new vertical-panel%
                         [parent right-side]
                         [style (list 'border)]
                         [border 10]
                         [alignment '(center center)]))
  (new message% [parent end-panel] [label "End State"])
  (define end-state-text-field (new text-field% [parent end-panel] [label ""]))
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel3 (new horizontal-panel% [parent end-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel3]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor (send end-state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? "" text-value))
                     (send editor erase)
                     (event-dispatcher 'addEnd (string->symbol text-value))))])
  (new button%
       [parent panel3]
       [label "Remove"]
       [callback (lambda (button event)
                   (define editor (send end-state-text-field get-editor))
                   (define text-value (string-trim (send editor get-text)))
                   (when (not (eq? "" text-value))
                     (send editor erase)
                     (event-dispatcher 'removeEnd (string->symbol text-value))))])
  (when (system-position-ok-before-cancel?)
    (send panel3 change-children reverse))


  ;;------------------------------------------------------------------------------------------------------------
  ;; Rules
  ;;------------------------------------------------------------------------------------------------------------
  (define rule-panel (new vertical-panel%
                          [parent right-side]
                          [style (list 'border)]
                          [border 10]
                          [alignment '(center center)]))
  (new message% [parent rule-panel] [label "Add Rule"])
  (define panel5 (new horizontal-panel% [parent rule-panel] [alignment '(center center)]))
  (define rule-start-text-field (new text-field% [parent panel5] [label ""] [stretchable-height #f]))
  (define rule-alpha-text-field (new text-field% [parent panel5] [label ""] [stretchable-height #f]))
  (define rule-end-text-field (new text-field% [parent panel5] [label ""] [stretchable-height #f]))
  (define panel6 (new horizontal-panel% [parent rule-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent panel6]
       [label "Add"]
       [callback (lambda (button event)
                   (define editor1 (send rule-start-text-field get-editor))
                   (define editor2 (send rule-alpha-text-field get-editor))
                   (define editor3 (send rule-end-text-field get-editor))
                   (define start (string-trim (send editor1 get-text)))
                   (define alpha (string-trim (send editor2 get-text)))
                   (define end (string-trim (send editor3 get-text)))
                   (define (notEmpty val) (not (eq? "" val)))
                   (when (and (notEmpty start)
                              (notEmpty alpha)
                              (notEmpty end))
                     
                     (send editor1 erase)
                     (send editor2 erase)
                     (send editor3 erase)
                     (event-dispatcher 'addRule `(,(string->symbol start)
                                                  ,(string->symbol alpha)
                                                  ,(string->symbol end)))))])
  (new button%
       [parent panel6]
       [label "Remove"]
       [callback (lambda (button event)
                   (define editor1 (send rule-start-text-field get-editor))
                   (define editor2 (send rule-alpha-text-field get-editor))
                   (define editor3 (send rule-end-text-field get-editor))
                   (define start (string-trim (send editor1 get-text)))
                   (define alpha (string-trim (send editor2 get-text)))
                   (define end (string-trim (send editor3 get-text)))
                   (define (notEmpty val) (not (eq? "" val)))
                   (when (and (notEmpty start)
                              (notEmpty alpha)
                              (notEmpty end))
                     
                     (send editor1 erase)
                     (send editor2 erase)
                     (send editor3 erase)
                     (event-dispatcher 'removeRule `(,(string->symbol start)
                                                     ,(string->symbol alpha)
                                                     ,(string->symbol end)))))])
  (when (system-position-ok-before-cancel?)
    (send panel6 change-children reverse))

  ;;------------------------------------------------------------------------------------------------------------
  ;; Message Windows
  ;;------------------------------------------------------------------------------------------------------------
  (define tape-error-win (new dialog% [label "Error"]))
  (define tape-error-win-panel (new vertical-panel%
                                    [parent tape-error-win]
                                    [border 5]
                                    [stretchable-height #f]
                                    [alignment '(center center)]))
  (new message% [parent tape-error-win-panel]
       [label "One or more of the tape input is not in the alphabet. Please add them and try again"])
  (new button% [parent tape-error-win-panel]
       [label "Continue"]
       [callback (lambda (button event)
                   (send tape-error-win show #f))])

  ; -----   
  (define machine-error-win (new dialog% [label "Error"]))
  
  (define machine-error-win-panel (new vertical-panel%
                                       [parent machine-error-win]
                                       [border 5]
                                       [stretchable-height #f]
                                       [alignment '(center center)]))
  (define machine-error-win-text (new message%
                                      [parent machine-error-win-panel]
                                      [auto-resize #t]
                                      [label ""]))
  (new button% [parent machine-error-win-panel]
       [label "Continue"]
       [callback (lambda (button event)
                   (send machine-error-win show #f))])
  
  ; -----

  (define machine-success-win (new dialog% [label "Success"]))

  (define machine-success-win-panel (new vertical-panel%
                                         [parent machine-success-win]
                                         [border 5]
                                         [stretchable-height #f]
                                         [alignment '(center center)]))
  (define machine-success-win-text (new message%
                                        [parent machine-success-win-panel]
                                        [auto-resize #t]
                                        [label "The machine was successfully built. Press Next and Prev to show the machines transitions"]))
  (new button% [parent machine-success-win-panel]
       [label "Continue"]
       [callback (lambda (button event)
                   (send machine-success-win show #f))])

  ; -----

  (define go-next-error-win (new dialog% [label "Error"]))

  (define go-next-error-win-panel (new vertical-panel%
                                       [parent go-next-error-win]
                                       [border 5]
                                       [stretchable-height #f]
                                       [alignment '(center center)]))
  (define go-next-error-win-text (new message%
                                      [parent go-next-error-win-panel]
                                      [auto-resize #t]
                                      [label ""]))
  (new button% [parent go-next-error-win-panel]
       [label "Continue"]
       [callback (lambda (button event)
                   (send go-next-error-win show #f))])


  ; -----   
  (define machine-end-win (new dialog% [label "Information"]))
  
  (define machine-end-win-panel (new vertical-panel%
                                     [parent machine-end-win]
                                     [border 5]
                                     [stretchable-height #f]
                                     [alignment '(center center)]))
  (define machine-end-win-text (new message%
                                    [parent machine-end-win-panel]
                                    [auto-resize #t]
                                    [label ""]))
  (new button% [parent machine-end-win-panel]
       [label "Done"]
       [callback (lambda (button event)
                   (send machine-end-win show #f))])
  
  (send frame show #t))




;; at-beginning-msg :: world -> string
;; returns the propper msg to display when the machine is at the start of the tape
;; if it is not at the start of the tape then the empty string is returned
(define (at-beginning-msg world)
  (if (empty? (cdr (get-field processed-config-list world)))
      "You have reached the beginning of the machine! There are no more previous states."
      ""))
  

;; at-end-msg :: world -> string
;; returns the propper msg to display when the machine reaches the end of the tape
;; if it is not at the end of the tape then the empty string is returned
(define (at-end-msg world)
  (define type (get-field type world))
  (define machine (get-field machine world))
  (define unpros-list (get-field unprocessed-config-list world))
  (define pros-list (get-field processed-config-list world))
  (cond
    ;; Lang recs have a seperate end conditon so we will check it here
    [(and (empty? unpros-list)
          (equal? type 'tm-language-recognizer))
     (if (equal? (caar pros-list) (lang-rec-machine-accept-state machine))
         "The input is accepted."
         "The input is rejected.")]
    [else
     (match (car unpros-list)
       ['accept "The input is accepted."]
       ['reject "The input is rejected."]
       ['halt "The machine has halted!!"]
       [_ ""])]))


;; goPrev :: world -> ()
;; Handles all functionality around setting the world up for the previous state transition
(define (goPrev world)
  (define type (get-field type world))
  (define pros-list (get-field processed-config-list world))
  (define prev-state (cadr pros-list))
  (define cur-rule (getCurRule (cdr pros-list) type)) ; The current rule that the machine is in after prev is pressed
  (define rule (getCurRule (if (equal? type 'ndfa)
                               pros-list
                               (cdr pros-list))
                           type))
  (define pda-cur-rule (getCurRule pros-list type)) ;; The current rule that pda machine is in after prev is pressed. Only use this for PDA's


  ;; Based on the machine type certin things need to be updated:
  ;; - pda: stack pushes and pops, world processed and unprocessed lists
  ;; - tm: tape index, world processed and unprocessed lists
  ;; - dfa/ndfa: world processed and unprocessed lists
  (when (and (not (equal? 'tm type))
             (not (equal? 'tm-language-recognizer type)))
    (set-world-tape-decrease world rule cur-rule prev-state))

  (when (equal? type 'pda)
    (handle-push/pop-prev world cur-rule))

  (send world setCurRule cur-rule)
  (send world setCurState (determine-prev-state prev-state type))
  (send world setProcessedConfigList (cdr pros-list))
  (send world setUnprocessedConfigList (cons (car pros-list)
                                             (get-field unprocessed-config-list world)))
  (send world setScrollBarIndex (index-of
                                 (machine-rule-list (get-field machine world))
                                 cur-rule))
  (displayln "Here"))

;; goNext :: world -> ()
;; Handles all functionality around setting the world up for the next state transition
(define (goNext world)
  (match-let*
      ([type (get-field type world)]
       [machine (get-field machine world)]
       [unpros-list (get-field unprocessed-config-list world)]
       [pros-list (get-field processed-config-list world)]
       [`(,nextState ,transitions ...) unpros-list]
       [cur-rule (getCurRule (append (list nextState) pros-list) type)])


    ;; Based on the machine type certian things need to be updated:
    ;; - pda: stack pushes and pops, world processed and unprocessed lists, cur-state
    ;; - tm: tape index, world processed and unprocessed lists, cur-state
    ;; - dfa/ndfa: world processed and unprocessed lists, cur-state
    (begin
      (set-world-tape-increase world cur-rule)
      (when (eq? type 'pda)
        (handle-push/pop-next world cur-rule))
      (send world setCurRule (getCurRule (append
                                          (list nextState)
                                          pros-list)
                                         type))
      (send world setCurState (determine-cur-state nextState type))
      (send world setProcessedConfigList (append (list nextState) pros-list))
      (send world setUnprocessedConfigList transitions)
      (send world setScrollBarIndex (index-of
                                     (machine-rule-list machine)
                                     cur-rule)))))


;; set-world-tape-increase :: world -> rule -> number
;; Determine if the tape input should increase.
;; This does not need to be done for tm's or on an empty transition
(define (set-world-tape-increase world cur-rule)
  (define type (get-field type world))
  (define get-input (match type
                      ['pda (cadar cur-rule)]
                      ['tm 'NONE]
                      ['tm-language-recognizer 'NONE]
                      [else (cadr cur-rule)]))           
  (cond
    [(and (not (equal? 'tm type))
          (not (equal? 'tm-language-recognizer type))
          (equal? EMP get-input))
     TAPE-INDEX-BOTTOM]
    [else (send world add1TapePosition)]))

;; set-world-tape-decrease :: world -> rule -> rule -> state -> ()
;; determines if the tape needs to be decreased, if so then it decreases
(define (set-world-tape-decrease world rule cur-rule prev-state)
  (define type (get-field type world))
  ;; Returns the input that was consumed by the machien 
  (define (input-consumed?) 
    (match type
      ['pda (if (equal? (length (cadr prev-state))
                        (length (cadr (car (get-field processed-config-list world)))))
                EMP
                #t)]
      ['tm (error "Interanl error| buttonFuntions.rkt - showPrev")]
      ['tm-language-recognizer (error "Interanl error| buttonFuntions.rkt - showPrev")]
      [_ (cadr cur-rule)]))
  
  (define input (input-consumed?))
  (match type
    [(or 'dfa 'ndfa) #:when (and (not (equal? EMP (cadr rule)))
                                 (not (<= (get-field tape-position world) -1)))
                     (send world sub1TapePosition)]
    [(not 'tm 'tm-language-recognizer) #:when (equal? EMP input)
                                       (void)]
    [_ (unless (<= (get-field tape-position world) -1)
         (send world sub1TapePosition))]))
  

;; handle-push/pop-next :: world -> rule -> ()
;; handles the logic for pda stack pushs and pop for 'goNext'
(define (handle-push/pop-next world cur-rule)
  (define (handle-pop stack)
    (define pop-list (caddar cur-rule))
    (cond
      [(symbol? pop-list) stack] ;; e is the element so nothing to pop
      [else
       (drop stack (length pop-list))]))
  
  (define (handle-push stack)
    (define push-list (cadadr cur-rule))
    (cond
      [(symbol? push-list) void] ;; e is the element so nothing to push
      [else
       (append push-list stack)]))
  (send
   world
   updateWorldStack
   ((compose1 handle-push handle-pop) (get-field stack world))))


;; handle-push/pop-prev :: world -> rule -> ()
;; handles the logic for pda stack pushs and pop for 'goPrev'
(define (handle-push/pop-prev world cur-rule)
  (define (handle-pop stack)
    (define pop-list (cadadr cur-rule))
    (cond
      [(symbol? pop-list) stack] ;; e is the element so nothing to pop
      [else
       (drop stack (length pop-list))]))
  
  (define (handle-push stack)
    (define push-list (caddar cur-rule))
    (cond
      [(symbol? push-list) void] ;; e is the element so nothing to push
      [else
       (append push-list stack)]))
  (send
   world
   updateWorldStack
   ((compose1 handle-pop handle-push) (get-field stack world))))


;; determine-cur-state :: transition -> type
;; Determins the current state that the machine is in
(define (determine-cur-state state type)
  (match type
    ['pda (car state)]
    ['tm (car state)]
    ['tm-language-recognizer (car state)]
    [_ (car (cdr state))]))


;; determine-prev-state :: state -> type -> state
;; Determins what the previous state of the machine was
(define (determine-prev-state state type)
  (match type
    ['pda (car state)]
    ['tm (car state)]
    ['tm-language-recognizer (car state)]
    [_ (car (cdr state))]))
  
  
     
   
