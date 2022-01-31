#lang racket/gui
(require (for-syntax syntax/parse)
         2htdp/image
         "./structs/state.rkt"
         "./globals.rkt"
         "./structs/machine.rkt"
         "./structs/posn.rkt"
         "./checkMachine.rkt"
         "./draw.rkt"
         "./structs/world.rkt")

(provide kick-off-gui)
(define MAX-ALPHABET 14)




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


;; delete-children :: Object -> ()
;; This function will remove all children on a given object
(define (delete-children obj)
  (for ([child (send obj get-children)])
    (send obj delete-child child)))

;; kick-off-gui :: world -> GUI
(define (kick-off-gui machine)
  (define world (new world% [machine machine]))
  (define (event-dispatcher event value)
    (define isActive (eq? 'active (get-field mode world)))
    (define (setIdle)
      (send world setMode 'idle)
      (remake-image 'control))
    (define (setActive)
      (send world setMode 'active)
      ;; re-render necessary fields
      (delete-children inner-alpha-display) (render-alpha-list)
      (delete-children rule-display) (render-rule-list)
      (delete-children tape-display) (render-tape-list)
      (remake-image 'control))
    
    (match event
      ['addAlpha (begin
                   (send world addAlpha value)
                   (delete-children inner-alpha-display)
                   (when isActive
                     (setIdle))
                   (render-alpha-list))]
      ['removeAlpha (begin
                      (send world removeAlpha value)
                      (delete-children inner-alpha-display)
                      (when isActive
                        (setIdle))
                      (render-alpha-list))]
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
                  (unless needsRedraw
                    (when isActive
                      (setIdle))
                    (delete-children rule-display)
                    (render-rule-list)))]
      ['removeRule (begin
                     (define needsRedraw (send world removeRule value))
                     (when needsRedraw
                       (when isActive
                         (setIdle))
                       (setIdle)
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
                             (send world setUnprocessedList unpros)
                             (send world setProcessedList pros)
                             (setActive)
                             (send machine-success-win show #t)))))]
      [_ (error (format "Invalid event to dispatch on '~s'" (symbol->string event)))]))

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
  (define dfa/ndfa-menu-item (new menu-item%
                                  [label "Dfa/Ndfa"]
                                  [parent machine-menu]
                                  [callback (lambda (btn event) (send frame set-label (string-append WELCOME-MSG "Dfa/Ndfa")))]))
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
    (send panel change-children reverse))
  

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
  (new button% [parent fwd-rev-panel] [label "🠈"])
  (new button% [parent fwd-rev-panel] [label "🠊"])

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
    (for ([input (machine-sigma-list (get-field machine world))])
      (new message% [parent tape-display] [font myfont] [enabled #t]
           [label (symbol->string input)])))



  (define machine-panel (new panel%
                             [min-height 180]
                             [parent center-side]))


  (define firstTime? #t)
  (define image null)
  ;; remake-image :: symbol(graph | control -> path

  (define machine-view (new canvas%
                            [parent machine-panel]
                            [paint-callback
                             (lambda (canvas dc)
                               (when firstTime?
                                 (set! firstTime? #f)
                                 (remake-image 'control))
                               (send dc draw-bitmap image
                                     (- (/ (send machine-panel get-width) 2)
                                        (/ (send image get-width)  2))
                                     (-
                                      (/ (send machine-panel get-height) 2)
                                      (/ (send image get-height) 2)))
                               (make-screen-bitmap 100 100))]))

  (define (remake-image mode)
    (define-values (width height) (send machine-view get-scaled-client-size))
    (begin
      (match mode
        ['graph (displayln "TODO")]
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
      (begin
        (define o (open-output-string))
        (write i o)
        (new message% [parent rule-display] [font myfont] [enabled #t]
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

  ;; TODO(jschappel): what does this do agai?!?!?!?!?!
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))


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
    (send panel change-children reverse))


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
    (send panel change-children reverse))


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
    (send panel change-children reverse))

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
  (send frame show #t)
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
                                        [label "The machine was successfully built. Press Next and Prev to show th emachines transitions"]))
  (new button% [parent machine-success-win-panel]
       [label "Continue"]
       [callback (lambda (button event)
                   (send machine-success-win show #f))])
  (send frame show #t)
  )