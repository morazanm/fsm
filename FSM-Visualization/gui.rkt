#lang racket/gui
(require (for-syntax syntax/parse) "./structs/state.rkt" "./globals.rkt"
         "./structs/machine.rkt" "./structs/posn.rkt") 
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

(define-syntax (line-display stx)
  (syntax-parse stx
    [(_ [value:id ...] dest-port)
     #`(begin
         (begin
           (display (quote value) dest-port)
           (display ": " dest-port)
           (display value dest-port)
           (display "\n" dest-port))...)]))

(define world%
  (class* object% (writable<%>)
    (init-field machine
                [tape-position 0]
                [mode 'idle] ;; Valid modes are: idle, active
                [type (machine-type machine)]
                [cur-rule CURRENT-RULE]
                [cur-state CURRENT-STATE]
                [processed-config-list '()]
                [unporcessed-config-list '()]
                [scroll-bar-index 0])
   
    
    (define/public (custom-display dest-port)
      (line-display [tape-position
                     machine type
                     mode
                     cur-rule
                     cur-state
                     processed-config-list
                     unporcessed-config-list
                     scroll-bar-index]
                    dest-port))

    ;; get-machine-alpha-list :: listOfSymbol
    (define/public (get-machine-alpha-list)
      (machine-alpha-list machine))

    ;; addAlpha :: symbol -> ()
    (define/public (addAlpha value)
      (set-machine-alpha-list! machine
                               (sort (remove-duplicates (cons value (machine-alpha-list machine)))
                                     symbol<?)))

    ;; addAlpha :: symbol -> ()
    (define/public (removeAlpha value)
      (set-machine-alpha-list! machine
                               (filter (lambda (v) (not (eq? v value)))
                                       (machine-alpha-list machine))))

    ;; addState :: symbol -> bool
    (define/public (addState value)
      (define isDuplicate (isInStateList value))
      (unless isDuplicate
        (set-machine-state-list!
         machine
         (cons (fsm-state value (true-function) (posn 0 0))
               (machine-state-list machine))))
      isDuplicate)

    ;; removeState :: symbol -> bool
    (define/public (removeState value)
      (define exists (isInStateList value))
      (when exists
        (set-machine-state-list!
         machine
         (remove
          value
          (machine-state-list machine)
          (lambda (target v2) (eq? target (fsm-state-name v2))))))
      exists)

    ;; addStart :: symbol -> ()
    (define/public (addStart value)
      (addState value) ;; Update the state list
      (set-machine-start-state! machine value)) ;; Update the start state

    ;; removeStart :: symbol -> bool
    (define/public (removeStart value)
      (define exists (removeState value))
      (when exists
        (set-machine-start-state! machine '()))
      exists)

    ;; addEnd :: symbol -> bool
    (define/public (addEnd value)
      (define exists (member value (machine-final-state-list machine) eq?))
      (unless exists
        (addState value)
        (set-machine-final-state-list!
         machine
         (cons value (machine-final-state-list machine))))
      exists)

    ;; removeEnd :: symbol -> bool
    (define/public (removeEnd value)
      (define exists (member value (machine-final-state-list machine) eq?))
      (when exists
        (set-machine-final-state-list!
         machine
         (filter (lambda (v) (not (eq? value v))
                   (machine-final-state-list machine)))))
      exists)

    ;; addRule :: rule -> bool
    (define/public (addRule rule)
      (define exists (member rule (machine-rule-list machine) eqRule?))
      (unless exists
        (set-machine-rule-list!
         machine
         (cons rule (machine-rule-list machine))))
      exists)

    ;; addRule :: rule -> bool
    (define/public (removeRule rule)
      (define exists (member rule (machine-rule-list machine) eqRule?))
      (when exists
        (set-machine-rule-list!
         machine
         (remove
          rule
          (machine-rule-list machine)
          eqRule?)))
      exists)

    ;; addTape :: listOfSymbol -> bool
    ;; To be a valid tape value it must exist in the alphabet, if not return false 
    (define/public (addTape values)
      (define allValid (empty? (filter (lambda (v)
                                         (not (member v (machine-alpha-list machine) eq?)))
                                       values)))
      (when allValid                     
        (set-machine-sigma-list!
         machine
         (append (machine-sigma-list machine) values)))
      allValid)

    ;; clearTape :: listOfSymbol -> ()
    (define/public (clearTape)
      (set-machine-sigma-list!
       machine
       '()))

    ;; setMode :: symbol('idle | 'active) -> ()
    (define/public (setMode value)
      (set! mode value))
    
    (define/public (custom-write dest-port)
      (write "Currently Unsupported" dest-port))

    ;; isInStateList :: symbol -> bool
    (define/private (isInStateList value)
      (not (eq? #f
                (member value
                        (machine-state-list machine)
                        (lambda (target v2) (eq? target (fsm-state-name v2)))))))

    (define/match (eqRule? target actual)
      [(`(,s1 ,r1 ,f1) `(,s2 ,r2 ,f2)) ;; dfa/ndfa 
       (and (eq? s1 s2)
            (eq? r1 r2)
            (eq? f1 f2))])

    ;; true-function :: lambda
    (define/private (true-function)
      (match type
        [(or 'dfa 'ndfa) TRUE-FUNCTION]
        ['pda PDA-TRUE-FUNCTION]
        [_ TM-TRUE-FUNCTION]))
    
    (super-new)))


;; delete-children :: Object -> ()
;; This function will remove all children on a given object
(define (delete-children obj)
  (for ([child (send obj get-children)])
    (send obj delete-child child)))

;; kick-off-gui :: world -> GUI
(require "./structs/world.rkt" "./structs/machine.rkt")
(define (kick-off-gui machine)
  (define world (new world% [machine machine]))
  (define (event-dispatcher event value)
    (define isActive (eq? 'active (get-field mode world)))
    (define (setIdle)
      (send world setMode 'idle)
      ;;TODO(jschappel): trigger redraw
      )
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
                   (display world)
                   )]
      ['removeStart (begin
                      (define needsRedraw (send world removeStart value))
                      (when needsRedraw
                        (setIdle))
                      (display world))]
      ['addState (begin
                   (define needsRedraw (send world addState value))
                   (when needsRedraw
                     (setIdle))
                   (display world))]
      ['removeState (begin
                      (define needsRedraw (send world removeState value))
                      (when needsRedraw
                        (setIdle))
                      (display world))]
      ['addEnd (begin
                 (define needsRedraw (send world addEnd value))
                 (when needsRedraw
                   (setIdle))
                 (display world))]      
      ['removeEnd (begin
                    (define needsRedraw (send world removeEnd value))
                    (when needsRedraw
                      (setIdle))
                    (display world))]
      ['addRule (begin
                  (define needsRedraw (send world addRule value))
                  (unless needsRedraw
                    (setIdle)
                    (delete-children rule-display)
                    (render-rule-list)))]
      ['removeRule (begin
                     (define needsRedraw (send world removeRule value))
                     (when needsRedraw
                       (setIdle)
                       (delete-children rule-display)
                       (render-rule-list)))]
      ['clearTape (begin
                    (send world clearTape)
                    (setIdle)
                    (delete-children tape-display)
                    (render-tape-list))]
      ['addTape (begin
                  (define allValid (send world addTape value))
                  (if (not allValid)
                      (begin
                        (displayln "Here:")
                        (send tape-error-win show #t))
                      (begin
                        (setIdle)
                        (delete-children tape-display)
                        (render-tape-list))))]
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

  
  (new button% [parent control-panel] [label "Run"] [min-width 120] [min-height 10])


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

  (define tmp (read-bitmap "/home/joshua/Github/fsm/aStar.png"))
  (define machine-view (new canvas%
                            [parent machine-panel]
                            [paint-callback
                             (lambda (canvas dc)
                               ;;(displayln (send machine-panel get-height))
                               ;;(displayln (send machine-panel get-width))
                               (send dc draw-bitmap tmp
                                     (- (/ (send machine-panel get-width) 2)
                                        (/ (send tmp get-width) 2))
                                     (-
                                      (/ (send machine-panel get-height) 2)
                                      (/ (send tmp get-height) 2)))
                               (make-screen-bitmap 100 100))]))


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
                   (displayln "here")
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
  (send frame show #t)
  )