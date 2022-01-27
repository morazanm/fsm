#lang racket/gui
(provide kick-off-gui)






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

(define GLOBAL-WORLD '())


;; kick-off-gui :: world -> GUI
(require "./structs/world.rkt" "./structs/machine.rkt")
(define (kick-off-gui world)
  (set! GLOBAL-WORLD world)
  (define type (world2-machine-type world))
  (displayln type)



  (define WELCOME-MSG  "FSM Visualization Tool:  ")

  ; Make a frame by instantiating the frame% class
  (define frame (new frame%
                     [width 1400]
                     [height 700]
                     [style '(fullscreen-button)]
                     [label (string-append WELCOME-MSG "Dfa/Ndfa")]))

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
  ;[alignment '(center center)]))


  ;; ----------------------------- LEFT SIDE BELOW -----------------------------
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
  (new text-field% [parent tape-panel] [label ""])
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel-tape (new horizontal-panel%
                          [parent tape-panel]
                          [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel-tape] [label "Add"])
  (new button% [parent panel-tape] [label "Remove"])
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
    (for ([alpha (machine-alpha-list (world2-fsm-machine GLOBAL-WORLD))])
      (new message% [parent inner-alpha-display] [enabled #t]
           [label (symbol->string alpha)])))
  (render-alpha-list );;(machine-alpha-list (world2-fsm-machine world)));; TODO:(Jschappel) Maybe move this?


  ;; ----------------------------- CENTER BELOW -----------------------------
  (define myfont (make-font #:size 16
                            #:family 'swiss))

  (define center-side (new vertical-panel%
                           [parent top-level]
                           [style (list 'border)]
                           [min-width 700]
                           [min-height 300]
                           [alignment '(center center)]))

  (define transitions (new horizontal-panel%
                           [parent center-side]
                           [min-height 60]
                           [style '(hscroll border)]
                           [spacing 10]
                           [stretchable-height #f]
                           [alignment '(center center)]))

  (for ([i (range 97 123)])
    (new message% [parent transitions] [font myfont] [enabled #t]
         [label (string (integer->char i))]))



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


  (define rules (new horizontal-panel%
                     [parent center-side]
                     [min-height 60]
                     [style '(hscroll border)]
                     [spacing 10]
                     [border 10]
                     [stretchable-height #f]
                     [alignment '(center center)]))

  (for ([i (range 97 123)])
    (let* [(s (string (integer->char i)))
           (rule (string-append "(" s " " s " "s ")"))]
      (if (eq? i 100)
          (new message% [parent rules] [color "red"] [font myfont] [enabled #t]
               [label rule]) 
          (new message% [parent rules] [font myfont] [enabled #t]
               [label rule]))))




  ;; ----------------------------- RIGHT SIDE BELOW -----------------------------
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
  (new text-field% [parent state-panel] [label ""])
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel (new horizontal-panel% [parent state-panel]
                     [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel] [label "Add"])
  (new button% [parent panel] [label "Remove"])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))



  (define alpha-panel (new vertical-panel%
                           [parent right-side]
                           [style (list 'border)]
                           [border 10]
                           [alignment '(center center)]))
  (new message% [parent alpha-panel] [label "Alpha Options"])
  (new text-field%
       [parent alpha-panel]
       [label ""])
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel4 (new horizontal-panel% [parent alpha-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel4] [label "Add"]
       [callback (lambda (button event)
                   ;;TODO:(jschappel) Fix this to call the dispatcher
                   (render-alpha-list))])
  (new button% [parent panel4] [label "Remove"])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))



  (define start-panel (new vertical-panel%
                           [parent right-side]
                           [style (list 'border)]
                           [border 10]
                           [alignment '(center center)]))
  (new message% [parent start-panel] [label "Start State"])
  (new text-field% [parent start-panel] [label ""])
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel2 (new horizontal-panel% [parent start-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel2] [label "Add"])
  (new button% [parent panel2] [label "Remove"])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))



  (define end-panel (new vertical-panel%
                         [parent right-side]
                         [style (list 'border)]
                         [border 10]
                         [alignment '(center center)]))
  (new message% [parent end-panel] [label "End State"])
  (new text-field% [parent end-panel] [label ""])
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define panel3 (new horizontal-panel% [parent end-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel3] [label "Add"])
  (new button% [parent panel3] [label "Remove"])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))



  (define rule-panel (new vertical-panel%
                          [parent right-side]
                          [style (list 'border)]
                          [border 10]
                          [alignment '(center center)]))
  (new message% [parent rule-panel] [label "Add Rule"])
  (define panel5 (new horizontal-panel% [parent rule-panel] [alignment '(center center)]))
  (new text-field% [parent panel5] [label ""] [stretchable-height #f])
  (new text-field% [parent panel5] [label ""] [stretchable-height #f])
  (new text-field% [parent panel5] [label ""] [stretchable-height #f])
  (define panel6 (new horizontal-panel% [parent rule-panel]
                      [alignment '(center center)]))
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button% [parent panel6] [label "Add"])
  (new button% [parent panel6] [label "Remove"])
  (when (system-position-ok-before-cancel?)
    (send panel change-children reverse))
  
  (send frame show #t))