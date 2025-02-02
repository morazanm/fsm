#lang racket/base

(require "../viz-lib/viz-macros.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/viz-state.rkt"
         "../../fsm-gviz/private/lib.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         racket/list
         racket/local
         racket/function)

(provide init-viz)
(define FONT-SIZE 20)
(define TAPE-SIZE 42)
(define HEDGE-COLOR 'violet)
(define YIELD-COLOR 'skyblue)
(define PERCENT-BORDER-GAP 0.9)
(define HEIGHT-BUFFER 20)
(define LETTER-KEY-WIDTH-BUFFER 20)
(define ARROW-KEY-WIDTH-BUFFER 40)
(define INS-TOOLS-BUFFER 30)
(define EXTRA-HEIGHT-FROM-CURSOR 4)
(define NODE-SIZE 50)

(define DEFAULT-ZOOM 1)
(define DEFAULT-ZOOM-FLOOR 1)
(define DEFAULT-ZOOM-CAP 2)
(define ZOOM-INCREASE 1.1)
(define ZOOM-DECREASE (/ 1 ZOOM-INCREASE))

(define TICK-RATE 1/60)
(define CLICK-BUFFER-SECONDS (/ (/ 1 TICK-RATE) 2))

(struct imsg-state
  (rules yield input-word word-img-offset word-img-offset-cap scroll-accum broken-invariants))

;; Listof Symbol natnum -> Image
;; Returns an image of a tape of symbols, capable of sliding when its start-index is varied
(define (make-tape-img tape start-index)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let ([letter-imgs
         (build-list
          TAPE-SIZE
          (λ (i)
            (if (< (+ start-index i) (length tape))
                (overlay (text (symbol->string (list-ref tape (+ start-index i))) 24 'black)
                         (overlay (square 25 'solid 'white) (square (add1 25) 'solid 'white)))
                (overlay (square 25 'solid 'white) (square (add1 25) 'solid 'white)))))])
    (make-tape-img letter-imgs start-index)))

(define TAPE-IMG-HEIGHT (image-height (make-tape-img (list 'a) 0)))

(define DEFAULT-RULE-YIELD
  (let ([DREV (let ([drev-text (text "Deriving: " FONT-SIZE 'black)])
                (overlay drev-text
                         (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [YIELD (let ([yield-text (text "Current Yield: " FONT-SIZE 'black)])
                 (overlay yield-text
                          (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [RULE-USED (text "The rule used: " FONT-SIZE 'black)])
    (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
            (above/align "right" RULE-USED DREV YIELD))))

(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER FONT-SIZE
                                               (list (list ARROW-UP-KEY "Restart")
                                                     (list ARROW-RIGHT-KEY "Forward")
                                                     (list ARROW-LEFT-KEY "Backward")
                                                     (list ARROW-DOWN-KEY "Finish")
                                                     (list CURSOR "Hold to drag")
                                                     (list W-KEY "Zoom in")
                                                     (list S-KEY "Zoom out")
                                                     (list R-KEY "Min zoom")
                                                     (list E-KEY "Mid zoom")
                                                     (list F-KEY "Max zoom")
                                                     (list A-KEY "Word start")
                                                     (list D-KEY "Word end"))))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height DEFAULT-RULE-YIELD)
                          (image-height E-SCENE-TOOLS)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (let ([DREV (let ([drev-text (text "Deriving: " FONT-SIZE 'black)])
                (overlay drev-text
                         (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [YIELD (let ([yield-text (text "Current Yield: " FONT-SIZE 'black)])
                 (overlay yield-text
                          (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white)))]
        [RULE-USED (text "The rule used: " FONT-SIZE 'black)])
    (bounding-limits (+ (image-width (rectangle 1 (* 2 FONT-SIZE) "solid" 'white))
                        (image-width (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                             (above/align "right" RULE-USED DREV YIELD))))
                     (* E-SCENE-WIDTH 0.9)
                     (+ E-SCENE-HEIGHT)
                     (+ E-SCENE-HEIGHT
                        (image-height (beside (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
                                              (above/align "right" RULE-USED DREV YIELD)))))))

(create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT (image-width E-SCENE-TOOLS) RULE-YIELD-DIMS FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
                        ((ARROW-UP-KEY "Restart")
                         (ARROW-RIGHT-KEY "Forward")
                         (ARROW-LEFT-KEY "Backward")
                         (ARROW-DOWN-KEY "Finish")
                         (CURSOR "Hold to drag")
                         (W-KEY "Zoom in")
                         (S-KEY "Zoom out")
                         (R-KEY "Min zoom")
                         (E-KEY "Mid zoom")
                         (F-KEY "Max zoom")
                         (A-KEY "Word start")
                         (D-KEY "Word end")))

;; viz-state -> Image
;; Returns a image containing all the information regarding what is being derived and what the current yield is
(define (create-instructions a-imsgs)
  (beside
   (rectangle 1 (* 2 FONT-SIZE) "solid" 'white)
   (local
     [(define DREV
        (let ([drev-text (text "Deriving: " FONT-SIZE 'black)])
          (overlay drev-text (rectangle (image-width drev-text) TAPE-IMG-HEIGHT 'solid 'white))))
      (define YIELD
        (let ([yield-text (text "Current Yield: " FONT-SIZE 'black)])
          (overlay yield-text (rectangle (image-width yield-text) TAPE-IMG-HEIGHT 'solid 'white))))
      (define INPUT-WORD
        (make-tape-img (imsg-state-input-word a-imsgs)
                       (if (> (length (imsg-state-input-word a-imsgs)) TAPE-SIZE)
                           (imsg-state-word-img-offset a-imsgs)
                           0)))
      (define (find-arrow used-rule acc)
        (cond [(> acc (sub1 (string-length used-rule))) 1]
              [(equal? (string-ref used-rule acc) #\→) acc]
              [(find-arrow used-rule (add1 acc))]))
      (define YIELD-WORD
        (let ([normalized-p-yield (if (list? (zipper-current (imsg-state-yield a-imsgs)))
                                      (zipper-current (imsg-state-yield a-imsgs))
                                      (list (zipper-current (imsg-state-yield a-imsgs))))])
          (make-tape-img
           normalized-p-yield
           (if (> (length normalized-p-yield) TAPE-SIZE) (imsg-state-word-img-offset a-imsgs) 0))))
      (define RULE-USED
        (if (equal? "" (zipper-current (imsg-state-rules a-imsgs)))
            ;; Use white so its invisible, makes it so the words dont shift (using an empty string would make the words shift)
            (text "The rule used: " FONT-SIZE 'white)
            (text "The rule used: " FONT-SIZE 'black)))
      (define RULE-USED-WORD
        (let ([arrow-place (find-arrow (zipper-current (imsg-state-rules a-imsgs)) 0)])
          (if (equal? "" (zipper-current (imsg-state-rules a-imsgs)))
              (text "" FONT-SIZE 'white)
              (beside (text (format "~a" (substring (zipper-current (imsg-state-rules a-imsgs)) 0 (sub1 arrow-place)))
                            FONT-SIZE
                            YIELD-COLOR)
                      (text (format " ~a" (substring (zipper-current (imsg-state-rules a-imsgs)) arrow-place))
                            FONT-SIZE
                            HEDGE-COLOR)))))
      ;(define INVARIANT-MSG (text "Invariant: " FONT-SIZE 'black))
      (define INVARIANT-STATE
        (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
            (text "All invariants hold." FONT-SIZE 'white)
            (if (empty? (zipper-current (imsg-state-broken-invariants a-imsgs)))
                (text "All invariants hold." FONT-SIZE 'black)
                (text (format "~a invariant does not hold."
                              (first (zipper-current (imsg-state-broken-invariants a-imsgs))))
                      FONT-SIZE
                      'black))))
      (define spacer
        (rectangle (- E-SCENE-WIDTH
                      (image-width RULE-USED)
                      (image-width RULE-USED-WORD)
                      ;(image-width INVARIANT-MSG)
                      (image-width INVARIANT-STATE)
                      10)
                   (image-height RULE-USED)
                   'solid
                   'white))
      (define RULE-YIELD-DREV-LABELS (above/align "right" RULE-USED DREV YIELD))
      (define WORDS
        (above/align "left"
                     (beside RULE-USED-WORD
                             spacer
                             ;INVARIANT-MSG
                             INVARIANT-STATE)
                     INPUT-WORD
                     YIELD-WORD))]
     (beside RULE-YIELD-DREV-LABELS WORDS))))

;; viz-state -> img
;; Returns the the instructions and e-scene-tools images combined into one
(define (create-instructions-and-tools a-imsgs)
  (above (create-instructions a-imsgs) (square INS-TOOLS-BUFFER 'solid 'white) E-SCENE-TOOLS))

;; viz-state -> viz-state
;; Updates the informative message so it displays the next rule yield
(define (right-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy informative-messages
                   (viz-state-informative-messages a-vs)
                   [component-state
                    (if (zipper-at-end? (imsg-state-rules a-imsgs))
                        a-imsgs
                        (struct-copy imsg-state
                                     a-imsgs
                                     [rules
                                      (if (zipper-at-end? (imsg-state-rules a-imsgs))
                                          (imsg-state-rules a-imsgs)
                                          (zipper-next (imsg-state-rules a-imsgs)))]
                                     [yield
                                      (if (zipper-at-end? (imsg-state-yield a-imsgs))
                                          (imsg-state-yield a-imsgs)
                                          (zipper-next (imsg-state-yield a-imsgs)))]
                                     [broken-invariants
                                      (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                                          'NO-INV
                                          (if (zipper-at-end? (imsg-state-broken-invariants a-imsgs))
                                              (imsg-state-broken-invariants a-imsgs)
                                              (zipper-next (imsg-state-broken-invariants
                                                            a-imsgs))))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the beginning state
(define (up-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy viz-state
                 a-vs
                 [informative-messages
                  (struct-copy informative-messages
                               (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state
                                                 a-imsgs
                                                 [rules (zipper-to-begin (imsg-state-rules a-imsgs))]
                                                 [yield (zipper-to-begin (imsg-state-yield a-imsgs))]
                                                 [broken-invariants
                                                  (if (equal? 'NO-INV
                                                              (imsg-state-broken-invariants a-imsgs))
                                                      'NO-INV
                                                      (zipper-to-begin (imsg-state-broken-invariants
                                                                        a-imsgs)))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the previous rule yield
(define (left-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy
       informative-messages
       (viz-state-informative-messages a-vs)
       [component-state
        (if (zipper-at-begin? (imsg-state-rules a-imsgs))
            a-imsgs
            (struct-copy imsg-state
                         a-imsgs
                         [yield
                          (if (zipper-at-begin? (imsg-state-yield a-imsgs))
                              (imsg-state-yield a-imsgs)
                              (zipper-prev (imsg-state-yield a-imsgs)))]
                         [rules
                          (if (zipper-at-begin? (imsg-state-rules a-imsgs))
                              (imsg-state-rules a-imsgs)
                              (zipper-prev (imsg-state-rules a-imsgs)))]
                         [broken-invariants
                          (if (equal? 'NO-INV (imsg-state-broken-invariants a-imsgs))
                              'NO-INV
                              (if (zipper-at-begin? (imsg-state-broken-invariants a-imsgs))
                                  (imsg-state-broken-invariants a-imsgs)
                                  (zipper-prev (imsg-state-broken-invariants a-imsgs))))]))])])))

;; viz-state -> viz-state
;; Updates the informative message so it displays the ending rule yield
(define (down-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy viz-state
                 a-vs
                 [informative-messages
                  (struct-copy informative-messages
                               (viz-state-informative-messages a-vs)
                               [component-state
                                (if (zipper-at-end? (imsg-state-rules a-imsgs))
                                    a-imsgs
                                    (struct-copy imsg-state
                                                 a-imsgs
                                                 [rules (zipper-to-end (imsg-state-rules a-imsgs))]
                                                 [yield (zipper-to-end (imsg-state-yield a-imsgs))]
                                                 [broken-invariants
                                                  (if (equal? 'NO-INV
                                                              (imsg-state-broken-invariants a-imsgs))
                                                      'NO-INV
                                                      (zipper-to-end (imsg-state-broken-invariants
                                                                      a-imsgs)))]))])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the beginning of their current words
(define (a-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy
     viz-state
     a-vs
     [informative-messages
      (struct-copy informative-messages
                   (viz-state-informative-messages a-vs)
                   [component-state
                    (struct-copy imsg-state a-imsgs [word-img-offset 0] [scroll-accum 0])])])))

;; viz-state -> viz-state
;; Purpose: Moves the deriving and current yield to the end of their current words
(define (d-key-pressed a-vs)
  (let ([a-imsgs (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (struct-copy viz-state
                 a-vs
                 [informative-messages
                  (struct-copy informative-messages
                               (viz-state-informative-messages a-vs)
                               [component-state
                                (struct-copy imsg-state
                                             a-imsgs
                                             [scroll-accum 0]
                                             [word-img-offset
                                              (imsg-state-word-img-offset-cap a-imsgs)])])])))

;; node -> img
;; Creates the first img to be displayed since this is always a special case
(define (create-first-img node)
  (lambda ()
    (graph->bitmap
     (add-node (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
               node
               #:atb
               (hash 'color 'black 'shape 'circle 'label node 'fontcolor 'black 'font "Sans")))))

(define viz-go-next
  (go-next E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))

(define viz-go-prev
  (go-prev E-SCENE-WIDTH
           E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))

(define viz-go-to-begin
  (go-to-begin E-SCENE-WIDTH
               E-SCENE-HEIGHT
               NODE-SIZE
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM-FLOOR
               PERCENT-BORDER-GAP))

(define viz-go-to-end
  (go-to-end E-SCENE-WIDTH
             E-SCENE-HEIGHT
             NODE-SIZE
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             PERCENT-BORDER-GAP))

(define viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))

(define viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))

(define (init-viz grammar
                  word
                  w-der
                  rules
                  graphs
                  broken-invariants
                  #:cpu-cores [cpu-cores #f]
                  #:special-graphs? [special-graphs? #f]
                  #:rank-node-lst [rank-node-lst '()])
  (let ()
    (run-viz
     graphs
     (create-first-img (first (first w-der)))
     (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2))
     DEFAULT-ZOOM
     DEFAULT-ZOOM-CAP
     DEFAULT-ZOOM-FLOOR
     (informative-messages create-instructions
                           (imsg-state (list->zipper rules)
                                       (list->zipper w-der)
                                       word
                                       0
                                       (let ([offset-cap (- (length word) TAPE-SIZE)])
                                         (if (> 0 offset-cap) 0 offset-cap))
                                       0
                                       broken-invariants)
                           RULE-YIELD-DIMS)
     (instructions-graphic E-SCENE-TOOLS
                           (bounding-limits 0
                                            (image-width E-SCENE-TOOLS)
                                            (+ 
                                             E-SCENE-HEIGHT
                                             (bounding-limits-height RULE-YIELD-DIMS)
                                             INS-TOOLS-BUFFER)
                                            (+ 
                                             E-SCENE-HEIGHT
                                             (bounding-limits-height RULE-YIELD-DIMS)
                                             INS-TOOLS-BUFFER
                                             (image-height ARROW-UP-KEY))))
     (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
     (create-viz-process-key ["right" viz-go-next right-key-pressed]
                             ["left" viz-go-prev left-key-pressed]
                             [ "up" viz-go-to-begin up-key-pressed]
                             ["down" viz-go-to-end down-key-pressed]
                             ["w" viz-zoom-in identity]
                             ["s" viz-zoom-out identity]
                             ["r" viz-max-zoom-out identity]
                             ["f" viz-max-zoom-in identity]
                             ["e" viz-reset-zoom identity]
                             ["a" identity a-key-pressed]
                             ["d" identity d-key-pressed]
                             ["wheel-down" viz-zoom-in identity]
                             ["wheel-up" viz-zoom-out identity])
     (create-viz-process-tick
      E-SCENE-BOUNDING-LIMITS
      NODE-SIZE
      E-SCENE-WIDTH
      E-SCENE-HEIGHT
      CLICK-BUFFER-SECONDS
      ([RULE-YIELD-DIMS
        (lambda (a-imsgs x-diff y-diff)
          (let ([new-scroll-accum (+ (imsg-state-scroll-accum a-imsgs) x-diff)])
            (cond
              [(and (>= (imsg-state-word-img-offset-cap a-imsgs)
                        (imsg-state-word-img-offset a-imsgs))
                    (<= (quotient (+ (imsg-state-scroll-accum a-imsgs) x-diff) 25) -1))
               (struct-copy imsg-state
                            a-imsgs
                            [word-img-offset (+ (imsg-state-word-img-offset a-imsgs) 1)]
                            [scroll-accum 0])]
              [(and (> (imsg-state-word-img-offset a-imsgs) 0)
                    (>= (quotient (+ (imsg-state-scroll-accum a-imsgs) x-diff) 25) 1))
               (struct-copy imsg-state
                            a-imsgs
                            [word-img-offset (- (imsg-state-word-img-offset a-imsgs) 1)]
                            [scroll-accum 0])]
              [else
               (struct-copy imsg-state
                            a-imsgs
                            [scroll-accum
                             (+ (imsg-state-scroll-accum a-imsgs) x-diff)])])))])
      ([ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
       [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
       [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
       [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
       [W-KEY-DIMS viz-zoom-in identity]
       [S-KEY-DIMS viz-zoom-out identity]
       [R-KEY-DIMS viz-max-zoom-out identity]
       [E-KEY-DIMS viz-reset-zoom identity]
       [F-KEY-DIMS viz-max-zoom-in identity]
       [A-KEY-DIMS identity a-key-pressed]
       [D-KEY-DIMS identity d-key-pressed]))
     'grammar-viz
     #:cpu-cores cpu-cores
     #:special-graphs? special-graphs?
     #:rank-node-lst rank-node-lst)))