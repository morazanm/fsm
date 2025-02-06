#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "default-informative-messages.rkt"
         "../viz-lib/zipper.rkt"
         "david-imsg-state.rkt"
         "../viz-lib/viz-constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/misc.rkt")

(define FONT-SIZE 18)
(provide (all-defined-out))
#|
A computation is a structure: (make-computation LoC LoR LoT visited)
LoC is a (listof configuration)
LoR is a (listof rule)
visited is a (listof configuration)
|#
(struct computation (LoC LoR visited) #:transparent)

(define HELD-INV-COLOR 'chartreuse4)
(define BRKN-INV-COLOR 'red2)
(define GRAPHVIZ-CUTOFF-GOLD 'darkgoldenrod2)
(define SM-VIZ-FONT-SIZE 18)
(define INS-TOOLS-BUFFER 30)

(define AB*B*UAB*
  (make-unchecked-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

(define a* (make-unchecked-ndpda '(K H)
                                 '(a b)
                                 '(a)
                                 'K
                                 '(H)
                                 `(((K ,EMP ,EMP)(H ,EMP))
                                   ((H a ,EMP)(H ,EMP)))))

(define EVEN-AS-&-BS (make-unchecked-tm '(K H I B S)
                          '(a b)
                          `(((K ,BLANK) (S ,BLANK))
                            ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                            ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                          'K
                          '(S)
                          'S))

(define DUMMY-RULE (list (list EMP EMP EMP) (list EMP EMP)))

(define INFORMATIVE-MSG-HEIGHT 50)


(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER SM-VIZ-FONT-SIZE
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
                                                         (list D-KEY "Word end")
                                                         (list J-KEY "Prv not inv")
                                                         (list L-KEY "Nxt not inv"))))

(define ndfa-info-img (ndfa-create-draw-informative-message (imsg-state AB*B*UAB*
              '(a b b)
              '()
              '()
              0
              (list->zipper '(((S (a b b)) (K (a b b)) (B (b b)) (K (b)) (H (b)) (H ())) ((S (a b b)) (C (b b)) (H (b b)) (H (b)) (H ())) ((S (a b b)) (K (a b b)) (H (a b b)))))
              'no-stck
              'no-farthest-consumed
              (list->zipper '())
              (sub1 (length '()))
              '(1 2 3 2)
              '((computation
                 ((H ()) (H (b)) (K (b)) (B (b b)) (K (a b b)) (S (a b b)))
                 ((H b H) (K ε H) (B b K) (K a B) (S ε K))
                 ((H (b)) (K (b)) (B (b b)) (K (a b b)) (S (a b b))))
                (computation ((H ()) (H (b)) (H (b b)) (C (b b)) (S (a b b))) ((H b H) (H b H) (C ε H) (S a C)) ((H (b)) (H (b b)) (C (b b)) (S (a b b)))))
              'no-max-cmps
              0
              (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                (if (> 0 offset-cap) 0 offset-cap))
              0)))

(define pda-info-img (pda-create-draw-informative-message (imsg-state a*
              '(a b b)
              '()
              '()
              0
              (list->zipper '())
              (list->zipper '())
              '(1)
              (list->zipper '())
              (sub1 (length '()))
              '(1 2 3 2)
              '((computation
                 ((H ()) (H (b)) (K (b)) (B (b b)) (K (a b b)) (S (a b b)))
                 ((H b H) (K ε H) (B b K) (K a B) (S ε K))
                 ((H (b)) (K (b)) (B (b b)) (K (a b b)) (S (a b b))))
                (computation ((H ()) (H (b)) (H (b b)) (C (b b)) (S (a b b))) ((H b H) (H b H) (C ε H) (S a C)) ((H (b)) (H (b b)) (C (b b)) (S (a b b)))))
              1
              0
              (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                (if (> 0 offset-cap) 0 offset-cap))
              0)))


(define tm-info-img (tm-create-draw-informative-message (imsg-state EVEN-AS-&-BS
                                                                    '()
                                                                    '()
                                                                    '(@ a a b)
                                                                    1
                                                                    (list->zipper '())
                                                                    (list->zipper '())
                                                                    '()
                                                                    (list->zipper '())
                                                                    0
                                                                    '()
                                                                    '()
                                                                    0
                                                                    0
                                                                    (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                                                      (if (> 0 offset-cap) 0 offset-cap))
                                                                    0)))

#;(imsg-state EVEN-AS-&-BS
            '()
            '()
            '(@ a a b)
            1
            (list->zipper '())
            (list->zipper '())
            0
            '()
            '()
            0
            0
            (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
              (if (> 0 offset-cap) 0 offset-cap))
            0)
                                                                    


(define NDFA-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height ndfa-info-img)
                          (image-height E-SCENE-TOOLS)))

(define PDA-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height pda-info-img)
                          (image-height E-SCENE-TOOLS)))

(define TM-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height tm-info-img)
                          (image-height E-SCENE-TOOLS)))

(define ndfa-img-bounding-limit
  (bounding-limits 0
                  (* NDFA-E-SCENE-HEIGHT 0.9)
                  NDFA-E-SCENE-HEIGHT
                  (+ NDFA-E-SCENE-HEIGHT (image-height ndfa-info-img)
                     )))

(define pda-img-bounding-limit
  (bounding-limits 0
                  (* PDA-E-SCENE-HEIGHT 0.9)
                  PDA-E-SCENE-HEIGHT
                  (+ PDA-E-SCENE-HEIGHT (image-height pda-info-img)
                     )))


(define tm-img-bounding-limit
  (bounding-limits 0
                  (* TM-E-SCENE-HEIGHT 0.9)
                  TM-E-SCENE-HEIGHT
                  (+ TM-E-SCENE-HEIGHT (image-height tm-info-img)
                     )))
(define NDFA-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 NDFA-E-SCENE-HEIGHT))
(define PDA-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 PDA-E-SCENE-HEIGHT))
(define TM-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 TM-E-SCENE-HEIGHT))

(define E-SCENE-TOOLS-WIDTH (image-width E-SCENE-TOOLS))

(create-bounding-limits E-SCENE-WIDTH NDFA-E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH
                                                     ndfa-img-bounding-limit SM-VIZ-FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
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
 (D-KEY "Word end")
 (J-KEY "Prv not inv")
 (L-KEY "Nxt not inv")))

#;(define pda-bounding-limits (create-bounding-limits E-SCENE-WIDTH PDA-E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH
                                                    pda-img-bounding-limit SM-VIZ-FONT-SIZE LETTER-KEY-WIDTH-BUFFER INS-TOOLS-BUFFER
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
 (D-KEY "Word end")
 (J-KEY "Prv not inv")
 (L-KEY "Nxt not inv"))))

(define jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP))
(define jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP))
(define viz-go-next
  (go-next E-SCENE-WIDTH
           NDFA-E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))

(define viz-go-prev
  (go-prev E-SCENE-WIDTH
           NDFA-E-SCENE-HEIGHT
           NODE-SIZE
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM-FLOOR
           PERCENT-BORDER-GAP))

(define viz-go-to-begin
  (go-to-begin E-SCENE-WIDTH
               NDFA-E-SCENE-HEIGHT
               NODE-SIZE
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM-FLOOR
               PERCENT-BORDER-GAP))

(define viz-go-to-end
  (go-to-end E-SCENE-WIDTH
             NDFA-E-SCENE-HEIGHT
             NODE-SIZE
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             PERCENT-BORDER-GAP))

(define viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           NDFA-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            NDFA-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                NDFA-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               NDFA-E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))

(define viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              NDFA-E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))