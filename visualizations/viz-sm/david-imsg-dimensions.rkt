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
         "david-viz-constants.rkt"
         "../viz-lib/viz-constants.rkt")

(define FONT-SIZE 18)
(provide (all-defined-out))

(define INS-TOOLS-BUFFER 30)

(define ndfa-info-img (ndfa-create-draw-informative-message
                       (imsg-state-ndfa AB*B*UAB*
                                        (list->zipper (list (ci '() '())))
                                        (list->zipper '())
                                        (ndfa-config 'S '() 0)
                                        (list->zipper '())
                                        (hash)
                                        #f
                                        0
                                        (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                          (if (> 0 offset-cap) 0 offset-cap))
                                        0
                                        standard-color-scheme)))

(define pda-info-img (pda-create-draw-informative-message
                      (imsg-state-pda a*
                                      (list->zipper (list (ci '() '())))
                                      (list->zipper '())
                                      (list->zipper (list (pda-config 'S '() '() 0)))
                                      (pda-config 'S '() '() 0)
                                      (list->zipper '())
                                      (hash) 
                                      #f
                                      #f
                                      1
                                      0
                                      (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                        (if (> 0 offset-cap) 0 offset-cap))
                                      0
                                      standard-color-scheme)))


(define tm-info-img (tm-create-draw-informative-message
                     (imsg-state-tm EVEN-AS-&-BS
                                    (list->zipper (list `(,LM b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a c c b a b
                                                        a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a c)))
                                    (list->zipper '(1))
                                    (list->zipper '())
                                    (list->zipper '())
                                    (list->zipper '())
                                    (list->zipper '(1))
                                    1
                                    'accept
                                    0
                                    (let ([offset-cap (- (length '(a b b)) TM-TAPE-SIZE)])
                                      (if (> 0 offset-cap) 0 offset-cap))
                                    0
                                    standard-color-scheme))
  #;(above (tm-create-draw-informative-message
                     (imsg-state-tm EVEN-AS-&-BS
                                    (list->zipper (list `(,LM b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a c c b a b
                                                        a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a b a c)))
                                    (list->zipper '(1))
                                    (list->zipper '())
                                    (list->zipper '())
                                    (list->zipper '())
                                    (list->zipper '(1))
                                    1
                                    'accept
                                    0
                                    (let ([offset-cap (- (length '(a b b)) TM-TAPE-SIZE)])
                                      (if (> 0 offset-cap) 0 offset-cap))
                                    0
                                    standard-color-scheme))
                     (let ([buffer-sqaure (square HEIGHT-BUFFER 'solid (color-palette-blank-color standard-color-scheme))])
                         (above (text "Accept traced" 20 (color-palette-legend-shown-accept-color standard-color-scheme))
                                   buffer-sqaure
                                   (text "Accept not traced" 20 (color-palette-legend-other-accept-color standard-color-scheme))
                                   buffer-sqaure
                                   (text "Reject not traced" 20 (color-palette-legend-other-reject-color standard-color-scheme))))))

(define mttm-2tape-info-img (mttm-create-draw-informative-message
                             (imsg-state-mttm ww
                                              (list->zipper (list (list `(,LM b a)
                                                                        `(,BLANK))))
                                              (list->zipper (list (list 1 0)))
                                              (list->zipper (list (trace 'khrecom 'khcom)))
                                              (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                              (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                              (list->zipper '())
                                              (list->zipper '(1 2 3 3 4 4 5 5))
                                              1
                                              'accept
                                              1
                                              0
                                              (let ([offset-cap (- (length '(a b b)) TM-TAPE-SIZE)])
                                                (if (> 0 offset-cap) 0 offset-cap))
                                              0
                                              standard-color-scheme)))
(define mttm-3tape-info-img (mttm-create-draw-informative-message
                             (imsg-state-mttm a^nb^n
                                              (list->zipper (list (list `(,LM b a)
                                                                        `(,BLANK)
                                                                        `(,BLANK))))
                                              (list->zipper (list (list 1 0 0)))
                                              (list->zipper (list (trace 'kh358/2days 'seasalttrio)))
                                              (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                              (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                              (list->zipper '())
                                              (list->zipper '(1 2 3 3 4 4 5 5))
                                              1
                                              'accept
                                              1
                                              0
                                              (let ([offset-cap (- (length '(a b b)) TM-TAPE-SIZE)])
                                                (if (> 0 offset-cap) 0 offset-cap))
                                              0
                                              standard-color-scheme)))
(define mttm->=4tape-info-img (mttm-create-draw-informative-message
                               (imsg-state-mttm a^nb^nc^n
                                                (list->zipper (list (list `(,LM a b)
                                                                          `(,BLANK)
                                                                          `(,BLANK)
                                                                          `(,BLANK))))
                                                (list->zipper (list (list 1 0 0 0)))
                                                (list->zipper (list (trace 'khrecom 'khcom)))
                                                (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                                (list->zipper (list (trace (mttm-config 'K (list (tape-config 1 '(@ _ a)) (tape-config 0 '(_))) 0)
                                                                         (rule '@ '@ '@ '@))))
                                                (list->zipper '())
                                                (list->zipper '(1 2 3 3 4 4 5 5))
                                                1
                                                'accept
                                                1
                                                0
                                                (let ([offset-cap (- (length '(a b b)) TM-TAPE-SIZE)])
                                                  (if (> 0 offset-cap) 0 offset-cap))
                                                0
                                                standard-color-scheme)))
                                                                   
(define NDFA-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height ndfa-info-img)
                          (image-height E-SCENE-TOOLS)))

(define PDA-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height pda-info-img)
                          (image-height E-SCENE-TOOLS)))

(define TM-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height tm-info-img)
                          (image-height E-SCENE-TOOLS)))

(define MTTM-2tape-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height mttm-2tape-info-img)
                          (image-height E-SCENE-TOOLS)))

(define MTTM-3tape-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height mttm-3tape-info-img)
                          (image-height E-SCENE-TOOLS)))

(define MTTM->=4tape-E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height mttm->=4tape-info-img)
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


(define mttm-2tape-img-bounding-limit
  (bounding-limits 0
                  (* MTTM-2tape-E-SCENE-HEIGHT 0.9)
                  MTTM-2tape-E-SCENE-HEIGHT
                  (+ MTTM-2tape-E-SCENE-HEIGHT (image-height mttm-2tape-info-img)
                     )))

(define mttm-3tape-img-bounding-limit
  (bounding-limits 0
                  (* MTTM-3tape-E-SCENE-HEIGHT 0.9)
                  MTTM-3tape-E-SCENE-HEIGHT
                  (+ MTTM-3tape-E-SCENE-HEIGHT (image-height mttm-3tape-info-img)
                     )))

(define mttm->=4tape-img-bounding-limit
  (bounding-limits 0
                  (* MTTM->=4tape-E-SCENE-HEIGHT 0.9)
                  MTTM->=4tape-E-SCENE-HEIGHT
                  (+ MTTM->=4tape-E-SCENE-HEIGHT (image-height mttm->=4tape-info-img)
                     )))

(define NDFA-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 NDFA-E-SCENE-HEIGHT))
(define PDA-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 PDA-E-SCENE-HEIGHT))
(define TM-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 TM-E-SCENE-HEIGHT))
(define MTTM-2tape-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 MTTM-2tape-E-SCENE-HEIGHT))
(define MTTM-3tape-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 MTTM-3tape-E-SCENE-HEIGHT))
(define MTTM->=4tape-E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 MTTM->=4tape-E-SCENE-HEIGHT))


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

(define pda-jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  PDA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-pda-invs-zipper))

(define pda-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  PDA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-pda-invs-zipper))

(define ndfa-jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-ndfa-invs-zipper))

(define ndfa-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-ndfa-invs-zipper))

(define tm-jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  TM-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-tm-invs-zipper))

(define tm-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  TM-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-tm-invs-zipper))

(define mttm-jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-mttm-invs-zipper))

(define mttm-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-mttm-invs-zipper))

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

(define pda-viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           PDA-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define pda-viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            PDA-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define pda-viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                PDA-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define pda-viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               PDA-E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))

(define pda-viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              PDA-E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))








(define tm-viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           TM-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define tm-viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            TM-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define tm-viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                TM-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define tm-viz-max-zoom-in
  (max-zoom-in E-SCENE-WIDTH
               TM-E-SCENE-HEIGHT
               ZOOM-INCREASE
               ZOOM-DECREASE
               NODE-SIZE
               PERCENT-BORDER-GAP
               DEFAULT-ZOOM-CAP
               DEFAULT-ZOOM))

(define tm-viz-reset-zoom
  (reset-zoom E-SCENE-WIDTH
              TM-E-SCENE-HEIGHT
              ZOOM-INCREASE
              ZOOM-DECREASE
              NODE-SIZE
              PERCENT-BORDER-GAP
              DEFAULT-ZOOM-CAP
              DEFAULT-ZOOM))

(define mttm-2tape-viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                MTTM-2tape-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define mttm-2tape-viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            MTTM-2tape-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define mttm-2tape-viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           MTTM-2tape-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define mttm-3tape-viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                MTTM-3tape-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define mttm-3tape-viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            MTTM-3tape-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))

(define mttm-3tape-viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           MTTM-3tape-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))

(define mttm->=4tape-viz-max-zoom-out
  (max-zoom-out E-SCENE-WIDTH
                MTTM->=4tape-E-SCENE-HEIGHT
                ZOOM-INCREASE
                ZOOM-DECREASE
                NODE-SIZE
                PERCENT-BORDER-GAP
                DEFAULT-ZOOM-CAP
                DEFAULT-ZOOM))

(define mttm->=4tape-viz-zoom-out
  (zoom-out E-SCENE-WIDTH
            MTTM->=4tape-E-SCENE-HEIGHT
            ZOOM-INCREASE
            ZOOM-DECREASE
            NODE-SIZE
            PERCENT-BORDER-GAP
            DEFAULT-ZOOM-CAP
            DEFAULT-ZOOM))


(define mttm->=4tape-viz-zoom-in
  (zoom-in E-SCENE-WIDTH
           MTTM->=4tape-E-SCENE-HEIGHT
           ZOOM-INCREASE
           ZOOM-DECREASE
           NODE-SIZE
           PERCENT-BORDER-GAP
           DEFAULT-ZOOM-CAP
           DEFAULT-ZOOM))