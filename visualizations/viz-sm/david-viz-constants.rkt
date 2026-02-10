#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "default-informative-messages.rkt"
         "../viz-lib/zipper.rkt"
         racket/treelist
         "david-imsg-state.rkt"
         "../viz-lib/viz-constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/tm.rkt")

(define FONT-SIZE 18)
(provide (all-defined-out))



#|
A computation is a structure: (computation LoC LoR LoT visited)
LoC is a (listof configuration)
LoR is a (listof rule)
visited is a (hashof configuration)
|#
(struct computation (LoC LoR visited) #:transparent)

#|
A rule is a structure: (rule source read destination action)
source is the source state                       | symbol
read is the element to be read on the tape       | symbol
destination is the destination state             | symbol
action is the action to be performed on the tape | TM-ACTION
|#
(struct rule (source read destination action) #:transparent)


(define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
(define (tm-getstates m) (m '() 0 'get-states))
  
(define (tm-getfinals m) (m '() 0 'get-finals))

(define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules

(define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules

(define (tm-getstart m) (m '() 0 'get-start))
  
(define (tm-getaccept m) (m '() 0 'get-accept))

(define (tm-whatami? m) (m 'whatami 0 'whatami))

;; X (listof X) (X -> boolean) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func) (for/or ([L lst]) (eq-func x L))) 

(define HELD-INV-COLOR 'chartreuse4)
(define BRKN-INV-COLOR 'red2)
(define TRACKED-ACCEPT-COLOR 'forestgreen)
(define ALL-ACCEPT-COLOR 'green)
(define SPLIT-ACCEPT-COLOR 
  (string-append (symbol->string TRACKED-ACCEPT-COLOR) ":" (symbol->string ALL-ACCEPT-COLOR)))

;;tm -> tm-struct
;;Purpose: Converts the tm into a tm structure
(define (remake-tm M)
  ;;(listof rules) -> (treelistof rule-struct)
  ;;Purpose: Converts the rules from the given tm to rule-structs
  (define (remake-rules a-lor)
    (for/treelist ([tm-rule a-lor])
      (rule (first (first tm-rule)) (second (first tm-rule))
            (first (second tm-rule)) (second (second tm-rule)))))
  (tm (tm-getstates M)
      (tm-getalphabet M)
      (remake-rules (tm-getrules M))
      (tm-getstart M)
      (tm-getfinals M)
      (if (eq? (tm-whatami? M) 'tm-language-recognizer) (tm-getaccept M) 'none)
      (tm-whatami? M)))

(define REJECT-COLOR 'violetred)
(define GRAPHVIZ-CUTOFF-GOLD 'darkgoldenrod2)
(define SM-VIZ-FONT-SIZE 18)
(define INS-TOOLS-BUFFER 30)

;;(X -> Y) :Purpose: A function to retrieve the index for a tm-config from a trace
(define get-tm-config-index-frm-trace (compose1 tm-config-index trace-config zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a pda-config from a trace
(define get-pda-config-index-frm-trace (compose1 pda-config-index trace-config zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a ndfa-config from a trace
(define get-ndfa-config-index-frm-trace (compose1 ndfa-config-index trace-config zipper-current))

;;(X -> Y) :Purpose: A function to retrieve the index for a tm-config from the invs-zipper
(define get-tm-config-index-frm-invs (compose1 tm-config-index first zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a pda-config from the invs-zipper
(define get-pda-config-index-frm-invs (compose1 pda-config-index first zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a ndfa-config from the invs-zipper
(define get-ndfa-config-index-frm-invs (compose1 ndfa-config-index first zipper-current))

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

(define EVEN-AS-&-BS (remake-tm (make-unchecked-tm '(K H I B S)
                                                   '(a b)
                                                   `(((K ,BLANK) (S ,BLANK))
                                                     ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                                                     ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                                                   'K
                                                   '(S)
                                                   'S)))

(define qempty? treelist-empty?)

(define E-QUEUE empty-treelist) 

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (treelist-first a-qox)))

;; (listof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (treelist-append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (treelist-rest a-qox)))

(define INFORMATIVE-MSG-HEIGHT 50)

(define (id x) x)

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

(define ndfa-info-img (ndfa-create-draw-informative-message
                       (imsg-state-ndfa AB*B*UAB*
                                        (list->zipper (list (ci '() '())))
                                        (list->zipper '())
                                        (ndfa-config 'S '() 0)
                                        (list->zipper '())
                                        (hash)
                                        0
                                        (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                          (if (> 0 offset-cap) 0 offset-cap))
                                        0)))

(define pda-info-img (pda-create-draw-informative-message
                      (imsg-state-pda a*
                                      (list->zipper (list (ci '() '())))
                                      (list->zipper '())
                                      (list->zipper '())
                                      (pda-config 'S '() '() 0)
                                      (list->zipper '())
                                      (hash) 
                                      #f 
                                      1
                                      0
                                      (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                        (if (> 0 offset-cap) 0 offset-cap))
                                      0)))


(define tm-info-img (tm-create-draw-informative-message (imsg-state-tm EVEN-AS-&-BS
                                                                       (list->zipper (list '(@ a a b)))
                                                                       (list->zipper '(1))
                                                                       (list->zipper '())
                                                                       (list->zipper '())
                                                                       (list->zipper '())
                                                                       (list->zipper '(1))
                                                                       1
                                                                       'accept
                                                                       0
                                                                       (let ([offset-cap (- (length '(a b b)) TAPE-SIZE)])
                                                                         (if (> 0 offset-cap) 0 offset-cap))
                                                                       0)))

                                                                   
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

(define pda-jump-next
  (jump-next-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-pda-invs-zipper))

(define pda-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
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
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-tm-invs-zipper))

(define tm-jump-prev
  (jump-prev-inv  E-SCENE-WIDTH
                  NDFA-E-SCENE-HEIGHT
                  NODE-SIZE
                  DEFAULT-ZOOM-CAP
                  DEFAULT-ZOOM-FLOOR
                  PERCENT-BORDER-GAP
                  imsg-state-tm-invs-zipper))

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