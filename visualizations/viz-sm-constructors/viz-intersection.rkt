#lang racket/base
(require "../../fsm-gviz/interface.rkt"
         2htdp/image
         "../../fsm-core/private/state.rkt"
         "../../fsm-core/private/rules.rkt"         
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/bounding-limits-macro.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt"
         racket/list
         racket/function)

(provide intersection-viz)

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
                                                     (list F-KEY "Max zoom"))))

;; imsg-img
;; Informative message image
(define imsg-img
  (above (text "N is an ndfa and needs to be converted to a dfa" 20 'black)
         (text "M is an ndfa and needs to be converted to a dfa" 20 'black)))

(define E-SCENE-HEIGHT (- (* 0.9 WINDOW-HEIGHT)
                          (image-height imsg-img)
                          (image-height E-SCENE-TOOLS)))
(define MIDDLE-E-SCENE (posn (/ E-SCENE-WIDTH 2) (/ E-SCENE-HEIGHT 2)))
(define E-SCENE-BOUNDING-LIMITS (bounding-limits 0 E-SCENE-WIDTH 0 E-SCENE-HEIGHT))

(define RULE-YIELD-DIMS
  (bounding-limits 0
                   (image-width imsg-img)
                   E-SCENE-HEIGHT
                   (+ E-SCENE-HEIGHT (image-height imsg-img))))

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
                         (F-KEY "Max zoom")))

;; L = nl
(define nl (make-unchecked-ndfa '(S) '(a b) 'S '() '()))

;; L = ab*
(define ab* (make-unchecked-ndfa '(S A) '(a b) 'S '(A) '((S a A) (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b*
  (make-unchecked-ndfa '(Z H B C D F)
                       '(a b)
                       'Z
                       '(F)
                       `((Z a H) (Z a B) (H a D) (D ,EMP F) (B a C) (C b F) (F b F))))
;; L = aab*
(define aab* (make-unchecked-ndfa '(W X Y) '(a b) 'W '(Y) '((W a X) (X a Y) (Y b Y))))
;; L = a*
(define a* (make-unchecked-ndfa '(S D) '(a b) 'S '(S) '((S a S) (S b D) (D a D) (D b D)) 'no-dead))

;; L = ab*
(define ab*-deter (make-unchecked-ndfa '(S A) '(a b) 'S '(A) '((S a A) (A b A))))

;; INTERSECTION VISUALIZATION

; ndfa ndfa --> ndfa
(define (union-special m1 m)
  (let* ((nm1 m1)
         (nm2 m))
    (let* ((new-start (gen-state (append (fsa-getstates nm1) 
                                         (fsa-getstates nm2))))
           (new-states (cons new-start
                             (append (fsa-getstates nm1) 
                                     (fsa-getstates nm2)))) ; nm1 & nm2 & s have no common state names
           (alphabet (remove-duplicates (append (fsa-getalphabet nm1) (fsa-getalphabet nm2))))
           (new-finals (union-states (fsa-getfinals nm1) (fsa-getfinals nm2)))
           (new-rules (cons (mk-fsarule new-start EMP (fsa-getstart nm1)) 
                            (cons (mk-fsarule new-start EMP (fsa-getstart nm2)) 
                                  (append (nm1 null 'get-deltas) 
                                          (nm2 null 'get-deltas))))))
      (make-unchecked-ndfa new-states
                           alphabet
                           new-start
                           new-finals
                           new-rules))))

;; graph-struct
;; grph - graph structure
;; inf - informative message
(struct graph-struct (grph inf))

;; create-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (create-node-graph graph M)
  (foldl (λ (state result)
           (add-node result
                     state
                     #:atb (hash 'color
                                 (cond
                                   [(eq? state (sm-start M)) 'darkgreen]
                                   [else 'black])
                                 'shape
                                 (if (member state (sm-finals M)) 'doublecircle 'circle)
                                 'label
                                 (if (equal? state '()) 'ds state)
                                 'fontcolor
                                 'black
                                 'font
                                 "Sans")))
         graph
         (sm-states M)))

;; create-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph graph M)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize 20 'style 'solid)))
         graph
         (sm-rules M)))

;; create-edge-graph-n
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-n graph N)
  (foldl (λ (rule result)
           (add-edge
            result
            (second rule)
            (if (equal? (first rule) '()) 'ds (first rule))
            (if (equal? (third rule) '()) 'ds (third rule))
            #:atb
            (hash 'fontsize 20 'style 'solid 'color (if (member rule (sm-rules N)) 'violet 'black))))
         graph
         (sm-rules N)))

;; create-edge-graph-m
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-m graph M)
  (foldl (λ (rule result)
           (add-edge
            result
            (second rule)
            (if (equal? (first rule) '()) 'ds (first rule))
            (if (equal? (third rule) '()) 'ds (third rule))
            #:atb
            (hash 'fontsize 20 'style 'solid 'color (if (member rule (sm-rules M)) 'orange 'black))))
         graph
         (sm-rules M)))

;; create-edge-graph-union
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-union graph union M N)
  (foldl (λ (rule result)
           (add-edge result
                     (second rule)
                     (if (equal? (first rule) '()) 'ds (first rule))
                     (if (equal? (third rule) '()) 'ds (third rule))
                     #:atb (hash 'fontsize
                                 20
                                 'style
                                 'solid
                                 'color
                                 (cond
                                   [(member rule (sm-rules M)) 'orange]
                                   [(member rule (sm-rules N)) 'violet]
                                   [else 'black]))))
         graph
         (sm-rules union)))

;; create-graph-structures
;; ndfa ndfa -> graph
;; Purpose: To create graph structures for the intersection
(define (create-graph-structures M N)
  (define (complement-fsa ndfa)
    (let* ([new-finals (filter (λ (s) (not (member s (sm-finals ndfa)))) (sm-states ndfa))])
      (make-unchecked-dfa (sm-states ndfa)
                          (sm-sigma ndfa)
                          (sm-start ndfa)
                          new-finals
                          (sm-rules ndfa)
                          'no-dead)))
  (let* ([Mdfa (ndfa->dfa M)]
         [dfaM (graph-struct
                (create-edge-graph-m
                 (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                    Mdfa)
                 Mdfa)
                (text "M-dfa: M converted to a dfa" 20 'black))]
         [Ndfa (ndfa->dfa N)]
         [dfaN (graph-struct
                (create-edge-graph-n
                 (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                    Ndfa)
                 Ndfa)
                (text "N-dfa: N converted to a dfa" 20 'black))]
         [Mcomplement (complement-fsa Mdfa)]
         [cmplM (graph-struct
                 (create-edge-graph-m
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     Mcomplement)
                  Mcomplement)
                 (text "CM-dfa: Complement of M-dfa" 20 'black))]
         [Ncomplement (complement-fsa Ndfa)]
         [cmplN (graph-struct
                 (create-edge-graph-n
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     Ncomplement)
                  Ncomplement)
                 (text "CN-dfa: Complement of N-dfa" 20 'black))]
         [nM (rename-states-fsa (list DEAD) Mcomplement)]
         [nN (rename-states-fsa (sm-states nM) Ncomplement)]
         [notM (create-edge-graph-m
                (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) nM)
                nM)]
         [notN (create-edge-graph-n
                (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) nN)
                nN)]
         [notMimg (graph-struct notM (text "CM-dfa States Renamed" 20 'black))]
         [notNimg (graph-struct notN (text "CN-dfa States Renamed" 20 'black))]
         [notM-U-notN (union-special nM nN)]
         [ndfa-un (ndfa->dfa notM-U-notN)]
         [comp-un (complement-fsa ndfa-un)]
         [unionMN (graph-struct
                   (create-edge-graph-union
                    (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                       notM-U-notN)
                    notM-U-notN
                    nM
                    nN)
                   (text "U-CM-dfa-CN-dfa: Union of CM-dfa and CN-dfa" 20 'black))]
         [dfaMN (graph-struct
                 (create-edge-graph
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     ndfa-un)
                  ndfa-un)
                 (text "DU-CM-dfa-CN-dfa: U-CM-dfa-CN-dfa converted to a dfa" 20 'black))]
         [final-graph
          (graph-struct
           (create-edge-graph
            (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) comp-un)
            comp-un)
           (text "Intersection of M and N: Complement of DU-CM-dfa-CN-dfa" 20 'black))])
    (cond
      [(and (eq? (sm-type M) 'dfa) (eq? (sm-type N) 'ndfa))
       (list dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
      [(and (eq? (sm-type N) 'dfa) (eq? (sm-type M) 'ndfa))
       (list dfaM cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
      [(and (eq? (sm-type M) 'ndfa) (eq? (sm-type N) 'ndfa))
       (list dfaM dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
      [(and (eq? (sm-type M) 'dfa) (eq? (sm-type N) 'dfa))
       (list cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)])))

;; make-init-grph-structure
;; ndfa ndfa -> graph
;; Purpose: To create the graph structure of the initial ndfa's
(define (make-init-grph-structure M N)
  (graph-struct
   (list (create-edge-graph-m
          (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) M)
          M)
         (create-edge-graph-n
          (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans")) N)
          N))
   (above
    (text (if (eq? (sm-type M) 'ndfa) "M is an ndfa and needs to be converted to a dfa" "M is a dfa")
          20
          'black)
    (text (if (eq? (sm-type N) 'ndfa) "N is an ndfa and needs to be converted to a dfa" "N is a dfa")
          20
          'black))))

;; draw-imsg
;; imsg -> img
;; Purpose: To draw informative messages
(define (draw-imsg a-imsg)
  (zipper-current (graph-struct-inf a-imsg)))

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

;; viz-state -> viz-state
;; Updates the informative messages to the next stage of the seqeuence
(define (right-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-next (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the previous stage of the seqeuence
(define (left-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-prev (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the beginning of the seqeuence
(define (up-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-begin? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-to-begin (graph-struct-inf a-graph-struct))])])]))))

;; viz-state -> viz-state
;; Updates the informative messages to the end of the seqeuence
(define (down-key-pressed a-vs)
  (let ([a-graph-struct (informative-messages-component-state (viz-state-informative-messages a-vs))])
    (if (zipper-at-end? (graph-struct-inf a-graph-struct))
        a-vs
        (struct-copy
         viz-state
         a-vs
         [informative-messages
          (struct-copy informative-messages
                       (viz-state-informative-messages a-vs)
                       [component-state
                        (struct-copy graph-struct
                                     a-graph-struct
                                     [inf (zipper-to-end (graph-struct-inf a-graph-struct))])])]))))

;; intersection-viz
;; fsa fsa -> void
(define (intersection-viz M N)
  (let ([renamed-machine (if (ormap (λ (x) (member x (sm-states M))) (sm-states N))
                             (rename-states-fsa (sm-states M) N)
                             N)])
    (run-viz
     (map graph-struct-grph
          (cons (make-init-grph-structure M N) (create-graph-structures M renamed-machine)))
     (lambda ()
       (apply above
              (map graph->bitmap (graph-struct-grph (make-init-grph-structure M renamed-machine)))))
     MIDDLE-E-SCENE
     DEFAULT-ZOOM
     DEFAULT-ZOOM-CAP
     DEFAULT-ZOOM-FLOOR
     (informative-messages
      draw-imsg
      (let ([new-start (generate-symbol 'K (sm-states M))])
        (graph-struct (list->zipper (map (lambda (x) '())
                                         (cons (make-init-grph-structure M N)
                                               (create-graph-structures M renamed-machine))))
                      (list->zipper (map graph-struct-inf
                                         (cons (make-init-grph-structure M N)
                                               (create-graph-structures M renamed-machine))))))
      RULE-YIELD-DIMS)
     (instructions-graphic E-SCENE-TOOLS
                           (bounding-limits 0
                                            (image-width imsg-img)
                                            E-SCENE-HEIGHT
                                            (+ E-SCENE-HEIGHT (image-height imsg-img))))
     (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
     (create-viz-process-key ["right" viz-go-next right-key-pressed]
                             ["left" viz-go-prev left-key-pressed]
                             ["up" viz-go-to-begin up-key-pressed]
                             ["down" viz-go-to-end down-key-pressed]
                             ["w" viz-zoom-in identity]
                             ["s" viz-zoom-out identity]
                             ["r" viz-max-zoom-out identity]
                             ["f" viz-max-zoom-in identity]
                             ["e" viz-reset-zoom identity]
                             ["wheel-down" viz-zoom-in identity]
                             ["wheel-up" viz-zoom-out identity])
     (create-viz-process-tick E-SCENE-BOUNDING-LIMITS
                              NODE-SIZE
                              E-SCENE-WIDTH
                              E-SCENE-HEIGHT
                              CLICK-BUFFER-SECONDS
                              ()
                              ([ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed]
                               [ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed]
                               [ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed]
                               [ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed]
                               [W-KEY-DIMS viz-zoom-in identity]
                               [S-KEY-DIMS viz-zoom-out identity]
                               [R-KEY-DIMS viz-max-zoom-out identity]
                               [E-KEY-DIMS viz-reset-zoom identity]
                               [F-KEY-DIMS viz-max-zoom-in identity]))
     'intersection-viz)))


