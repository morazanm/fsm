#lang racket
(require "../../fsm-gviz/interface.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "../viz-lib/resize-sm-image.rkt"
         ;"definitions-viz.rkt"
         ;"run-viz.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/sm-getters.rkt"
         "../../fsm-core/private/misc.rkt"
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/viz-state.rkt"
         ;"../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/bounding-limits.rkt"
         "../../fsm-core/private/regexp.rkt"
         "../viz-lib/viz-imgs/cursor.rkt"
         "../viz-lib/zipper.rkt")

(provide intersection-viz)

(define FNAME "fsm")

;; L = nl
(define nl (make-unchecked-ndfa '(S)
                                '(a b)
                                'S
                                '()
                                '()))

;; L = ab*
(define ab* (make-unchecked-ndfa '(S A)
                                 '(a b)
                                 'S
                                 '(A)
                                 '((S a A)
                                   (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-unchecked-ndfa '(Z H B C D F)
                                      '(a b)
                                      'Z
                                      '(F)
                                      `((Z a H)
                                        (Z a B)
                                        (H a D)
                                        (D ,EMP F)
                                        (B a C)
                                        (C b F)
                                        (F b F))))
;; L = aab*
(define aab* (make-unchecked-ndfa '(W X Y)
                                  '(a b)
                                  'W
                                  '(Y)
                                  '((W a X)
                                    (X a Y)
                                    (Y b Y))))
;; L = a*
(define a* (make-unchecked-ndfa '(S D)
                                '(a b)
                                'S
                                '(S)
                                '((S a S)
                                  (S b D)
                                  (D a D)
                                  (D b D))
                                'no-dead))

;; L = ab*
(define ab*-deter (make-unchecked-ndfa '(S A)
                                       '(a b)
                                       'S
                                       '(A)
                                       '((S a A)
                                         (A b A))))

;; INTERSECTION VISUALIZATION



;; make-node-union
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (make-node-union graph los s f)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state s) 'green]
                                     [else 'black])
                        'shape (if (member state f)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         los))

;; make-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (make-edge-union graph M N ns)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid)))
         graph
         (append (list (list ns EMP (sm-start M))
                       (list ns EMP (sm-start N)))
                 (sm-rules M)
                 (sm-rules N))))

;; create-union-graph
;; ndfa ndfa -> dgraph
;; Purpose: To create a graph image for the union
;; Assume: The intersection of the states of the given machines is empty
(define (create-union-graph M N)
  (let* [(new-start (generate-symbol 'S (append (sm-states M) (sm-states N))))
         (new-states (cons new-start
                           (append (sm-states M) (sm-states N))))
         (added-edges (list (list new-start EMP (sm-start M))
                            (list new-start EMP (sm-start N))))
         (new-finals (append (sm-finals M) (sm-finals N)))]
    (make-edge-union (make-node-union
                      (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                      new-states new-start new-finals) M N new-start)))
                    


;; graph-struct
(struct graph-struct (grph inf))

(define E-SCENE (empty-scene 1250 600))
(define E-SCENE-TOOLS (overlay (beside (above (above (triangle 30 'solid 'black)
                                                     (rectangle 10 30 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Restart the visualization" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rectangle 30 10 'solid 'black)
                                                      (rotate 270 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Move one step forward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (beside (rotate 90 (triangle 30 'solid 'black))
                                                      (rectangle 30 10 'solid 'black))
                                              (square 20 'solid 'white)
                                              (text "Move one step backward" 18 'black))
                                       (square 40 'solid 'white)
                                       (above (above (rectangle 10 30 'solid 'black)
                                                     (rotate 180 (triangle 30 'solid 'black)))
                                              (square 20 'solid 'white)
                                              (text "Complete the visualization" 18 'black))
                                       )
                               (empty-scene 1250 100)))


;; create-node-graph
;; graph los start final -> graph
;; Purpose: To make a node graph
(define (create-node-graph graph M)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(eq? state (sm-start M)) 'darkgreen]
                                     [else 'black])
                        'shape (if (member state (sm-finals M))
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         (sm-states M)))

;; create-edge-graph
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph graph M)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                )))
         graph
         (sm-rules M)))

;; create-edge-graph-n
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-n graph N)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (if (member rule (sm-rules N))
                                                           'violet
                                                           'black)
                                                )))
         graph
         (sm-rules N)))

;; create-edge-graph-m
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-m graph M)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (if (member rule (sm-rules M))
                                                           'orange
                                                           'black)
                                                )))
         graph
         (sm-rules M)))

;; create-edge-graph-union
;; graph ndfa ndfa -> graph
;; Purpose: To make an edge graph
(define (create-edge-graph-union graph union M N)
  (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (if (equal? (first rule) '())
                                        'ds
                                        (first rule))
                                    (if (equal? (third rule) '())
                                        'ds
                                        (third rule))
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color (cond [(member rule (sm-rules M))
                                                              'orange]
                                                             [(member rule (sm-rules N))
                                                              'violet]
                                                             [else
                                                              'black]))))
         graph
         (sm-rules union)))


;; create-graph-img
;; ndfa ndfa -> img
;; Purpose: To create a graph image for the intersection
(define (create-graph-imgs M N)
  (define (complement-fsa ndfa)
    (let* [(new-finals (filter (λ (s) (not (member s (sm-finals ndfa)))) (sm-states ndfa)))]
      (make-unchecked-dfa (sm-states ndfa)
                          (sm-sigma ndfa)
                          (sm-start ndfa)
                          new-finals (sm-rules ndfa) 'no-dead)))
  (let* [(Mdfa (ndfa->dfa M))
         (dfaM (graph-struct (create-edge-graph-m
                              (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                 Mdfa)
                              Mdfa)
                             (text "M-dfa: M converted to a dfa" 20 'black)))
         (Ndfa (ndfa->dfa N))
         (dfaN (graph-struct (create-edge-graph-n
                              (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                 Ndfa)
                              Ndfa)
                             (text "N-dfa: N converted to a dfa" 20 'black)))
         (Mcomplement (complement-fsa (ndfa->dfa M)))
         (cmplM (graph-struct (create-edge-graph-m
                               (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                  Mcomplement)
                               Mcomplement)
                              (text "CM-dfa: Complement of M-dfa" 20 'black)))
         (Ncomplement (complement-fsa (ndfa->dfa N)))
         (cmplN (graph-struct (create-edge-graph-n
                               (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                  Ncomplement)
                               Ncomplement)
                              (text "CN-dfa: Complement of N-dfa" 20 'black)))
         (nM (rename-states-fsa (list DEAD) (complement-fsa (ndfa->dfa M))))
         (nN (rename-states-fsa (sm-states nM) (complement-fsa (ndfa->dfa N))))
         (notM (create-edge-graph-m
                (create-node-graph
                 (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                 nM)
                nM))
         (notN (create-edge-graph-n (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                       nN)
                                    nN))
         (notMimg (graph-struct  notM (text "CM-dfa States Renamed" 20 'black)))
         (notNimg (graph-struct notN (text "CN-dfa States Renamed" 20 'black)))
         (notM-U-notN (make-unchecked-union nM nN))
         (ndfa-un (ndfa->dfa notM-U-notN))
         (comp-un (complement-fsa ndfa-un))
         (unionMN (graph-struct(create-edge-graph-union (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                           notM-U-notN)
                                                        notM-U-notN nM nN)
                               (text "U-CM-dfa-CN-dfa: Union of CM-dfa and CN-dfa" 20 'black)))
         (dfaMN (graph-struct (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                    ndfa-un)
                                                 ndfa-un)
                              (text "DU-CM-dfa-CN-dfa: U-CM-dfa-CN-dfa converted to a dfa" 20 'black)))
         (final-graph (graph-struct (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                          comp-un)
                                                       comp-un)
                                    (text "Intersection of M and N: Complement of DU-CM-dfa-CN-dfa" 20 'black)))]
    (cond [(and (eq? (sm-type M) 'dfa)
                (eq? (sm-type N) 'ndfa))
           (list dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type N) 'dfa)
                (eq? (sm-type M) 'ndfa))
           (list dfaM cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type M) 'ndfa)
                (eq? (sm-type N) 'ndfa))
           (list dfaM dfaN cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)]
          [(and (eq? (sm-type M) 'dfa)
                (eq? (sm-type N) 'dfa))
           (list cmplM cmplN notMimg notNimg unionMN dfaMN final-graph)])))
          
    
                    
     
;; make-init-grph-img
;; ndfa ndfa -> img
;; Purpose: To draw the graph of the initial ndfa's
(define (make-init-grph-img M N)
  (graph-struct (list
                 (create-edge-graph-m
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     M)
                  M)
                 (create-edge-graph-n
                  (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                     N)
                  N))
                (list (text (if (eq? (sm-type N) 'ndfa)
                                "\n\n\n N is an ndfa and needs to be converted to a dfa"
                                "\n\n\n N is a dfa") 20 'black)
                      (text (if (eq? (sm-type M) 'ndfa)
                                " M is an ndfa and needs to be converted to a dfa"
                                " M is a dfa") 20 'black))
                ))

;; draw-imsg
;; imsg -> img
(define (draw-imsg a-imsg)
  (zipper-current (graph-struct-inf a-imsg)))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
#;(define (draw-world a-vs)
    (let [(width (image-width (first (viz-state-pimgs a-vs))))
          (height (image-height (first (viz-state-pimgs a-vs))))]
      (if (or (> width (image-width E-SCENE))
              (> height (image-height E-SCENE)))
          (above (overlay (resize-image (first (viz-state-pimgs a-vs)) (- (image-width E-SCENE) 5)
                                        (- (image-height E-SCENE) 5))
                          E-SCENE) E-SCENE-TOOLS)
          (above (overlay (first (viz-state-pimgs a-vs)) E-SCENE) E-SCENE-TOOLS))))


;; intersection-viz
;; fsa fsa -> void
(define (intersection-viz M N)
  (let* [(imgs (create-graph-imgs M N))]
    (run-viz (list (make-init-grph-img M) (create-graph-imgs M))
             (lambda () (make-init-grph-img M))
             MIDDLE-E-SCENE
             DEFAULT-ZOOM
             DEFAULT-ZOOM-CAP
             DEFAULT-ZOOM-FLOOR
             (informative-messages draw-imsg
                                   (let ([new-start (generate-symbol 'K (sm-states M))])
                                     (graph-struct-inf
                                      0
                                      new-start
                                      (list (list new-start EMP (sm-start M))
                                            (map (λ (f) (list f EMP new-start))
                                                 (sm-finals M)))))
                                   RULE-YIELD-DIMS)
             (instructions-graphic
              E-SCENE-TOOLS
              (bounding-limits 0
                               (image-width imsg-img)
                               E-SCENE-HEIGHT
                               (+ E-SCENE-HEIGHT (image-height imsg-img))))
             (create-viz-draw-world E-SCENE-WIDTH E-SCENE-HEIGHT INS-TOOLS-BUFFER)
             (create-viz-process-key (list (list "right" viz-go-next right-key-pressed);;right-key-pressed)
                                           (list "left" viz-go-prev left-key-pressed);;left-key-pressed)
                                           (list "up" viz-go-to-begin up-key-pressed);;up-key-pressed)
                                           (list "down" viz-go-to-end down-key-pressed);;down-key-pressed)
                                           (list "w" viz-zoom-in identity)
                                           (list "s" viz-zoom-out identity)
                                           (list "r" viz-max-zoom-out identity)
                                           (list "f" viz-max-zoom-in identity)
                                           (list "e" viz-reset-zoom identity)
                                           (list "wheel-down" viz-zoom-in identity)
                                           (list "wheel-up" viz-zoom-out identity)))
             (create-viz-process-tick E-SCENE-BOUNDING-LIMITS NODE-SIZE E-SCENE-WIDTH E-SCENE-HEIGHT
                                      CLICK-BUFFER-SECONDS
                                      (list)
                                      (list (list ARROW-UP-KEY-DIMS viz-go-to-begin up-key-pressed);;up-key-pressed)
                                            (list ARROW-DOWN-KEY-DIMS viz-go-to-end down-key-pressed);;down-key-pressed)
                                            (list ARROW-LEFT-KEY-DIMS viz-go-prev left-key-pressed);;left-key-pressed)
                                            (list ARROW-RIGHT-KEY-DIMS viz-go-next right-key-pressed);;right-key-pressed)
                                            (list W-KEY-DIMS viz-zoom-in identity)
                                            (list S-KEY-DIMS viz-zoom-out identity)
                                            (list R-KEY-DIMS viz-max-zoom-out identity)
                                            (list E-KEY-DIMS viz-reset-zoom identity)
                                            (list F-KEY-DIMS viz-max-zoom-in identity)
                                            ;(list A-KEY-DIMS identity a-key-pressed)
                                            #;(list D-KEY-DIMS identity d-key-pressed)))
             )))

(define no-one-el (make-dfa '(S A B C D E F G)
                            '(a b c)
                            'S
                            '(D E F)
                            '((S a A)
                              (S b B)
                              (S c C)
                              (A a A)
                              (A b D)
                              (A c F)
                              (B a D)
                              (B b B)
                              (B c E)
                              (C a F)
                              (C b E)
                              (C c C)
                              (D a D)
                              (D b D)
                              (D c G)
                              (E a G)
                              (E b E)
                              (E c E)
                              (F a F)
                              (F b G)
                              (F c F)
                              (G a G)
                              (G b G)
                              (G c G))
                            'no-dead))