#lang racket
(require "../fsm-gviz/private/lib.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         "definitions-viz.rkt"
         "run-viz.rkt"
         "../fsm-core/interface.rkt")

(provide intersection-viz)

(define FNAME "fsm")

;; L = nl
(define nl (make-ndfa '(S)
                      '(a b)
                      'S
                      '()
                      '()))

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
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
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))

;; L = ab*
(define ab*-deter (make-dfa '(S A)
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
                    



(define E-SCENE (empty-scene 1250 600))
(define E-SCENE-TOOLS (overlay (above (above (triangle 30 'solid 'black)
                                             (rectangle 10 30 'solid 'black))
                                      (square 20 'solid 'white)
                                      (text "Restart the visualization" 20 'black)
                                      (square 60 'solid 'white)
                                      (beside (rectangle 30 10 'solid 'black)
                                              (rotate 270 (triangle 30 'solid 'black)))
                                      (square 20 'solid 'white)
                                      (text "Move one step forward" 20 'black)
                                      (square 60 'solid 'white)
                                      (beside (rotate 90 (triangle 30 'solid 'black))
                                              (rectangle 30 10 'solid 'black))
                                      (square 20 'solid 'white)
                                      (text "Move one step backward" 20 'black)
                                      (square 60 'solid 'white)
                                      (above (rectangle 10 30 'solid 'black)
                                             (rotate 180 (triangle 30 'solid 'black)))
                                      (square 20 'solid 'white)
                                      (text "Complete the visualization" 20 'black)
                                      )
                               (empty-scene 250 600)))


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
      (make-dfa (sm-states ndfa)
                (sm-sigma ndfa)
                (sm-start ndfa)
                new-finals (sm-rules ndfa) 'no-dead)))
  (let* [(Mdfa (ndfa->dfa M))
         (dfaM (above (graph->bitmap (create-edge-graph-m
                                      (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                         Mdfa)
                                      Mdfa))
                      (text "M-dfa: M converted to a dfa" 20 'black)))
         (Ndfa (ndfa->dfa N))
         (dfaN (above (graph->bitmap (create-edge-graph-n
                                      (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                         Ndfa)
                                      Ndfa))
                      (text "N-dfa: N converted to a dfa" 20 'black)))
         (Mcomplement (sm-complement (ndfa->dfa M)))
         (cmplM (above (graph->bitmap (create-edge-graph-m
                                       (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                          Mcomplement)
                                       Mcomplement))
                       (text "CM-dfa: Complement of M-dfa" 20 'black)))
         (Ncomplement (sm-complement (ndfa->dfa N)))
         (cmplN (above (graph->bitmap (create-edge-graph-n
                                       (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                          Ncomplement)
                                       Ncomplement))
                       (text "CN-dfa: Complement of N-dfa" 20 'black)))
         (nM (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa M))))
         (nN (sm-rename-states (list DEAD) (sm-complement (ndfa->dfa N))))
         (notM (create-edge-graph-m
                (create-node-graph
                 (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                 nM)
                nM))
         (notN (create-edge-graph-n (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                       nN)
                                    nN))
         (notMimg (above (graph->bitmap notM) (text "CM-dfa States Renamed" 20 'black)))
         (notNimg (above (graph->bitmap notN) (text "CN-dfa States Renamed" 20 'black)))
         (notM-U-notN (sm-union nM nN))
         (ndfa-un (ndfa->dfa notM-U-notN))
         (comp-un (complement-fsa ndfa-un))
         (unionMN (above (graph->bitmap
                          (create-edge-graph-union (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                      notM-U-notN)
                                                   notM-U-notN nM nN))
                         (text "U-CM-dfa-CN-dfa: Union of CM-dfa and CN-dfa" 20 'black)))
         (dfaMN (above (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                            ndfa-un)
                                                         ndfa-un))
                       (text "DU-CM-dfa-CN-dfa: U-CM-dfa-CN-dfa converted to a dfa" 20 'black)))
         (final-graph (above (graph->bitmap (create-edge-graph (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                                                                  comp-un)
                                                               comp-un))
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
  (above (text (if (eq? (sm-type M) 'ndfa)
                   " M is an ndfa and needs to be converted to a dfa"
                   " M is a dfa") 20 'black)
         (graph->bitmap (create-edge-graph-m
                         (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                            M)
                         M))
         (text (if (eq? (sm-type N) 'ndfa)
                   "\n\n\n N is an ndfa and needs to be converted to a dfa"
                   "\n\n\n N is a dfa") 20 'black)
         (graph->bitmap (create-edge-graph-n
                         (create-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "LR" 'font "Sans"))
                                            N)
                         N))))
     

;; draw-world
;; viz-state -> img
;; Purpose: To render the given viz-state
(define (draw-world a-vs)
  (let [(width (image-width (first (viz-state-pimgs a-vs))))
        (height (image-height (first (viz-state-pimgs a-vs))))]
    (if (or (> width (image-width E-SCENE))
            (> height (image-height E-SCENE)))
        (beside E-SCENE-TOOLS (overlay (resize-image (first (viz-state-pimgs a-vs)) (- (image-width E-SCENE) 5)
                                                     (- (image-height E-SCENE) 5))
                                       E-SCENE))
        (beside E-SCENE-TOOLS (overlay (first (viz-state-pimgs a-vs)) E-SCENE)))))


;; intersection-viz
;; fsa fsa -> void
(define (intersection-viz M N)
  (let* [(imgs (create-graph-imgs M N))]
    (run-viz (viz-state imgs (list (make-init-grph-img M N))) draw-world 'intersection-viz)))

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