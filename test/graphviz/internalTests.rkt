#lang racket
(require
  "../../GraphViz/render-graph.rkt"
  "../../GraphViz/lib.rkt"
  "../test-helpers.rkt"
  "../../main.rkt"
  "../test-machine.rkt")
#|
  This file houses the test for the GraphViz library and fsm interface files
  This file was created by: Josh Schappel 5/5/21
|#

(module+ test
  (require rackunit)
  (require rackunit/text-ui)

  (define DEFAULT-NODE (hash
                        'color "black"
                        'shape "circle"))

  (define START-NODE (hash
                      'color "forestgreen"
                      'shape "circle"))

  (define END-NODE (hash
                    'color "black"
                    'shape "doublecircle"))

  (define STARTFINAL-NODE (hash
                           'color "forestgreen"
                           'shape "doublecircle"))

  (define ACCEPT-NODE (hash
                       'color "black"
                       'shape "doubleoctagon"))

  ;; -------------------------------
  ;; --------- create graph --------
  ;; ------------------------------- 
  (define graph-constructor
    (test-suite "Create Graph Function"
                (test-case "Default"
                           (define test-graph (create-graph 'test))
                           (check-equal? (graph-name test-graph) 'test)
                           (check-equal? (graph-node-list test-graph) '())
                           (check-equal? (graph-edge-list test-graph) '())
                           (check-equal? (graph-color-blind test-graph) 0))
                (test-case "Color Blind"
                           (define graph-cb (create-graph 'test-colorblind #:color 2))
                           (check-equal? (graph-name graph-cb) 'test-colorblind)
                           (check-equal? (graph-node-list graph-cb) '())
                           (check-equal? (graph-edge-list graph-cb) '())
                           (check-equal? (graph-color-blind graph-cb) 2))))

  ;; -------------------------------
  ;; --------- add node ------------
  ;; -------------------------------
  (define graph-add-node
    (test-suite "Add Node Function"
                (test-case "Default"
                           (define test-graph (add-node (create-graph 'test) 'A 'none))
                           
                           (check-equal? (length (graph-node-list test-graph)) 1)
                           (check-equal? (node-name (car (graph-node-list test-graph))) 'A)
                           (check-equal? (node-type (car (graph-node-list test-graph))) 'none)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) DEFAULT-NODE))
                (test-case "Start Node"
                           (define test-graph (add-node (create-graph 'test) 'A 'start))
                           (check-equal? (node-atb (car (graph-node-list test-graph))) START-NODE))
                (test-case "Final Node"
                           (define test-graph (add-node (create-graph 'test) 'A 'final))
                           (check-equal? (node-atb (car (graph-node-list test-graph))) END-NODE))
                (test-case "Startfinal Node"
                           (define test-graph (add-node (create-graph 'test) 'A 'startfinal))
                           (check-equal? (node-atb (car (graph-node-list test-graph))) STARTFINAL-NODE))
                (test-case "Acept Node"
                           (define test-graph (add-node (create-graph 'test) 'A 'accept))
                           (add-node test-graph 'A 'accept)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) ACCEPT-NODE))))

  ;; -------------------------------
  ;; --------- add edge ------------
  ;; -------------------------------
  (define graph-add-edge
    (test-suite "Add Edge Function"
                (test-case "Default"
                           (define test-graph (add-edge (create-graph 'test) 'a 'A 'B))
                           (define expected-atb (hash
                                                 'fontsize 15
                                                 'label (list 'a)))
                           
                           (check-equal? (edge-atb (car (graph-edge-list test-graph))) expected-atb)
                           (check-equal? (edge-start-node (car (graph-edge-list test-graph))) 'A)
                           (check-equal? (edge-end-node (car (graph-edge-list test-graph))) 'B))
                (test-case "Dashed"
                           (define test-graph (add-edge (create-graph 'test) 'a 'A-1 'B-2))
                           (define expected-atb (hash
                                                 'fontsize 15
                                                 'label (list 'a)))
                           (check-equal? (edge-atb (car (graph-edge-list test-graph))) expected-atb)
                           (check-equal? (edge-start-node (car (graph-edge-list test-graph))) 'A1)
                           (check-equal? (edge-end-node (car (graph-edge-list test-graph))) 'B2))))

  ;; -------------------------------
  ;; -------- fsa->graph -----------
  ;; -------------------------------
  (define graph-render-graph
    (test-suite "Render Graph Function"
                (test-case "dfa"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'F 'F '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'S 'S '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list (edge '#hash((fontsize . 15) (label . (b a))) 'F 'F) (edge '#hash((fontsize . 15) (label . (a))) 'S 'F))
                                             0))
                           (check-equal? expected (fsa->graph a* 0 #f #f)))
                (test-case "ndfa"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'M 'M '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'S 'S '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'F 'F '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list
                                              (edge '#hash((fontsize . 15) (label . (f))) 'M 'F)
                                              (edge '#hash((fontsize . 15) (label . (m))) 'S 'M)
                                              (edge '#hash((fontsize . 15) (label . (s))) 'F 'S))
                                             0))
                           (check-equal? expected (fsa->graph FSM 0 #f #f)))
                (test-case "pda"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'F 'F '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'M 'M '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'S 'S '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list
                                              (edge '#hash((fontsize . 15) (label . (((M b (a)) (M ε)) ((M a (b)) (M ε)) ((M b ε) (M (b))) ((M a ε) (M (a)))))) 'M 'M)
                                              (edge '#hash((fontsize . 15) (label . (((M ε ε) (F ε))))) 'M 'F)
                                              (edge '#hash((fontsize . 15) (label . (((S ε ε) (M ε))))) 'S 'M))
                                             0))
                           (check-equal? expected (fsa->graph pda-numa=numb 0 #f #f)))
                (test-case "tm"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'N 'N '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'Y 'Y '#hash((color . "black") (shape . "doubleoctagon")) 'accept)
                                              (node 'E 'E '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'D 'D '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'C 'C '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'B 'B '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'S 'S '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list
                                              (edge '#hash((fontsize . 15) (label . (((D @) (E R))))) 'D 'E)
                                              (edge '#hash((fontsize . 15) (label . (((D _) (N _))))) 'D 'N)
                                              (edge '#hash((fontsize . 15) (label . (((D a) (S a))))) 'D 'S)
                                              (edge '#hash((fontsize . 15) (label . (((C c) (D z))))) 'C 'D)
                                              (edge '#hash((fontsize . 15) (label . (((C _) (N _)) ((C a) (N a))))) 'C 'N)
                                              (edge '#hash((fontsize . 15) (label . (((B _) (N _)) ((B c) (N c))))) 'B 'N)
                                              (edge '#hash((fontsize . 15) (label . (((B b) (C z))))) 'B 'C)
                                              (edge '#hash((fontsize . 15) (label . (((E c) (N c)) ((E b) (N b)) ((E a) (N a))))) 'E 'N)
                                              (edge '#hash((fontsize . 15) (label . (((E _) (Y _))))) 'E 'Y)
                                              (edge '#hash((fontsize . 15) (label . (((S _) (Y _))))) 'S 'Y)
                                              (edge '#hash((fontsize . 15) (label . (((S z) (N z)) ((S c) (N c)) ((S b) (N b))))) 'S 'N)
                                              (edge '#hash((fontsize . 15) (label . (((S a) (B z))))) 'S 'B)
                                              (edge '#hash((fontsize . 15) (label . (((E z) (E R)) ((E @) (E R))))) 'E 'E)
                                              (edge '#hash((fontsize . 15) (label . (((D z) (D L)) ((D c) (D L)) ((D b) (D L)) ((D @) (D R))))) 'D 'D)
                                              (edge '#hash((fontsize . 15) (label . (((C z) (C R)) ((C b) (C R)) ((C @) (C R))))) 'C 'C)
                                              (edge '#hash((fontsize . 15) (label . (((B z) (B R)) ((B a) (B R)) ((B @) (B R))))) 'B 'B)
                                              (edge '#hash((fontsize . 15) (label . (((S @) (S R))))) 'S 'S))
                                             0))
                           (check-equal? expected (fsa->graph a^nb^nc^n 0 #f #f)))))

  (test-all 'verbose
            (graph-add-node
             graph-constructor
             graph-add-edge
             graph-render-graph))
  ) ;; end test module
