#lang racket
(require
  "../../GraphViz/interface.rkt"
  "../test-helpers.rkt"
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
                                             (list (edge 'F 'F  '#hash((fontsize . 15) (label . (b a)))) (edge 'S 'F '#hash((fontsize . 15) (label . (a)))))
                                             0
                                             '#hash((rankdir . "LR"))))
                           (check-equal? expected (fsa->graph a* 0)))
                (test-case "ndfa"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'M 'M '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'S 'S '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'F 'F '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list
                                              (edge 'M 'F '#hash((fontsize . 15) (label . (f))))
                                              (edge 'S 'M '#hash((fontsize . 15) (label . (m))))
                                              (edge 'F 'S '#hash((fontsize . 15) (label . (s))) ))
                                             0
                                             '#hash((rankdir . "LR"))))
                           (check-equal? expected (fsa->graph FSM 0)))
                (test-case "pda"
                           (define expected (graph
                                             'G
                                             (list
                                              (node 'F 'F '#hash((color . "black") (shape . "doublecircle")) 'final)
                                              (node 'M 'M '#hash((color . "black") (shape . "circle")) 'none)
                                              (node 'S 'S '#hash((color . "forestgreen") (shape . "circle")) 'start))
                                             (list
                                              (edge 'M 'M '#hash((fontsize . 15) (label . (((M b (a)) (M ε)) ((M a (b)) (M ε)) ((M b ε) (M (b))) ((M a ε) (M (a)))))))
                                              (edge 'M 'F '#hash((fontsize . 15) (label . (((M ε ε) (F ε))))))
                                              (edge 'S 'M '#hash((fontsize . 15) (label . (((S ε ε) (M ε)))))))
                                             0
                                             '#hash((rankdir . "LR"))))
                           (check-equal? expected (fsa->graph pda-numa=numb 0)))
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
                                              (edge 'D 'E '#hash((fontsize . 15) (label . (((D @) (E R))))))
                                              (edge 'D 'N '#hash((fontsize . 15) (label . (((D _) (N _))))))
                                              (edge 'D 'S '#hash((fontsize . 15) (label . (((D a) (S a))))))
                                              (edge 'C 'D '#hash((fontsize . 15) (label . (((C c) (D z))))))
                                              (edge 'C 'N '#hash((fontsize . 15) (label . (((C _) (N _)) ((C a) (N a))))))
                                              (edge 'B 'N '#hash((fontsize . 15) (label . (((B _) (N _)) ((B c) (N c))))))
                                              (edge 'B 'C '#hash((fontsize . 15) (label . (((B b) (C z))))))
                                              (edge 'E 'N '#hash((fontsize . 15) (label . (((E c) (N c)) ((E b) (N b)) ((E a) (N a))))))
                                              (edge 'E 'Y '#hash((fontsize . 15) (label . (((E _) (Y _))))))
                                              (edge 'S 'Y '#hash((fontsize . 15) (label . (((S _) (Y _))))))
                                              (edge 'S 'N '#hash((fontsize . 15) (label . (((S z) (N z)) ((S c) (N c)) ((S b) (N b))))))
                                              (edge 'S 'B '#hash((fontsize . 15) (label . (((S a) (B z))))))
                                              (edge 'E 'E '#hash((fontsize . 15) (label . (((E z) (E R)) ((E @) (E R))))))
                                              (edge 'D 'D '#hash((fontsize . 15) (label . (((D z) (D L)) ((D c) (D L)) ((D b) (D L)) ((D @) (D R))))))
                                              (edge 'C 'C '#hash((fontsize . 15) (label . (((C z) (C R)) ((C b) (C R)) ((C @) (C R))))))
                                              (edge 'B 'B '#hash((fontsize . 15) (label . (((B z) (B R)) ((B a) (B R)) ((B @) (B R))))))
                                              (edge 'S 'S '#hash((fontsize . 15) (label . (((S @) (S R)))))))
                                             0
                                             '#hash((rankdir . "LR"))))
                           (check-equal? expected (fsa->graph a^nb^nc^n 0)))))

  (test-all 'verbose
            (graph-add-node
             graph-constructor
             graph-add-edge
             graph-render-graph))
  ) ;; end test module
