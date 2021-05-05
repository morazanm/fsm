#lang racket
(require "render-graph.rkt" "lib.rkt" (for-syntax racket/syntax syntax/stx syntax/parse))
(require rackunit/text-ui)
#|
  This file houses the test for the GraphViz library and fsm interface files
  This file was created by: Josh Schappel 5/5/21
|#

(define TEST-STYLE 'verbose)

;; Lazy mans testing
(define-syntax (test-all stx)
  (syntax-parse stx
    [(_ (val:id ...))
     #:with (calls ...) (stx-map (lambda (test-name) #`(run-tests #,test-name TEST-STYLE)) #`(val ...))
     #`(begin
         calls ...)]))


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
                       'shape "circle"))

  ;; -------------------------------
  ;; --------- create graph --------
  ;; ------------------------------- 
  (define graph-constructor
    (test-suite "Create Graph Function"
                (test-case "Default"
                           (define test-graph (create-graph "test"))
                           (check-equal? (graph-name test-graph) "test")
                           (check-equal? (graph-node-list test-graph) '())
                           (check-equal? (graph-edge-list test-graph) '())
                           (check-equal? (graph-color-blind test-graph) 0))
                (test-case "Color Blind"
                           (define graph-cb (create-graph "test-colorblind" #:color 2))
                           (check-equal? (graph-name graph-cb) "test-colorblind")
                           (check-equal? (graph-node-list graph-cb) '())
                           (check-equal? (graph-edge-list graph-cb) '())
                           (check-equal? (graph-color-blind graph-cb) 2))))

  ;; -------------------------------
  ;; --------- add node ------------
  ;; -------------------------------
  (define graph-add-node
    (test-suite "Add Node Function"
                (test-case "Default"
                           (define test-graph (create-graph "test"))
                           (add-node test-graph 'A 'none)
                           (check-equal? (length (graph-node-list test-graph)) 1)
                           (check-equal? (node-name (car (graph-node-list test-graph))) 'A)
                           (check-equal? (node-type (car (graph-node-list test-graph))) 'none)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) DEFAULT-NODE))
                (test-case "Start Node"
                           (define test-graph (create-graph "test"))
                           (add-node test-graph 'A 'start)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) START-NODE))
                (test-case "Final Node"
                           (define test-graph (create-graph "test"))
                           (add-node test-graph 'A 'final)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) END-NODE))
                (test-case "Startfinal Node"
                           (define test-graph (create-graph "test"))
                           (add-node test-graph 'A 'startfinal)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) STARTFINAL-NODE))
                (test-case "Acept Node"
                           (define test-graph (create-graph "test"))
                           (add-node test-graph 'A 'accept)
                           (check-equal? (node-atb (car (graph-node-list test-graph))) ACCEPT-NODE))))

  ;; -------------------------------
  ;; --------- add edge ------------
  ;; -------------------------------
  (define graph-add-edge
    (test-suite "Add Edge Function"
                (test-case "Default"
                           (define test-graph (create-graph "test"))
                           (define expected-atb (hash
                                                 'fontsize 15
                                                 'label (list 'a)))
                           (add-edge test-graph 'a 'A 'B)
                       
                           (check-equal? (edge-atb (car (graph-edge-list test-graph))) expected-atb)
                           (check-equal? (edge-start-node (car (graph-edge-list test-graph))) 'A)
                           (check-equal? (edge-end-node (car (graph-edge-list test-graph))) 'B))
                (test-case "Dashed"
                           (define test-graph (create-graph "test"))
                           (define expected-atb (hash
                                                 'fontsize 15
                                                 'label (list 'a)))
                           (add-edge test-graph 'a 'A-1 'B-2)
                       
                           (check-equal? (edge-atb (car (graph-edge-list test-graph))) expected-atb)
                           (check-equal? (edge-start-node (car (graph-edge-list test-graph))) 'A1)
                           (check-equal? (edge-end-node (car (graph-edge-list test-graph))) 'B2))))





  (test-all
   (graph-add-node
    graph-constructor
    graph-add-edge))
  ) ;; end test module
