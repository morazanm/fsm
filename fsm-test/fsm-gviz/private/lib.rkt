#lang racket/base

(module+ test
  (require rackunit
           "../../../fsm-gviz/private/lib.rkt")

  (test-equal? "Nodes with spaces in names"
               (add-nodes (create-graph 'test) '(|A A| B C D E-1))
               (graph
                'test
                (list
                 (node 'A__A #hash((color . "black") (label . "A A") (shape . "circle")))
                 (node 'B #hash((color . "black") (label . "B") (shape . "circle")))
                 (node 'C #hash((color . "black") (label . "C") (shape . "circle")))
                 (node 'D #hash((color . "black") (label . "D") (shape . "circle")))
                 (node 'E1 #hash((color . "black") (label . "E-1") (shape . "circle"))))
                '()
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))


  (test-equal? "Edges with dashes"
               (add-edges (add-nodes (create-graph 'test) '(A B C D))
                          '((A a B) (B b B) (B c-1 D)))
               (graph
                'test
                (list
                 (node 'A #hash((color . "black") (label . "A") (shape . "circle")))
                 (node 'B #hash((color . "black") (label . "B") (shape . "circle")))
                 (node 'C #hash((color . "black") (label . "C") (shape . "circle")))
                 (node 'D #hash((color . "black") (label . "D") (shape . "circle"))))
                (list
                 (edge 'B 'D #hash((fontsize . 15) (label . (c-1))))
                 (edge 'B 'B #hash((fontsize . 15) (label . (b))))
                 (edge 'A 'B #hash((fontsize . 15) (label . (a)))))
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))

  (test-equal? "Node with custom label"
               (add-node (create-graph 'test) 'A #:atb (hash 'label "AA"))
               (graph
                'test
                (list (node 'A #hash((label . "AA"))))
                '()
                '()
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))


  (test-equal? "Subgraphs with edges"
               (add-edges (add-subgraph (add-nodes (create-graph 'test) '(A B))
                                        (add-nodes (create-subgraph #:name 'cluster1) '(AA BB)))
                          '((A a B)
                            (AA a B)
                            (cluster1 a B)))
               (graph
                'test
                (list
                 (node 'A #hash((color . "black") (label . "A") (shape . "circle")))
                 (node 'B #hash((color . "black") (label . "B") (shape . "circle"))))
                (list
                 (edge 'AA 'B #hash((fontsize . 15) (label . (a)) (ltail . cluster1)))
                 (edge 'AA 'B #hash((fontsize . 15) (label . (a))))
                 (edge 'A 'B #hash((fontsize . 15) (label . (a)))))
                (list
                 (subgraph
                  'cluster1
                  (list
                   (node 'AA #hash((color . "black") (label . "AA") (shape . "circle")))
                   (node 'BB #hash((color . "black") (label . "BB") (shape . "circle"))))
                  '()
                  '()
                  #hash()))
                DEFAULT-FORMATTERS
                DEFAULT-GRAPH
                '()))

  ;; --- Exception Checks Below ---
  (check-exn
   exn:fail?
   (lambda () (add-nodes (add-nodes (create-graph 'test) '(A B C D)) '(A))))
  
  (check-exn
   exn:fail?
   (lambda ()
     (define sg (create-subgraph #:name 'A))
     (add-subgraph (add-nodes (create-graph 'test) '(A B C D)) sg)))

  )