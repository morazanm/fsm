#lang racket

(require "../fsm-gviz/private/lib.rkt"
         "../fsm-core/private/csg.rkt"
         "../fsm-core/interface.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "viz.rkt"
         "csg-example.rkt"
         2htdp/universe
         rackunit
         (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen])
         2htdp/image
         )

(define anbn (make-unchecked-csg '(S A B)
                                 '(a b)
                                 (list (list 'S ARROW 'AaB)
                                       (list 'AaA ARROW 'aSb)
                                       (list 'AaA ARROW EMP)
                                       (list 'B ARROW 'A)
                                       )
                                 'S))

;; symbol -> symbol
;; Just removes number renaming that we have to do for graphviz and returns the original symbol
(define (undo-renaming symb) (string->symbol
                              (list->string
                               (filter (lambda (x) (not (or (equal? #\0 x)
                                                            (equal? #\1 x)
                                                            (equal? #\2 x)
                                                            (equal? #\3 x)
                                                            (equal? #\4 x)
                                                            (equal? #\5 x)
                                                            (equal? #\6 x)
                                                            (equal? #\7 x)
                                                            (equal? #\8 x)
                                                            (equal? #\9 x)
                                                            )
                                                        )
                                         )
                                       (string->list (symbol->string symb))
                                       )
                               )
                              )
  )

;; (listof symbols) (listof rules) MutableHashtable
;; Returns the levels for each graph needed to display the derivation
(define (generate-levels curr-state rules used-names)
  (local [
          ;; list list -> num
          ;; Purpose: Returns the index of the the begining of wherever the sublst matches in the original lst
          (define (index-of lst sublst)
            (local [(define sublst-length (length sublst))
                    (define (index-of-helper lst sublst idx)
                      (if (empty? lst)
                          -1
                          (if (equal? (take lst sublst-length) sublst)
                              idx
                              (index-of-helper (rest lst) sublst (add1 idx))
                              )
                          )
                      )
                    ]
              (index-of-helper lst sublst 0)
              )
            )

          ;; rename-symbols
          ;; symbol (listof symbol) -> (listof symbol)
          ;; Purpose: To rename the symbols in the substituted part of the yield if needed
          (define (rename-symbols nt hashtb)
            (let [(result (hash-ref hashtb nt #f))] 
              (if result
                  (begin (hash-set! hashtb nt (add1 result))
                         (string->symbol (format "~s~s" nt (add1 result))))
                  (begin (hash-set! hashtb nt 0)
                         (string->symbol (format "~s0" nt)))))
            )

          ;; (listof symbols) (listof rules) MutableHashtable (listof symbol) (listof symbol) (listof edges)
          ;; Returns the levels for each graph needed to display the derivation
          (define (generate-levels-helper curr-state rules used-names hex-nodes yield-nodes levels)
            (if (empty? (first (first rules)))
                (list hex-nodes yield-nodes levels)
                (let* [
                       (curr-rule (first rules))
                       (idx-of-replaced (index-of (map undo-renaming curr-state) (symbol->fsmlos (first curr-rule))))
                       (replaced-str-length (string-length (symbol->string (first curr-rule))))
                       (before-replacement (take curr-state idx-of-replaced))
                       (before-replacement-removed (drop curr-state idx-of-replaced))
                       (removed (take before-replacement-removed replaced-str-length))
                       (removed-combined-symbol (rename-symbols (first curr-rule) used-names))
                       (replacement-symbols (map (lambda (x) (rename-symbols x used-names)) (symbol->fsmlos (second curr-rule))))
                       (after-removed (drop before-replacement-removed replaced-str-length))                                               
                       ]
                  (generate-levels-helper (append before-replacement replacement-symbols after-removed)
                                          (rest rules)
                                          used-names
                                          (cons (remove-duplicates (flatten (cons removed-combined-symbol hex-nodes)))
                                                (cons (remove-duplicates (flatten (cons removed-combined-symbol hex-nodes))) hex-nodes))
                                          (cons (append before-replacement replacement-symbols after-removed)
                                                (cons (append before-replacement replacement-symbols after-removed) yield-nodes))
                                          (cons (foldr (lambda (val accum) (cons (list val removed-combined-symbol) accum)) '() removed)
                                                (cons (foldr (lambda (val accum) (cons (list removed-combined-symbol val) accum)) '() replacement-symbols)
                                                      levels
                                                      )
                                                )
                                          )
                  )
                )
            )
          ]
    (map reverse (generate-levels-helper curr-state rules used-names '() '() '()))
    )
  )


;; deriv-with-rules -> deriv-with-rules
;; Purpose: This is just taking the list received and moving the rules like such:
;; '( ((S) ()) ((AbA) (AbA)) )
;; where the rule applied was next to where it was applied, to:
;; '( ((S) (AbA)) ((AbA) (AaAbA)) )
;; now the rule is next to where it will be applied
(define (move-rule-applications-in-list lst)
  (if (= (length lst) 1)
      (list (list (first (first lst)) '() '()))
      (cons (list (first (first lst)) (second (second lst)) (third (second lst)))
            (move-rule-applications-in-list (rest lst)))
      ))

(csg-derive-edited anbn '(a a b b))
;  (map (lambda (x) (list (second x) (third x))) (move-rule-applications-in-list (csg-derive-edited anbn '(a a b b))))
;  (generate-levels (list 'S) (map (lambda (x) (list (second x) (third x))) (move-rule-applications-in-list (csg-derive-edited anbn '(a a b b)))) (make-hash))



;; dgrph is a structure that has
;; up-levels - unprocessed levels
;; p-levels - processed levels
;; nodes - nodes in the graph
;; up-hex-nodes - unprocessed hex nodes
;; p-hex-nodes - processed hex nodes
;; up-yield-nodes - unprocessed yield nodes
;; p-yield-nodes - processed yield nodes
;; hedges - highlighted edges of the graphs
;; up-rules - unprocessed grammar rules
;; p-rules - processed grammar rules
(struct dgrph (up-levels p-levels nodes up-hex-nodes p-hex-nodes up-yield-nodes p-yield-nodes hedges up-rules p-rules))

(define HEDGE-COLOR 'red)
(define YIELD-COLOR 'pink)
(define FONT-SIZE 12)

;; extract-nodes
;; (listof level) -> (listof node)
;; Purpose: To extract nodes from the list of edges - check if this is right
(define (extract-nodes loe)
  (remove-duplicates (flatten loe)))

;; make-node-graph
;; graph lon -> graph
;; Purpose: To make a node graph
(define (make-node-graph graph lon hedge-nodes hex-nodes yield-node)
  (foldl (λ (state result)
           (begin
             (displayln (format "nodes: ~s" state))
             (add-node
              result
              state
              #:atb (hash 'color (cond [(member state hedge-nodes)
                                        HEDGE-COLOR]
                                       [(member state yield-node)
                                        YIELD-COLOR]
                                       [else 'black])
                          'shape (cond [(member state hex-nodes)
                                        'hexagon]
                                       [else 'circle]
                                       )
                          'label  (symbol->string (undo-renaming state))
                          'fontcolor 'black
                          'font "Sans"))
             )
           )
         graph
         (reverse lon)))

;; make-edge-graph
;; graph (listof level) -> graph
;; Purpose: To make an edge graph
(define (make-edge-graph graph loe hedges)
  (foldl (lambda (rules result)
           (begin
             ;(displayln (format "edge rule: ~s" rules))
             (if (empty? (first rules))
                 result
                 (foldl (lambda (rule result)
                          (add-edge result
                                    ""
                                    (first rule)
                                    (second rule)
                                    #:atb (hash 'fontsize FONT-SIZE
                                                'style 'solid
                                                'color (if (member rule hedges)
                                                           HEDGE-COLOR
                                                           'black))))
                        result
                        rules))
             )
           )
         graph
         (reverse loe))
  )

;; create-dgraphs
;; dgrph (listof dgrph) boolean -> (listof dgrph)
;; Purpose: To create all the dgrphs for graph imgs
(define (create-dgrphs a-dgrph lod hex?)
  (if (empty? (dgrph-up-levels a-dgrph))
      (cons a-dgrph lod)
      (let* [(new-up-levels (rest (dgrph-up-levels a-dgrph)))
             (test (displayln (format "new-up: ~s" new-up-levels)))
             (new-ad-levels (cons (first (dgrph-up-levels a-dgrph))
                                  (dgrph-p-levels a-dgrph)))
                 
             (new-nodes (extract-nodes new-ad-levels))
             (new-up-hex-nodes (rest (dgrph-up-hex-nodes a-dgrph)))
             (new-p-hex-nodes (cons (first (dgrph-up-hex-nodes a-dgrph)) (dgrph-p-hex-nodes a-dgrph)))
             ;(new-hex-nodes (first (first (dgrph-up-levels a-dgrph))))
             (new-up-yield-nodes (rest (dgrph-up-yield-nodes a-dgrph)))
             (new-p-yield-nodes (cons (first (dgrph-up-yield-nodes a-dgrph)) (dgrph-p-yield-nodes a-dgrph)))
             ;(new-yield-nodes (second (first (dgrph-up-levels a-dgrph))))
             (new-hedges (first (dgrph-up-levels a-dgrph)))
             (new-up-rules (dgrph-up-rules a-dgrph))
             (new-p-rules (dgrph-p-rules a-dgrph))]
        (if hex?
            (let [
                  (new-up-hex-nodes (rest (dgrph-up-hex-nodes a-dgrph)))
                  (new-p-hex-nodes (cons (first (dgrph-up-hex-nodes a-dgrph)) (dgrph-p-hex-nodes a-dgrph)))
                  (new-up-yield-nodes (rest (dgrph-up-yield-nodes a-dgrph)))
                  (new-p-yield-nodes (cons (first (dgrph-up-yield-nodes a-dgrph)) (dgrph-p-yield-nodes a-dgrph)))
                  (new-up-rules (rest (dgrph-up-rules a-dgrph)))
                  (new-p-rules (cons (first (dgrph-up-rules a-dgrph))
                                     (dgrph-p-rules a-dgrph)))
                  ]
              (create-dgrphs
               (dgrph new-up-levels                      
                      new-ad-levels
                      new-nodes
                      new-up-hex-nodes
                      new-p-hex-nodes
                      new-up-yield-nodes
                      new-p-yield-nodes
                      new-hedges
                      new-up-rules
                      new-p-rules
                      )
               (cons a-dgrph lod)
               #f
               )
              )
            (let [
                  (new-up-hex-nodes (dgrph-up-hex-nodes a-dgrph))
                  (new-p-hex-nodes (dgrph-p-hex-nodes a-dgrph))
                  (new-up-yield-nodes (dgrph-up-yield-nodes a-dgrph))
                  (new-p-yield-nodes (dgrph-p-yield-nodes a-dgrph))
                  (new-up-rules (dgrph-up-rules a-dgrph))
                  (new-p-rules (dgrph-p-rules a-dgrph))
                  ]
              (create-dgrphs
               (dgrph new-up-levels                      
                      new-ad-levels
                      new-nodes
                      new-up-hex-nodes
                      new-p-hex-nodes
                      new-up-yield-nodes
                      new-p-yield-nodes
                      new-hedges
                      new-up-rules
                      new-p-rules
                      )
               (cons a-dgrph lod)
               #t
               )
              )
            )
        )
      )
  )

;; create-graph-structs
;; dgprh -> img
;; Purpose: Creates the final graph structure that will be used to create the images in graphviz
(define (create-graph-structs a-dgrph)
  (let* [(nodes (dgrph-nodes a-dgrph))
         (levels (reverse (map reverse (dgrph-p-levels a-dgrph))))
         (hedges (dgrph-hedges a-dgrph))
         (test (displayln (format "hedges: ~s" hedges)))
         (hedge-nodes (map (λ (x) (if (empty? x)
                                      '()
                                      (second x)))
                           hedges))
         #;(yield-node (map (λ (x) (if (empty? x)
                                       '()
                                       (first x)))
                            hedges))
         (test (displayln (format "hnodes: ~s" (dgrph-up-hex-nodes a-dgrph))))
         (yield-nodes (first (dgrph-up-yield-nodes a-dgrph)))
         (hex-nodes (first (dgrph-up-hex-nodes a-dgrph)))
         ]
    (make-edge-graph (make-node-graph (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans" 'ordering "in"))
                                      nodes hedge-nodes hex-nodes yield-nodes)
                     levels hedges)))

(define moved-rules (map (lambda (x) (list (second x) (third x))) (move-rule-applications-in-list (csg-derive-edited anbn '(a a b b)))))
(define rules (cons ""
                    (foldr (lambda (x accum) (if (empty? (first x))
                                                 '()
                                                 (append (list (string-append (symbol->string (first x)) " → " (symbol->string (second x)))) accum)
                                                 )
                             )
                           '()
                           moved-rules
                           )
                    )
  )
(define (filter-even-indexed-items-from-list lst)
  (local [
          (define (helper lst even?) (if (empty? lst)
                                         '()
                                         (if even?
                                             (helper (rest lst) #f)
                                             (cons (first lst) (helper (rest lst) #t))
                                             )
                                         )
            )
          ]
    (helper lst #t)
    )
  )
;(println rules)
(define renamed (generate-levels (list 'S) moved-rules (make-hash)))
;(define renamed (list (first renamed0) (second renamed0) (filter-even-indexed-items-from-list (third renamed0))))
;(third renamed)
;(map displayln (first renamed))
;(println (second renamed))
;(println (third renamed))

;(up-levels p-levels nodes up-hex-nodes p-hex-nodes up-yield-nodes p-yield-nodes hedges up-rules p-rules)
(length (third renamed))
(length (second renamed))
(length (first renamed))
(define dgraph (dgrph (third renamed) '() '() (first renamed) '() (second renamed) '()  '() (rest rules) (list (first rules))))
(define lod (reverse (create-dgrphs dgraph '() #f)))
(length lod)
(map graph->bitmap (filter-even-indexed-items-from-list (map create-graph-structs (rest lod))))
