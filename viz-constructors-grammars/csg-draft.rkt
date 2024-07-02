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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define anbn (make-unchecked-csg '(S A B)
                                 '(a b)
                                 (list (list 'S ARROW 'AaB)
                                       (list 'AaA ARROW 'aSb)
                                       (list 'AaA ARROW EMP)
                                       (list 'B ARROW 'A)
                                       )
                                 'S))



;; csg-viz




#;(create-edges (make-yields (rest (csg-derive-edited anbn '(a b)))
                             (list (second (first (rest (csg-derive-edited anbn '(a b))))))
                             (first (first (rest (csg-derive-edited anbn '(a b)))))
                             (second (first (rest (csg-derive-edited anbn '(a b)))))
                             (make-hash)) (edges '() '() '()))

#|

'((S || ||)
  (AaB S AaB)
  (AaA B A)
  (aSb AaA aSb)
  (aAaBb S AaB)
  (aAaAb B A)
  (ab AaA ε))

|#


(define E-SCENE (empty-scene 1250 600))
(struct viz-state (pimgs upimgs))


;; yield is a structure that has
;; before - everything on the left side of substituted part of the yield
;; to-subst - the part of the yield that got substituted with the rules
;; after - everything on the right side of substituted part of the yield
;; subst - whatever replaced to-subst in the yield
;; taken - names of the nodes that are already taken
;; ny - new yield created from renamed subst to be used in creating the next yield
;; old - yield not renamed
(struct yield (before to-subst after subst taken old ny) #:transparent)

;; edges is a structure that has
;; hex which are hexagon edges
;; yield which are edges that are not hexes and are in the current yield of the grammar
;; all which are hex and yield edges appended and grouped how they appear in the graph
(struct edges (hex yield all) #:transparent)

;; find-index-right
;; subst yield -> (listof node)
;; Purpose: To find the index of where the lhs starts in a yield from the right
(define (find-index-right subst yield)
  (define (window-function window symbols accum)
    (cond
      [(empty? window) accum]
      [(empty? symbols) 0]
      [(equal? (take symbols (length window)) window)
       accum] 
      [else
       (window-function window (cdr symbols) (add1 accum))])) 
  
  (window-function (reverse subst) (reverse yield) 0))


;; find-index-left
;; subst yield -> (listof node)
;; Purpose: To find the index of where the lhs starts in a yield from the right
(define (find-index-left subst yield)
  (define (window-function window symbols accum)
    (cond
      [(empty? window) accum]
      [(empty? symbols) 0]
      [(equal? (take symbols (length window)) window)
       accum] 
      [else
       (window-function window (cdr symbols) (add1 accum))])) 
  
  (window-function subst yield 0))

;; rename-symbols
;; symbol (listof symbol) -> (listof symbol)
;; Purpose: To rename the symbols in the substituted part of the yield if needed
(define (rename-symbols-old subst accum)
  (if (empty? subst)
      empty
      (if (member (first subst) accum)
          (cons (generate-symbol (first subst) accum)
                (rename-symbols-old (rest subst) accum))
          (cons (first subst) (rename-symbols-old (rest subst) accum)))))

(define (rename-symbols nt hashtb)
  (let [(result (hash-ref hashtb nt #f))] 
    (if result
        (begin (hash-set! hashtb nt (add1 result))
               (string->symbol (format "~s~s" nt (add1 result))))
        (begin (hash-set! hashtb nt 0)
               (string->symbol (format "~s0" nt)))))
  )


;; TODO: ON THE RIGHT TRACK, ADD THE UPDATED YIELD AS A PARAMETER SO WE CAN EXTRACT TO-SUB
;; AND USE IT TO MAKE HEXES


(define (symbol->csgsymb-helper str-list prev prev-nums symbols-list)
  (if (empty? str-list)
      (cons (string->symbol (string-append prev prev-nums)) symbols-list)
      (if (or (equal? (first str-list) "0")
              (equal? (first str-list) "1")
              (equal? (first str-list) "2")
              (equal? (first str-list) "3")
              (equal? (first str-list) "4")
              (equal? (first str-list) "5")
              (equal? (first str-list) "6")
              (equal? (first str-list) "7")
              (equal? (first str-list) "8")
              (equal? (first str-list) "9")
              )
          (symbol->csgsymb-helper (rest str-list) prev (string-append prev-nums (first str-list)) symbols-list)
          (symbol->csgsymb-helper (rest str-list) (first str-list) "" (cons (string->symbol (string-append prev prev-nums)) symbols-list))
          )
      )
  )
                                                           
                                              
(define (symbol->csglos symbol)
  (let [
        (str-list (map string (string->list (symbol->string symbol))))
        ]
    (reverse (symbol->csgsymb-helper (rest str-list) (first str-list) "" '()))
    )
  )
;; before-after-subst
;; level -> struct
;; Purpose: To extract before, to-subst, after, and subst from the level
(define (before-after-substs level accum updated-yield old-up-yield hashtb)
  (let* [
         ;(test3 (displayln (format "third level: ~s" (third level))))
         (sub  (los->symbol (map (lambda (symb) (rename-symbols symb hashtb)) (symbol->fsmlos (third level)))))
         ;(test (displayln (format "sub: ~s" sub)))
         
         (bef (los->symbol (take (symbol->fsmlos updated-yield)
                                 (find-index-left (symbol->csglos (second level))
                                                  (symbol->csglos updated-yield)))))
         ;(test0 (displayln (format "before: ~s" bef)))
         (aft (los->symbol (take-right (symbol->fsmlos updated-yield)
                                       (find-index-right (symbol->csglos (second level))
                                                         (symbol->csglos updated-yield)))))
         ;(test1 (displayln (format "after: ~s" aft)))
         (tak (append (symbol->csglos sub) accum))
         (new-yield (los->symbol (list bef sub aft)))
         (to-sub (drop-right (drop (symbol->csglos old-up-yield)
                                   (find-index-left (symbol->csglos (second level))
                                                    (symbol->csglos updated-yield)))
                             (find-index-right (symbol->csglos (second level))
                                               (symbol->csglos updated-yield))))
         ;(test5 (displayln (format "to-sub ~s" to-sub)))
         ]
    (yield bef to-sub aft sub tak (first level) new-yield)))


;; make-yields
;; der accum -> (listof yield)
;; Purpose: To create a list of yields to use for building edges
(define (make-yields der accum previous-yield updated-yield hashtb)
  (if (empty? der)
      empty
      (let [(new-yield (before-after-substs (first der) accum previous-yield updated-yield hashtb))]
        (cons new-yield
              (make-yields (rest der) (yield-taken new-yield) (yield-old new-yield) (yield-ny new-yield) hashtb)))))

;; compute-hexes
;; yield accum -> accum
;; Purpose: To compute hexes
(define (compute-hexes a-yield accum hexes)
  (let* [(s-exploded (symbol->fsmlos (yield-subst a-yield)))]
    (remove-duplicates (append (map (λ (edge) (if (member (second edge) (yield-to-subst a-yield))
                                                  (list (first edge) (los->symbol (yield-to-subst a-yield)))
                                                  edge))
                                    accum)
                               (reverse hexes))
                       )))


;; group-edges
;; (listof node) (listof edge) -> (listof level)
(define (group-edges lon loe)
  (remove-duplicates (if (empty? lon)
                         empty
                         (cons (filter (λ (edge) (equal? (first lon) (first edge)))
                                       loe)
                               (group-edges (rest lon) loe)))))


;; create-levels
;; (listof edge) -> (listof edge)
;; Purpose: To arrange edges into levels
(define (create-levels loe)
  (group-edges (map (λ (edge) (first edge)) loe)
               loe))

;; create-single-level
;; yield (listof edge) -> edges
;; Purpose: To create edges of a single step using new yield and accum
(define (create-single-level a-yield a-nl)
  (let* [(new-accum-edges (filter (λ (edge) (not (member (second edge)
                                                         (yield-to-subst a-yield))))
                                  (append (map (λ (x) (flatten (list (los->symbol
                                                                      (yield-to-subst a-yield)) x)))
                                               (symbol->csglos (yield-subst a-yield)))
                                          (edges-yield a-nl))))
         (hexes (reverse (filter (λ (edge) (not (member edge new-accum-edges)))
                                 (compute-hexes a-yield
                                                (edges-yield a-nl)
                                                (edges-hex a-nl)))))
         ]
    (edges hexes new-accum-edges (create-levels (append hexes new-accum-edges)))))

;; create-edges
;; (listof yield) -> (listof edges)
;; Purpose: To create the edges from the list of yields
(define (create-edges loy edge-accum)
  (if (empty? loy)
      empty
      (let [(new-level (create-single-level (first loy) edge-accum))]
        (cons new-level (create-edges (rest loy) new-level)))))


#;(symbol? (second (second (edges-yield (first (create-edges (make-yields (list '(AaB S AaB)
                                                                                '(AaA B A)
                                                                                '(aSb AaA aSb)
                                                                                '(aAaBb S AaB)
                                                                                '(aAaAb B A)
                                                                                '(ab AaA ε)) '(S) 'AaB 'S (make-hash)) (edges '() '() '())))))))





;; create-graph-imgs
;; (listof edges) -> (listof img)
;; Purpose: To create the graph images from a list of edges
(define (create-graph-imgs edges)
  (let* [(all-edges (flatten (edges-all edges)))
         (hexes (edges-hex edges))
         (yield (edges-yield edges))
         (nodes (extract-nodes-from-levels all-edges))]
                           


    ;; draw-world
    ;; viz-state -> img
    ;; Purpose: To render the given viz-state
    (define (draw-world a-vs)
      (overlay (first (viz-state-pimgs a-vs)) E-SCENE)
      (overlay (first (viz-state-pimgs a-vs)) E-SCENE))

    ;; csg-viz-test
    ;; fsa -> void
    (define (csg-viz-test csg w)
      (let* [(der (csg-derive-edited anbn '(a b)))
             (edges (create-edges (make-yields (rest (csg-derive-edited csg w))
                                               (list (second (first (rest (csg-derive-edited csg w)))))
                                               (first (first (rest (csg-derive-edited csg w))))
                                               (second (first (rest (csg-derive-edited csg w))))
                                               (make-hash)) (edges '() '() '())))
             (loimgs (create-graph-imgs edges))]
        (run-viz (viz-state loimgs (list (make-init-grph-img der))) draw-world 'csg-viz-test)))



    