#lang racket

(require "../fsm-gviz/private/lib.rkt"
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/interface.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/misc.rkt"
         "viz.rkt"
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; csg-viz

#|

'((S || ||)
  (AaB S AaB)
  (AaA B A)
  (aSb AaA aSb)
  (aAaBb S AaB)
  (aAaAb B A)
  (ab AaA ε))

|#

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
;; accum which are edges that need to be saved in the accum because they might change
(struct edges (hex accum) #:transparent)

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
(define (rename-symbols subst accum)
  (if (empty? subst)
      empty
      (if (member (first subst) accum)
          (cons (generate-symbol (first subst) accum)
                (rename-symbols (rest subst) accum))
          (cons (first subst) (rename-symbols (rest subst) accum)))))


;; TODO: ON THE RIGHT TRACK, ADD THE UPDATED YIELD AS A PARAMETER SO WE CAN EXTRACT TO-SUB
;; AND USE IT TO MAKE HEXES

;; before-after-subst
;; level -> struct
;; Purpose: To extract before, to-subst, after, and subst from the level
(define (before-after-substs level accum updated-yield old-up-yield)
  (let* [(sub (los->symbol (rename-symbols (symbol->fsmlos (third level)) accum)))
         (bef (los->symbol (take (symbol->fsmlos updated-yield)
                                 (find-index-left (symbol->fsmlos (second level))
                                                  (symbol->fsmlos updated-yield)))))
         (aft (los->symbol (take-right (symbol->fsmlos updated-yield)
                                       (find-index-right (symbol->fsmlos (second level))
                                                         (symbol->fsmlos updated-yield)))))
          
         (tak (append (symbol->fsmlos sub) accum))
         (new-yield (los->symbol (list bef sub aft)))
         (to-sub (drop-right (drop (symbol->fsmlos old-up-yield)
                                   (find-index-left (symbol->fsmlos (second level))
                                                    (symbol->fsmlos updated-yield)))
                             (find-index-right (symbol->fsmlos (second level))
                                               (symbol->fsmlos updated-yield))))
         (dd (display (format "~s\n\n" to-sub)))
         ]
    (yield bef to-sub aft sub tak (first level) new-yield)))


;; make-yields
;; der accum -> (listof yield)
;; Purpose: To create a list of yields to use for building edges
(define (make-yields der accum previous-yield updated-yield)
  (if (empty? der)
      empty
      (let [(new-yield (before-after-substs (first der) accum previous-yield updated-yield))]
        (cons new-yield
              (make-yields (rest der) (yield-taken new-yield) (yield-old new-yield) (yield-ny new-yield))))))

;; compute-hexes
;; yield accum -> accum
;; Purpose: To compute hexes
(define (compute-hexes a-yield accum hexes)
  (let* [(s-exploded (symbol->fsmlos (yield-subst a-yield)))]
    (remove-duplicates (append (map (λ (edge) (if (member (second edge) (yield-to-subst a-yield))
                                                  (list (first edge) (los->symbol (yield-to-subst a-yield)))
                                                  edge))
                                    accum)
                               hexes)
                       )))

;; create-single-level
;; yield (listof edge) -> edges
;; Purpose: To create edges of a single step using new yield and accum
(define (create-single-level a-yield a-nl)
  (let* [(new-accum-edges (filter (λ (edge) (not (member (second edge)
                                                         (yield-to-subst a-yield))))
                                  (append (map (λ (x) (flatten (list (los->symbol
                                                                      (yield-to-subst a-yield)) x)))
                                               (symbol->fsmlos (yield-subst a-yield)))
                                          (edges-accum a-nl))))
         (hexes (filter (λ (edge) (not (member edge new-accum-edges)))
                        (compute-hexes a-yield
                                       (edges-accum a-nl)
                                       (edges-hex a-nl))))
         ]
    (edges hexes new-accum-edges)))

;; create-edges
;; (listof yield) -> (listof edges)
;; Purpose: To create the edges from the list of yields
(define (create-edges loy edge-accum)
  (if (empty? loy)
      empty
      (let [(new-level (create-single-level (first loy) edge-accum))]
        (cons new-level (create-edges (rest loy) new-level)))))















