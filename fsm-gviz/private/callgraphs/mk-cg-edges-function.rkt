#lang fsm

(provide mk-cg-edges-function)

;; -> (listof edges)
(define (mk-cg-edges-function computation-tree->cg-edges init-config
                              rm-edges-on-accept remove-unprioritized-edges)
  ;; -> (listof Edge)
  ;; Purpose: Return computation-graph edges for given
  ;;          machine and word
  (define (ct->cg-edges)
    ((compose remove-unprioritized-edges
              rm-edges-on-accept
              remove-duplicates
              computation-tree->cg-edges)
     (list init-config)
     '()))
  (ct->cg-edges))

