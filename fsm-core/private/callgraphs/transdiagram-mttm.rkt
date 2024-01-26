#lang racket
(require 2htdp/image)
(require "../../../fsm-gviz/private/lib.rkt" "cg-defs.rkt"
         "../sm-apply.rkt" "../sm-getters.rkt")

(provide transition-diagram-mttm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multitape-tm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M -> (listof node)
;; Purpose: Generate a list of nodes
(define (dot-nodes M)
  (let* ((start-state (sm-start M))
         (final-states (if (not (eq? 'mttm-language-recognizer (sm-type M)))
                           (sm-finals M)                      
                           (filter (lambda (x) (not (eq? x (sm-accept M)))) (sm-finals M))))
         (dd (displayln (format "~s\n~s" (sm-finals M) final-states)))
         (rest-states (filter (lambda (x) (and (not (member x (append (list start-state) final-states)))
                                               (not (eq? x (sm-accept M))))) (sm-states M)))
         (accept-state (if (eq? 'mttm-language-recognizer (sm-type M))
                           (sm-accept M)
                           '())))
    (append
     (list (list start-state `((color "forestgreen") (shape "circle") (label ,start-state))))
     (map (lambda (x) (list x `((color "black") (shape "doublecircle") (label ,x)))) final-states)
     (map (lambda (x) (list x `((color "black") (shape "circle") (label ,x)))) rest-states)
     (if (not (null? accept-state)) (list (list accept-state `((color "black") (shape "doubleoctagon") (label ,accept-state)))) '()))))

;.................................................

;; list -> string
;; Purpose: Convert given list to a string
(define (list->string2 l)
  (if (empty? l)
      ""
      (string-append (symbol->string (car l))
                     (if (not (empty? (cdr l))) " " "")
                     (list->string2 (cdr l)))))

;; M -> (listof edge)
;; Purpose: Generate a list of edges
(define (dot-edges M)
  ;; (listof trans) -> (listof edge)
  ;; Purpose: Convert one transition into edges
  (define (edge l)
    (let* ((fromst (car (car l)))
           (tost (car (cadr l)))
           (read (cadr (car l)))
           (action (cadr (cadr l)))
           (labell (string-append "[(" (list->string2 read) ")(" (list->string2 action) ")]")))
      (list fromst tost `((fontsize 15) (label ,labell)))))
  (map edge (sm-rules M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; fsm word -> image
;; Purpose: Given a mttm as list, create a .png file from a .dot file, and return a bitmap
(define (transition-diagram-mttm M)
  (define fname "fsm")
  ;; (listof string) -> string 
  ;; creates a string where each value in the list is on a new line
  (define (one-rule-per-line rules)
    (string-join rules "\n"))
  ;; image
  ;; Purpose: Store a graph image 
  (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'fontsize 13)
                               #:fmtrs (formatters (hash) (hash) (hash 'label one-rule-per-line))))
  (begin
    (set! cgraph
          (foldl
           (lambda (a-node a-graph)
             (let* [(state (first a-node))
                    (color (second (first (second a-node))))
                    (shape (second (second (second a-node))))
                    (label (second (third (second a-node))))]
               (add-node a-graph state #:atb (hash 'color color 'shape shape 'label label)))) 
           cgraph   
           (dot-nodes M)))
    (set! cgraph
          (foldl
           (lambda (a-trans a-graph)
             (let* [(state1 (first a-trans))
                    (state2 (second a-trans))
                    (label (second (second (third a-trans))))] 
               (add-edge a-graph label state1 state2)))
           cgraph
           (dot-edges M)))
    (let [(res (graph->bitmap cgraph))]
      res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





