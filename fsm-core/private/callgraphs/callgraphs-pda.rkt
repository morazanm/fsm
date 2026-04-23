#lang racket/base
(require "../../../fsm-gviz/private/lib.rkt"
         "cg-defs.rkt"
         "mk-cg-edges-function.rkt"
         "../sm-getters.rkt"
         "../misc.rkt"
         "../constants.rkt"
         "../sm-apply.rkt"
         racket/list
         racket/string)
(provide make-pda-cg-edges dot-nodes-pda dot-trans-pda computation-diagram-pda)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pda-cg-edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine word -> (pda-Edge)
;; Purpose: Given a machine and a word, returns a list of edges
;;          including those that lead to a dead state
(define (make-pda-cg-edges M word threshold)
  (let* [(my-ds (generate-symbol 'ds (sm-states M)))]
    ;; (listof pda-Edge) (listof pda-stuci) (listof pda-stuci) -> (listof pda-stuci)
    ;; Purpose: Given edges and a list of pda-stucis, will create
    ;;          a new list of pda-stucis that contains the reached
    ;;          states and their respective unconsumed inputs
    ;; Accumulator Invariants:
    ;;  visited  = list of pda-stucis already explored 
    (define (add-to-stucis edges to-visit visited)
      ;; pda-Edge pda-stuci -> stack
      ;; Purpose: Given an edge and a pda-stuci, creates the new stack
      ;;          by popping and pushing
      (define (new-stack e stuci)
        (let* [(stack (pda-stuci-stack stuci))
               (pop-list (if (symbol? (pda-Edge-pop e))
                             '()
                             (pda-Edge-pop e)))
               (push-list (if (symbol? (pda-Edge-push e))
                              '()
                              (pda-Edge-push e)))] 
          (cond [(null? stack) (append push-list stack)]
                [(and (<= (length pop-list) (length stack))
                      (equal? (take stack (length pop-list)) pop-list)) 
                 (append push-list (drop stack (length pop-list)))]
                [else (append push-list stack)])))
      ;; (listof pda-Edge) pda-stuci -> (listof pda-stuci)
      ;; Purpose: Given edges and a pda-stuci, creates all
      ;;          new pda-stucis associated with the transitions
      ;;          following the given pda-stuci
      (define (add-to-stucis-helper edges stuci)
        (let* [(pda-stuci-empty-ui
                (lambda (r) (pda-stuci (pda-Edge-tost r)
                                       (pda-stuci-ui stuci)
                                       (new-stack r stuci)
                                       (add1 (pda-stuci-int stuci)))))
               (pda-stuci-any-ui
                (lambda (r) (pda-stuci (pda-Edge-tost r)
                                       (if (eq? (pda-Edge-read r)
                                                (car (pda-stuci-ui stuci)))
                                           (cdr (pda-stuci-ui stuci))
                                           (pda-stuci-ui stuci))
                                       (new-stack r stuci)
                                       (add1 (pda-stuci-int stuci)))))               
               (empty-rules
                (lambda (r) (and (eq? (pda-Edge-read r) EMP)
                                 (eq? (pda-Edge-fromst r) (pda-stuci-state stuci)))))
               (empty-or-nonempty-rules
                (lambda (r) (and (or (eq? (pda-Edge-read r) (car (pda-stuci-ui stuci)))
                                     (eq? (pda-Edge-read r) EMP))
                                 (eq? (pda-Edge-fromst r) (pda-stuci-state stuci)))))] 
          (if (null? (pda-stuci-ui stuci))
              (map pda-stuci-empty-ui
                   (filter empty-rules
                           edges))
              (map pda-stuci-any-ui
                   (filter empty-or-nonempty-rules
                           edges)))))
      (cut-off
       (if (null? to-visit)
           '()
           (let* [(new-stucis (add-to-stucis-helper edges (car to-visit)))]
             (if (null? new-stucis)
                 (add-to-stucis edges (cdr to-visit) visited)
                 (append (filter-non-member new-stucis visited)
                         (add-to-stucis edges (cdr to-visit)
                                        (append new-stucis
                                                visited))))))
       threshold))
    ;; (listof stuci) (listof stuci) -> (listof stuci)
    ;; Purpose: Filter out stucis already visited
    (define (filter-non-member s v)
      (cond ((null? s) '())
            ((ormap (lambda (x) (pda-stucis-equal? (car s) x)) v)
             (filter-non-member (cdr s) v))
            (else (cons (car s) (filter-non-member (cdr s) v)))))
    ;; pda-stuci -> (listof pda-Edge)
    ;; Purpose: Given a pda-stuci, creates all possible edges for that
    ;;          pda-stuci
    (define (add-to-edges stuci)
      ;; rule -> Boolean
      ;; Purpose: Given a rule, determines whether it can be popped off the stack
      (define (can-be-popped? r)
        (let* [(stack (pda-stuci-stack stuci))
               (pop (pda-rule-pop r))]
          (cond [(eq? EMP pop) #t]
                [(null? stack) #f]
                [(and (<= (length pop) (length stack))
                      (equal? pop (take stack (length pop)))) #t]
                [else #f])))
      (let* [(empty-rules
              (lambda (r) (and (eq? EMP (pda-rule-read r))
                               (can-be-popped? r)
                               (eq? (pda-stuci-state stuci) (pda-rule-fromst r)))))
             (nonempty-rules
              (lambda (r) (and (eq? (pda-stuci-state stuci) (pda-rule-fromst r))
                               (can-be-popped? r)
                               (not (or (and (null? (pda-stuci-stack stuci))
                                             (equal? EMP (pda-rule-push r)))
                                        (and (not (null? (pda-stuci-stack stuci)))
                                             (equal? (pda-stuci-stack stuci) (pda-rule-pop r)))))
                               (eq? (car (pda-stuci-ui stuci)) (pda-rule-read r)))))
             (nonempty-rules-empty-stack-empty-push
              (lambda (r) (and (eq? (pda-stuci-state stuci) (pda-rule-fromst r))
                               (can-be-popped? r)
                               (and (null? (pda-stuci-stack stuci))
                                    (equal? EMP (pda-rule-push r)))
                               (eq? (car (pda-stuci-ui stuci)) (pda-rule-read r)))))
             (nonempty-rules-nonempty-stack-nonempty-push
              (lambda (r) (and (eq? (pda-stuci-state stuci) (pda-rule-fromst r))
                               (can-be-popped? r)
                               (and (not (null? (pda-stuci-stack stuci)))
                                    (equal? (pda-stuci-stack stuci) (pda-rule-pop r)))
                               (eq? (car (pda-stuci-ui stuci)) (pda-rule-read r)))))
             (empty-or-nonempty-rules
              (lambda (r) (or (and (eq? (pda-stuci-state stuci) (pda-rule-fromst r))
                                   (can-be-popped? r)
                                   (eq? (car (pda-stuci-ui stuci)) (pda-rule-read r)))
                              (and (eq? (pda-stuci-state stuci) (pda-rule-fromst r))
                                   (can-be-popped? r)
                                   (eq? EMP (pda-rule-read r))))))
             (empty-trans?
              (lambda (e) (eq? EMP (pda-Edge-read e))))
             (stack (pda-stuci-stack stuci))
             (pop-stack
              (remove-duplicates
               (map (lambda (r) (pda-spedge my-ds EMP (list r) my-ds EMP))
                    stack)))
             (cutoff/spedge
              pda-spedge)
             (cutoff/edge
              (if (< (pda-stuci-int stuci) (- threshold 1))
                  pda-edge
                  cutoff-edge))
             (cutoff-or-spedge
              (lambda (r)
                (cutoff/spedge (pda-rule-fromst r)
                               (pda-rule-read r)
                               (pda-rule-pop r)
                               (pda-rule-tost r)
                               (pda-rule-push r))))
             (cutoff-or-edge
              (lambda (r)
                (cutoff/edge (pda-rule-fromst r)
                             (pda-rule-read r)
                             (pda-rule-pop r)
                             (pda-rule-tost r)
                             (pda-rule-push r))))]    
        (cond [(and (null? (pda-stuci-ui stuci))
                    (null? (pda-stuci-stack stuci)))
               (map (lambda (r) (if (eq? EMP (pda-rule-push r))
                                    (cutoff-or-spedge r)
                                    (cutoff-or-edge r)))
                    (filter empty-rules
                            (sm-rules M)))]
              [(null? (pda-stuci-ui stuci))
               (let* [(empty-rules-edge
                       (map (lambda (r) (if (equal? (pda-stuci-stack stuci) (pda-rule-pop r))
                                            (cutoff-or-spedge r)
                                            (cutoff-or-edge r)))
                            (filter empty-rules
                                    (sm-rules M))))]
                 (cond [(and (null? empty-rules-edge)
                             (equal? (pda-stuci-state stuci) my-ds))
                        pop-stack]
                       [(null? empty-rules-edge)  
                        (append (list (cutoff-or-spedge (list (list (pda-stuci-state stuci) EMP EMP) (list my-ds EMP))))
                                pop-stack)]
                       [else empty-rules-edge]))]
              [else
               (let* [(new-rules-ui-length=1
                       (append (map cutoff-or-spedge 
                                    (filter nonempty-rules-empty-stack-empty-push
                                            (sm-rules M)))
                               (map cutoff-or-spedge 
                                    (filter nonempty-rules-nonempty-stack-nonempty-push
                                            (sm-rules M)))
                               (map cutoff-or-edge  
                                    (filter nonempty-rules
                                            (sm-rules M))) 
                               (map cutoff-or-edge  
                                    (filter empty-rules
                                            (sm-rules M)))))
                      (new-rules-ui-length>1
                       (map cutoff-or-edge  
                            (filter empty-or-nonempty-rules                     
                                    (sm-rules M))))
                      (ui-length=1? (= 1 (length (pda-stuci-ui stuci))))
                      (new-rules (if ui-length=1?
                                     new-rules-ui-length=1
                                     new-rules-ui-length>1))]
                 (cond [(and (null? new-rules)
                             (equal? (pda-stuci-state stuci) my-ds))
                        (append (list (cutoff-or-spedge (list (list my-ds (car (pda-stuci-ui stuci)) EMP) (list my-ds EMP))))
                                pop-stack)]
                       [(and (andmap empty-trans? new-rules)
                             (equal? (pda-stuci-state stuci) my-ds))
                        (append new-rules
                                pop-stack)]
                       [(null? new-rules)
                        (append (list (cutoff-or-spedge (list (list (pda-stuci-state stuci) EMP EMP) (list my-ds EMP)))
                                      (cutoff-or-spedge (list (list my-ds (car (pda-stuci-ui stuci)) EMP) (list my-ds EMP))))
                                pop-stack)]
                       [(andmap empty-trans? new-rules)
                        (append new-rules
                                (list (cutoff-or-spedge (list (list (pda-stuci-state stuci) EMP EMP) (list my-ds EMP)))
                                      (cutoff-or-spedge (list (list my-ds (car (pda-stuci-ui stuci)) EMP) (list my-ds EMP))))
                                pop-stack)]
                       [else new-rules]))])))            
    ;; (listof pda-stuci) (listof pda-stuci) -> (listof pda-Edge)
    ;; Purpose: Given a list of pda-stucis, appends all possible
    ;;          edges in the machine
    ;; How: The function starts with one start pda-stuci with the full word and an empty stack,
    ;;      and finds all edges associated with that pda-stuci. Then, the function
    ;;      takes the new generated edges to find the reached pda-stucis with adjusted
    ;;      unconsumed input and stack. Next, it recursively appends the old edges
    ;;      to newly found edges associated with the new pda-stucis and so on, until
    ;;      the function halts. 
    ;; Accumulator Invariants:
    ;;  to-visit = list of stucis
    ;;  visited  = list of all stucis ever visited 
    (define (pda-computation-tree->cg-edges to-visit visited)
      (let* [(new-edges (append-map (lambda (s) (add-to-edges s)) to-visit))
             (new-stucis (add-to-stucis (remove-duplicates new-edges) to-visit visited))]
        (if (null? new-stucis)
            new-edges
            (append
             new-edges
             (pda-computation-tree->cg-edges new-stucis (append to-visit visited))))))
    ;; Termination Argument:
    ;;  Given a start pda-stuci, the function will generate the edges associated with
    ;;  that pda-stuci, and then the new pda-stucis associated with the new edges, and so on.
    ;;  The functions add-to-edges and add-to-stucis are mutually recursive. However,
    ;;  at some point the unconsumed input will be empty, all possible rules will have
    ;;  been found, and all pda-stuci stacks will be empty. pda-stucis cannot be examined
    ;;  repeatedly, and there is a cutoff once a certain computation length is reached,
    ;;  which avoids an infinite loop. Once there are no edges left to create new pda-stucis,
    ;;  the function appends these last set of edges one more time, and halts. 
    ;; (listof pda-Edge) -> (listof pda-Edge)
    ;; Purpose: Given a list of edges removes duplicates. The pda-Edge subtypes are prioritized
    ;;          as follows (from most to least): cutoff-spedge, cutoff-edge and pda-spedge, pda-edge
    (define (pda-remove-duplicate-Edges Edges)
      (let* [(only-pda-edges (filter pda-edge? Edges))
             (only-pda-spedges (filter pda-spedge? Edges))
             (only-cutoff-edges (filter cutoff-edge? Edges))
             (new-pda-edges
              (filter
               (lambda (r) (not (ormap (lambda (r2) (pda-Edges-equal? r r2))
                                       (append only-cutoff-edges
                                               only-pda-spedges))))
               only-pda-edges))
             (new-pda-spedges
              (filter
               (lambda (r) (not (ormap (lambda (r2) (pda-Edges-equal? r r2))
                                       only-cutoff-edges)))
               only-pda-spedges))
             (new-cutoff-edges
              (filter
               (lambda (r) (not (ormap (lambda (r2) (pda-Edges-equal? r r2))
                                       only-pda-spedges)))
               only-cutoff-edges))
             (cutoff-spedges
              (map
               (lambda (E)
                 (cutoff-spedge (pda-Edge-fromst E)
                                (pda-Edge-read E)
                                (pda-Edge-pop E)
                                (pda-Edge-tost E)
                                (pda-Edge-push E)))
               (filter
                (lambda (r) (ormap (lambda (r2) (pda-Edges-equal? r r2))
                                   only-pda-spedges))
                only-cutoff-edges)))]
        (append
         new-pda-edges
         new-pda-spedges
         new-cutoff-edges
         cutoff-spedges))) 
    ;; (listof pda-Edge) -> (listof edge/pda-Edge)
    ;; Purpose: Given the finished list of edges, extracts the redundant transitions
    ;;          if the machine accepts the word. Only the edges are left that are
    ;;          essential for the word to get accepted. Only the last transition remains a pda-spedge.
    (define (pda-remove-redundant-edges-on-accept edges)
      ;; (listof sm-showtransitions) -> (listof pda-Edge)
      ;; Purpose: Makes pda-edges and a pda-spedge from sm-showtransitions
      (define (edges-on-accept st)
        (let* [(fromst (car (car st)))
               (ui-from (cadr (car st)))
               (stack-from (caddr (car st)))]      
          (if (and (and (member fromst (sm-finals M))
                        (null? stack-from)
                        (null? ui-from))
                   (eq? (cadr st) 'accept))
              '()
              (let* [(tost (car (cadr st)))
                     (ui-to (cadr (cadr st)))
                     (stack-to (caddr (cadr st)))
                     (read (if (eq? ui-from ui-to)
                               'ε
                               (car ui-from)))
                     (pop-and-push-empty/equal?
                      (filter (lambda (e) (or (and (eq? 'ε (pda-Edge-pop e))
                                                   (eq? 'ε (pda-Edge-push e)))
                                              (equal? (pda-Edge-pop e) (pda-Edge-push e))))
                              edges))
                     (pop-empty-push-any?
                      (filter (lambda (e) (and (eq? 'ε (pda-Edge-pop e))
                                               (or (eq? 'ε (pda-Edge-push e))
                                                   (and (>= (length stack-to) (length (pda-Edge-push e)))
                                                        (equal? (take stack-to (length (pda-Edge-push e))) (pda-Edge-push e))))))
                              edges))
                     (push-empty-pop-any?
                      (filter (lambda (e) (and (eq? 'ε (pda-Edge-push e))
                                               (or (eq? 'ε (pda-Edge-pop e))
                                                   (and (>= (length stack-from) (length (pda-Edge-pop e)))
                                                        (equal? (take stack-from (length (pda-Edge-pop e))) (pda-Edge-pop e))))))
                              edges))
                     (valid-edges
                      (and (cond [(and (null? stack-from)
                                       (null? stack-to)) pop-and-push-empty/equal?]
                                 [(null? stack-from) pop-empty-push-any?]
                                 [(null? stack-to) push-empty-pop-any?]
                                 [else (and pop-empty-push-any?
                                            push-empty-pop-any?)])
                           (filter (lambda (e) (and (eq? (pda-Edge-fromst e) fromst)
                                                    (eq? (pda-Edge-tost e) tost)))
                                   edges)))] 
                ;; pda-Edge -> Boolean
                ;; Purpose: Given one of the valid edges, determines whether it can be used 
                (define (use-edge? v-edge)
                  (cond [(and (eq? 'ε (pda-Edge-pop v-edge))
                              (eq? 'ε (pda-Edge-push v-edge)))
                         (equal? stack-to stack-from)]
                        [(eq? 'ε (pda-Edge-pop v-edge))
                         (equal? stack-to (append (pda-Edge-push v-edge) stack-from))]
                        [(eq? 'ε (pda-Edge-push v-edge))
                         (equal? stack-from (append (pda-Edge-pop v-edge) stack-to))]
                        [else
                         (equal? stack-to (append (pda-Edge-push v-edge) (drop stack-from (length (pda-Edge-pop v-edge)))))]))
                ;; (listof pda-Edge) -> pda-Edge
                ;; Purpose: Given a list of valid edges, finds the one that can be used to turn stack-from into stack-to
                (define (find-edge v-edges)
                  (if (use-edge? (car v-edges))
                      (car v-edges)
                      (find-edge (cdr v-edges))))
                (let* [(pop (pda-Edge-pop (find-edge valid-edges)))
                       (push (pda-Edge-push (find-edge valid-edges)))] 
                  (if (and (member tost (sm-finals M))
                           (null? stack-to)
                           (null? ui-to)) 
                      (cons (pda-spedge fromst read pop tost push)
                            (edges-on-accept (cdr st)))
                      (cons (pda-edge fromst read pop tost push)
                            (edges-on-accept (cdr st)))))))))
      (cond [(ormap (lambda (r) (member (pda-Edge-tost r) (sm-finals M))) 
                    (append (filter pda-spedge? edges)
                            (filter cutoff-spedge? edges)))
             (remove-duplicates (edges-on-accept (sm-showtransitions M word)))]
            [(and (member (sm-start M) (sm-finals M))
                  (null? word))
             '()]
            [else edges]))
    ;; (listof pda-stuci) natnum -> (listof pda-stuci)
    ;; Purpose: Given a list of pda-stucis and a threshold, filters out pda-stucis that reach given threshold
    ;; Accumulator invariants: 
    ;;  stucis = list of pda-stucis
    (define (cut-off stucis threshold)
      (filter (lambda (s) (< (pda-stuci-int s) threshold)) stucis))
    (mk-cg-edges-function
     pda-computation-tree->cg-edges
     (pda-stuci (sm-start M) word '() 0)
     pda-remove-redundant-edges-on-accept
     pda-remove-duplicate-Edges)))

;.................................................

;; M word (listof pda-Edge) optargs -> (listof node)
;; Purpose: Given a machine, its edges, and a word, creates a list of
;;          nodes in dot format
;; Color blindness:
;;  'default - default colors
;;  'deut    - Deuteranopia
(define (dot-nodes-pda M word new-rules color-blindness)
  (let* [(start-state (sm-start M))
         (edge-states (remove-duplicates (append (map (lambda (r) (pda-Edge-fromst r)) new-rules)
                                                 (map (lambda (r) (pda-Edge-tost r)) new-rules))))
         (all-states
          (if (null? edge-states)
              (list (sm-start M))
              edge-states))
         (end-states
          (remove-duplicates
           (append (map (lambda (r) (pda-Edge-tost r))
                        (filter pda-spedge? new-rules))
                   (map (lambda (r) (pda-Edge-tost r))
                        (filter cutoff-spedge? new-rules)))))
         (cutoff-states
          (remove-duplicates
           (append (map (lambda (r) (pda-Edge-tost r))
                        (filter cutoff-edge? new-rules))
                   (map (lambda (r) (pda-Edge-tost r))
                        (filter cutoff-spedge? new-rules)))))
         (final-states (sm-finals M))]
    ;; (listof state) -> (listof node)
    ;; Purpose: Given a list of all states in a machine after a word
    ;;          has been consumed, returns a list of nodes 
    (define (new-node los)
      (if (null? los)
          '()
          (let* [(a-color
                  (cond [(= 1 (length all-states)) "crimson"]
                        [(member (car los) end-states) "crimson"]
                        [(and (eq? color-blindness 'default)
                              (eq? (car los) start-state)) "forestgreen"]
                        [(and (eq? color-blindness 'deut)
                              (eq? (car los) start-state)) "dodgerblue"]
                        [else "black"]))
                 (a-shape
                  (cond [(member (car los) final-states) "doublecircle"]
                        [else "circle"]))
                 (a-label
                  (symbol->string (car los)))
                 (a-fontcolor
                  (cond [(or (and (eq? (car los) start-state)
                                  (member (car los) end-states))
                             (= 1 (length all-states)))
                         (cond [(eq? color-blindness 'default) "forestgreen"]
                               [(eq? color-blindness 'deut) "dodgerblue"])]
                        [else "black"]))
                 (a-fillcolor
                  (if (member (car los) cutoff-states)
                      "gold"
                      "white"))]
            (cons (list (car los) `((color ,a-color) (shape ,a-shape) (label ,a-label) (fontcolor ,a-fontcolor) (fillcolor ,a-fillcolor))) 
                  (new-node (cdr los))))))
    (new-node all-states)))

;.................................................

;; (listof pda-Edge) -> (listof trans)
;; Purpose: Given a machines's edges, creates a list of
;;          transitions in dot format
(define (dot-trans-pda new-rules)
  ;; (listof pda-Edge) -> (listof trans)
  ;; Purpose: Given a list of edges, creates
  ;;          a list of transitions
  (define (all-trans loes)
    ;; (listof component) -> (listof trans)
    ;; Purpose: Given a list of components within an edge, returns a transition
    (define (new-trans loc)      
      (list (pda-Edge-fromst loc) (pda-Edge-tost loc) `((fontsize 15) (label ,(string-append "[" (symbol->string (pda-Edge-read loc))
                                                                                             (format " ~a ~a" (pda-Edge-pop loc) (pda-Edge-push loc)) "]")))))
    (if (null? loes)
        '()
        (cons (new-trans (car loes))
              (all-trans (cdr loes)))))
  (all-trans new-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .dot files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; computation-diagram-pda

;; (listof string) -> string 
;; creates a string where each value in the list is on a new line
(define (one-rule-per-line rules)
  (string-join rules "\n"))

(define graph-formatters (formatters
                          (hash)                            ; graph level formatters
                          (hash)                            ; node level formatters
                          (hash 'label one-rule-per-line))) ; edge level formatters

;.................................................

;; Optional Arguments is either:
;; 1. (list symbol)         -> contains symbol for color blindness
;; 2. (list integer)        -> contains number for cut-off threshold
;; 3. (list symbol integer) -> contains symbol for color blindness and number for cut-off threshold

;; pda word [optargs] -> image
;; Purpose: Given a machine and a word, creates a .png file from
;;          a .dot file, and returns a bitmap.
;; Color blindness:
;;  'default - default colors
;;  'deut - Deuteranopia
;; Threshold:
;;  8 - default 
;;  2 - min 
(define (computation-diagram-pda M word . optargs)
  (define fname "fsm")
  ;; Definitions for Threshold
  (define DEFAULT-THRESH 25)
  (define MIN-THRESH 2)
  (define threshold
    (cond [(null? optargs) DEFAULT-THRESH]
          [(and (integer? (car optargs))
                (>= (car optargs) MIN-THRESH))
           (car optargs)]
          [(and (> (length optargs) 1)
                (integer? (cadr optargs))
                (>= (cadr optargs) MIN-THRESH))
           (cadr optargs)]
          [else DEFAULT-THRESH]))
  ;; Definitions for Color Blindness
  (define DEFAULT-COLOR 'default)
  (define DEUTERANOPIA 'deut)
  (define COLOR-LIST (list DEFAULT-COLOR DEUTERANOPIA))
  (define color-blindness 
    (cond [(null? optargs) DEFAULT-COLOR]
          [(and (symbol? (car optargs))
                (member (car optargs) COLOR-LIST))
           (car optargs)]
          [(and (> (length optargs) 1)
                (symbol? (cadr optargs))
                (member (cadr optargs) COLOR-LIST))
           (cadr optargs)]
          [else DEFAULT-COLOR]))
  (let* [(new-rules (make-pda-cg-edges M word threshold))]
    ;; image
    ;; Purpose: Stores a computation graph image
    (define cgraph (create-graph 'cgraph #:atb (hash 'rankdir "LR" 'label (cond [(not (null? (append (filter cutoff-edge? new-rules)
                                                                                                      (filter cutoff-spedge? new-rules))))
                                                                                 (format "All computations on '~a cut off at threshold ~a" word threshold)]
                                                                                [(or (ormap (lambda (r) (member (pda-Edge-tost r) (sm-finals M))) 
                                                                                            (append (filter pda-spedge? new-rules)
                                                                                                    (filter cutoff-spedge? new-rules)))
                                                                                     (and (member (sm-start M) (sm-finals M))
                                                                                          (null? word)))
                                                                                 (format "Machine accepts: '~a" word)]
                                                                                [else (format "Machine rejects: '~a" word)])
                                                     'fontsize 13)
                                 #:fmtrs (formatters (hash) (hash) (hash 'label one-rule-per-line)))) 
    (begin
      (set! cgraph
            (foldl
             (lambda (a-node a-graph)
               (let* [(state (car a-node))
                      (color (cadr (car (cadr a-node))))
                      (shape (cadr (cadr (cadr a-node))))
                      (label (cadr (caddr (cadr a-node)))) 
                      (fontcolor (cadr (cadddr (cadr a-node))))
                      (style "filled")
                      (fillcolor (cadr (car (cddddr (cadr a-node)))))]
                 (add-node a-graph state #:atb (hash 'color color 'shape shape 'label label 'fontcolor fontcolor 'style style 'fillcolor fillcolor)))) 
             cgraph   
             (dot-nodes-pda M word new-rules color-blindness)))
      (set! cgraph
            (foldl
             (lambda (a-trans a-graph)
               (let* [(state1 (car a-trans))
                      (state2 (cadr a-trans))
                      (fontsize (cadr (car (caddr a-trans))))
                      (label (cadr (cadr (caddr a-trans))))
                      (style (if (member state2 (sm-states M))
                                 "solid"
                                 "dashed"))] 
                 (add-edge a-graph label state1 state2 #:atb (hash 'fontsize fontsize 'style style))))
             cgraph
             (dot-trans-pda new-rules)))
      (let [(res (graph->bitmap cgraph))]
        res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

