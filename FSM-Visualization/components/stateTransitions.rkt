#lang racket
(require "../../fsm-main.rkt" "../structs/world.rkt" "../globals.rkt")

(provide getCurRule)

(define getCurRule (lambda (processed-list)
                     (println processed-list)
                     (case MACHINE-TYPE
                       [(pda) (println "TODO")]
                       [(tm) (println "TODO")]
                       [else (get-dfa-ndfa-rule processed-list)])))


;;get-dfa-ndfa-rule: Returns the current rule for a dfa/ndfa
(define (get-dfa-ndfa-rule processed-list)
  (cond
    ;; If the processed list doesn't have at least 2 items
    ;;   in it then no rule was followed, so return an empty rule
    [(< (length processed-list) 2) (list 'empty 'empty 'empty)] 
    [(= (length (caar processed-list)) (length (caadr processed-list)))
     (list
      (cadadr processed-list)
      EMP
      (cadar processed-list))]
    [else
     (list
      (cadadr processed-list)
      (caaadr processed-list)
      (cadar processed-list))]))


(define (get-pda-rule processed-list)
  (cond
    [(< (length processed-list) 2) (list (list 'empty empty) (list 'empty 'empty))]
    [else (get-pda-parts processed-list)]))

(define (get-pda-parts pl)
  (let* ((first (car pl)) ;; The first of transition
         (second (cadr pl)) ;; The second transition
         (start-state (car first)) ;; The starting state
         (final-state (car second)) ;; The state that the transition ends in
         (rule (caddr second)) ;; The rule that the function transitions on
         )
    '((start-state )((final-state)))))



'(((a a a b a) A) ((a a b a) A) ((a b a) A) ((b a) A) ((a) B) (() C) reject)

'((S (a a a b b b) ())
  (M (a a a b b b) ())
  (M (a a b b b) (a))
  (M (a b b b) (a a))
  (M (b b b) (a a a))
  (M (b b) (a a))
  (M (b) (a))
  (M () ())
  (F () ())
  accept)

;; state input stack
'(((S e e) (M e)) ((M e e) (F e)) ((M a e) (M (a))) ((M b e) (M (b))) ((M a (b)) (M e)) ((M b (a)) (M e)))