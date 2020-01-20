#lang racket
(require "../../fsm-main.rkt" "../structs/world.rkt" "../globals.rkt")

(provide getCurRule)

(define getCurRule (lambda (processed-list)
                     ;;(println processed-list)
                     (case MACHINE-TYPE
                       [(pda) (get-pda-rule processed-list)]
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


;; get-pda-rule: processed-list -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (construct-pda-rule processed-list)]))

;; construct-pda-rule: processed-list -> pda-rule
;; Purpose: Constructes a pda rule from the given processed list
(define (construct-pda-rule pl)
  (letrec (
           (next-state (caar pl)) ;; The initial state that the machine is in
           (init-state (caadr pl)) ;; The state that the machien ends in
           (next-input (cadar pl)) ;; The initial state's input
           (init-input (cadadr pl)) ;; The state that the machien ends in input
           (next-stack (caddar pl)) ;; The elemetns that are on the init stack
           (sec (cadr pl))  ;; The second list in the stack
           (init-stack (caddr sec)) ;; The elements that are on the next stack

           ;; take*: Integer List -> List or symbol
           ;; Purpose: functions the same as Racket's take function except if the list
           ;;   result of take is the empty list then 'e is returned instead
           (take* (lambda (num a-list)
                    (let ((t (take a-list num)))
                      (if (empty? t) 'e t))))

           ;; determine-consumed: none -> symbol
           ;; Purpose: determins what the input is that is consumed
           (determin-consumed (lambda ()
                                (cond
                                  ;; If both inputs are equal then nothing was consumed
                                  [(equal? init-input next-input)'e]
                                  [else (car init-input)])))

           ;; determin-pushed: none -> integer
           ;; Purpose: Returns the number of elements that have been pushed on the stack
           (determin-pushed (lambda ()
                              (let ((num (- (length next-stack) (length init-stack))))
                                (if (< num 0) 0 num))))

           ;; determin-poped: none -> integer
           ;; Purpose: Returns the number of elements that have been poped off the stack
           (determin-poped (lambda ()
                             (let ((num (- (length init-stack) (length next-stack))))
                               (if (< num 0) 0 num)))))

    (cond
      ;; If there is less then 2 elements then we are at the ed so return the default
      [(< (length pl) 2) '((empty empty empty) (empty empty))]
      [else
       (list
        (list init-state (determin-consumed) (take* (determin-poped) init-stack))
        (list next-state (take* (determin-pushed) next-stack)))])))

#|

(sm-showtransitions P '(a a a b b b))
'((S (a a a b b b) ())   ;; (S e e) (F e)
  (F (a a a b b b) ())   ;; (F a e) (F (c))
  (F (a a b b b) (c))    ;; (F a e) (F (c))
  (F (a b b b) (c c))    ;; (F a e) (F (c))
  (F (b b b) (c c c))    ;; (F b (c)) (F e))
  (F (b b) (c c))        ;; (F b (c)) (F e))
  (F (b) (c))            ;; (F b (c)  (F e)
  (F () ())              ;; WTF IS THIS
  accept)


;; '(state symbol pop) '(state push)
'(((S e e) (F e))
 ((F a e) (F (c)))
 ((F b (c)) (F e)))
|#