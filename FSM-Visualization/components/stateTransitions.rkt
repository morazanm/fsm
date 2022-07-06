#lang racket
(require "../../fsm-main.rkt"
         "../structs/world.rkt"
         "../globals.rkt")

(provide getCurRule)

(define getCurRule (lambda (processed-list #:debug [debug #f])
                     (case MACHINE-TYPE
                       [(pda)
                        (get-pda-rule processed-list debug)]
                       [(tm)
                        (get-tm-rule processed-list)]
                       [(tm-language-recognizer)
                        (get-tm-rule processed-list)]
                       [(mttm)
                        (get-mttm-rule processed-list)]
                       [(mttm-language-recognizer)
                        (get-mttm-rule processed-list)]
                       [else
                        (get-dfa-ndfa-rule processed-list)])))


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


;; get-tm-rule processed-list -> tm-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-tm-rule processed-list)
  (cond
    [(< (length processed-list) 2) '((empty empty) (empty empty))]
    [else (construct-tm-rule processed-list)]))


;; construct-tm-rule: processed-list -> tm-rule
;; Purpose: Constructs the current tm rule based on the processed list
(define (construct-tm-rule pl)
  (let* ( (cur-trans (cadr pl))  ;; The current transiton
          (next-trans (car pl))  ;; The next transition
          (cur-state (car cur-trans)) ;; The current state the machine is in
          (cur-tape-index (cadr cur-trans)) ;; The tape index the machine is in 
          (cur-tape (caddr cur-trans)) ;; The input the machine has
          (next-state (car next-trans)) ;; The next state the machine goes to 
          (next-tape-index (cadr next-trans)) ;; The new tape index the machine goes to
          (next-tape (caddr next-trans)) ;; The new tape the machine has

          (cur-tape-element (list-ref cur-tape cur-tape-index)) ;; The currently highlights element
          (next-tape-element (list-ref next-tape next-tape-index))) ;; The next highlighted element

    (cond
      [(cur-tape-index . > . next-tape-index) ;; moved to left
       (list (list cur-state cur-tape-element) (list next-state LEFT))]
      [(cur-tape-index . < . next-tape-index) ;; moved to right
       (list (list cur-state cur-tape-element) (list next-state RIGHT))]
      [else                                   ;;statyed in same posn
       (list (list cur-state cur-tape-element) (list next-state next-tape-element))])))

;; get-mttm-rule processed-list -> mttm-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-mttm-rule pl)
  (cond
    [(< (length pl) 2) '((empty (empty empty empty)) (empty (empty empty empty)))]
    [else (construct-mttm-rule pl)]))

;; construct-mttm-rule :: processed-list -> mttm-rule
;; Purpose: Constructs the current mttm rule based on the processed list
(define (construct-mttm-rule pl)
  (define cur-trans (cadr pl)) ;; The current transiton
  (define next-trans (car pl)) ;; The next transition
  (displayln pl))


;; get-pda-rule: processed-list -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list debug)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (construct-pda-rule processed-list debug)]))

;; construct-pda-rule: processed-list -> bool -> pda-rule
;; Purpose: Constructes a pda rule from the given processed list
(define (construct-pda-rule pl debug)
  (letrec (
           (next-state (caar pl)) ;; The initial state that the machine is in
           (init-state (caadr pl)) ;; The state that the machien ends in
           (next-input (cadar pl)) ;; The initial state's input
           (init-input (cadadr pl)) ;; The state that the machine ends in input
           (next-stack (caddar pl)) ;; The elemetns that are on the next stack
           (sec (cadr pl))  ;; The second list in the stack
           (init-stack (caddr sec)) ;; The elements that are on the init stack

           ;; take*: Integer List -> List or symbol
           ;; Purpose: functions the same as Racket's take function except if the list
           ;;   result of take is the empty list then EMP is returned instead
           (take* (lambda (num a-list)
                    (let ((t (take a-list num)))
                      (if (empty? t) EMP t))))

           (num-dif (lambda (l1 l2)
                      (cond [(empty? l1) 0]
                            [(equal? (car l1) (car l2)) (num-dif (cdr l1) (cdr l2))]
                            [else (+ 1 (num-dif (cdr l1) (cdr l2)))])))

           ;; determine-consumed: none -> symbol
           ;; Purpose: determins what the input is that is consumed
           (determin-consumed (lambda ()
                                (cond
                                  ;; If both inputs are equal then nothing was consumed
                                  [(equal? init-input next-input) EMP]
                                  [else (car init-input)])))

           ;; determin-pushed: none -> integer
           ;; Purpose: Returns the list or elements to be pushed
           (determin-pushed (lambda (prev-stack next-stack)
                              (letrec [(helper
                                        (lambda (rev-p-stack rev-n-stack)
                                          (cond
                                            [(empty? rev-n-stack) EMP]
                                            [(empty? rev-p-stack) (reverse rev-n-stack)]
                                            [(not (eq? (car rev-p-stack) (car rev-n-stack))) (reverse rev-n-stack)]
                                            [else 
                                             (helper (cdr rev-p-stack) (cdr rev-n-stack))])))]
                                (helper (reverse prev-stack) (reverse next-stack)))))

           ;; determin-poped: list -> list -> list
           ;; Purpose: Returns the list or elements to be popped
           (determin-poped (lambda (prev-stack next-stack)
                             (letrec [(helper
                                       (lambda (rev-p-stack rev-n-stack)
                                         (cond
                                           [(empty? rev-p-stack) EMP]
                                           [(empty? rev-n-stack) (reverse rev-p-stack)]
                                           [(not (eq? (car rev-p-stack) (car rev-n-stack))) (reverse rev-p-stack)]
                                           [else 
                                            (helper (cdr rev-p-stack) (cdr rev-n-stack))])))]
                               (helper (reverse prev-stack) (reverse next-stack))))))
                                  
    (when debug
      (displayln "---Stacks are:---")
      (displayln init-stack)
      (displayln next-stack)
      (displayln "------"))
    (cond
      ;; If there is less then 2 elements then we are at the end so return the default
      [(< (length pl) 2) '((empty empty empty) (empty empty))]
      [else
       (list
        (list init-state (determin-consumed) (determin-poped init-stack next-stack))
        (list next-state (determin-pushed init-stack  next-stack)))])))