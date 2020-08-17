#lang racket
(require "../../fsm-main.rkt" "../structs/world.rkt" "../globals.rkt")

(provide getCurRule)

(define getCurRule (lambda (processed-list)
                     (case MACHINE-TYPE
                       [(pda) (get-pda-rule processed-list)]
                       [(tm) (get-tm-rule processed-list)]
                       [(tm-language-recognizer) (get-tm-rule processed-list)]
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


;; get-pda-rule: processed-list -> tm-rule
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
           ;; Purpose: Returns the number of elements that have been pushed on the stack
           (determin-pushed (lambda ()
                              (let ((num (- (length next-stack) (length init-stack))))
                                (cond
                                  [(and (equal? (length init-stack) (length next-stack))
                                        (not (equal? init-stack next-stack)))
                                   (num-dif init-stack next-stack)]
                                  [else
                                  (if (< num 0) 0 num)]))))

           ;; determin-poped: none -> integer
           ;; Purpose: Returns the number of elements that have been poped off the stack
           (determin-poped (lambda ()
                             (let ((num (- (length init-stack) (length next-stack))))
                               (cond
                                 [(and (equal? (length init-stack) (length next-stack))
                                       (not (equal? init-stack next-stack)))
                                  (num-dif init-stack next-stack)]
                                 [else 
                                  (if (< num 0) 0 num)])))))
    ;;(println init-stack)
    ;;(println next-stack)
    (cond
      ;; If there is less then 2 elements then we are at the end so return the default
      [(< (length pl) 2) '((empty empty empty) (empty empty))]
      [else
       (list
        (list init-state (determin-consumed) (take* (determin-poped) init-stack))
        (list next-state (take* (determin-pushed) next-stack)))])))