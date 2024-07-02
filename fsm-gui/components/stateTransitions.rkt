#lang racket
(require
  "../globals.rkt"
  "../../fsm-core/interface.rkt")

(provide getCurRule)

;; getCurRule: processed-list optional(listof rules) -> rule
;; Determins what the current rule is from the processed-list
;; The rules arg is only used for PDA's. See contruct-pda-rule for more info
;; on why this is needed.
(define (getCurRule processed-list (rules #f) (transitions #f))
  (match MACHINE-TYPE
    ['pda
     (get-pda-rule processed-list rules transitions)]
    [(or 'tm 'tm-language-recognizer)
     (get-tm-rule processed-list)]
    [(or 'mttm 'mttm-language-recognizer)
     (get-mttm-rule processed-list)]
    [_ ; dfa/ndfa
     (get-dfa-ndfa-rule processed-list)]
    )
  )


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
    [(< (length pl) 2) '(null null null)]
    [else (construct-mttm-rule pl)]))

;; construct-mttm-rule :: processed-list -> mttm-rule
;; Purpose: Constructs the current mttm rule based on the processed list
(define (construct-mttm-rule pl)
  (match-define `(,cur-state ,cur-tapes ...) (cadr pl)) ;; The state that the machine is in
  (match-define `(,next-state ,next-tapes ...) (car pl)) ;; The next state that the machine is in
  (define tuple-list
    (map (match-lambda* [`((,cur-pos ,cur-tape) (,next-pos ,_))
                         #:when (< cur-pos next-pos)
                         ;; tape incriments so we move Right
                         (cons (list-ref cur-tape cur-pos) RIGHT)]
                        [`((,cur-pos ,cur-tape) (,next-pos ,_))
                         #:when (> cur-pos next-pos)
                         ;; tape decriments so we move LEFT
                         (cons (list-ref cur-tape cur-pos) LEFT)]
                        ;; Otherwise we write to the tape
                        [`((,cur-pos ,cur-tape) (,next-pos ,next-tape))
                         (cons (list-ref cur-tape cur-pos)
                               (list-ref next-tape next-pos))])
         cur-tapes
         next-tapes))
  `((,cur-state ,(map car tuple-list)) (,next-state ,(map cdr tuple-list))))

(define (format-helper rule) (if (empty? rule)
                                 '()
                                 (if (eq? '() (first rule))
                                     (cons EMP (format-helper (rest rule)))
                                     (cons (first rule) (format-helper (rest rule)))
                                     )
                                 )
  )

(define (format-rule rule) (format-helper rule))


;; get-pda-rule: processed-list listof(rules) -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list rules transitions)
  (begin ;(displayln processed-list)
         ;(display transitions)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (second (first (filter (lambda (transition) (equal? (first transition) (first processed-list))) transitions)))]
    )
  )
  )


;; construct-pda-rule: processed-list  listof(rules) -> pda-rule
;; Purpose: Constructes a pda rule from the given processed list
;; NOTE: There is no way to distinguish between
;; ((S a (y )) (A (y )) and ((S a ,EMP) (A ,EMP))
;; because both do the same and leave the stack unchanged. Therefore, either can
;; be picked. When we come across this case we will search the rule for which form is
;; present and choose that one. If both are present then we will pick the first that
;; applies.
(define (construct-pda-rule pl rules)
  (match-define `(,next-state ,next-input ,next-stack) (car pl))
  (match-define `(,init-state ,init-input ,init-stack) (cadr pl))
  ;; If both inputs are equal then nothing was consumed and EMP is used
  (define consumed-input (if (equal? init-input next-input) EMP (car init-input)))

  ;; determin-pushed: list list -> list | symbol
  ;; Purpose: Returns the list or elements to be pushed
  (define/match (determin-pushed _init-stack next-stack)
      [(_ '()) EMP]
    [('() n) n]
    [((list-rest a1 ... b1 _) (list-rest a2 ... b2 _))
     (begin
       ;(displayln (format "~s\n~s" _init-stack next-stack))
       (if (not (equal? b1 b2)) next-stack (determin-pushed a1 a2))
       )
     ])

  ;; determin-poped: list list -> list | symbol
  ;; Purpose: Returns the list or elements to be popped
  (define/match (determin-poped init-stack _next-stack)
    [('() _) EMP]
    [(_ '()) init-stack]
    [((list-rest a1 ... b1 _) (list-rest a2 ... b2 _))
     (if (not (equal? b1 b2)) init-stack (determin-poped a1 a2))])

  (define cur-rule `((,init-state ,consumed-input ,(determin-poped init-stack next-stack))
                     (,next-state ,(determin-pushed init-stack  next-stack))))
  (match cur-rule
    [(list (list _ _ EMP) (list _ EMP))
     (if (member cur-rule rules)
         cur-rule
         `((,init-state ,consumed-input ,init-stack) (,next-state ,next-stack)))]
    [_ cur-rule]))
