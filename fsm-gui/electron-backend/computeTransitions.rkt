#lang racket
(require "../../fsm-core/interface.rkt")
(provide transitions->jsexpr)


;; transitions->jsexpr :: listof(transitions) symbol -> jsexpr(transitions)
;; Given the list of fsm-core transitions, computes the jsexpr form of the transitions
(define (transitions->jsexpr transitions type)
  ;; transition->jsexpr :: transition transition type -> jsexpr(transition)
  ;; Computes a single jsexpr(transition) from 2 fsm-core transitions
  (define (transition->jsexpr t1 t2)
    (match type
      [(or 'dfa 'ndfa) (dfa-rule->jsexpr t1 t2)]
      [_ (error "TODO")]))
  (define (loop transitions)
    (match transitions
      [`(,f ,n ,xs ...) (cons (transition->jsexpr f n)
                              (loop (cons n xs)))]
      [_ '()]))
  (hash 'transitions (loop transitions)))

;; dfa-rule->jsexpr :: transition transition -> jsexpr(transition)
;; computes the jsexpr(transition) for a dfa given 2 fsm-core transitions
(define (dfa-rule->jsexpr t1 t2)
  (if (or (equal? t2 'reject) (equal? t2 'accept))
      (hash 'rule (hash 'start (symbol->string (cadr t1))
                        'input (symbol->string EMP)
                        'end (symbol->string t2))
            'invPass #f)
      (match-let ([`(,(and input1 `(,i1 ...)) ,s1) t1]
                  [`(,(and input2 `(,i2 ...)) ,s2) t2])
                   
                
        (hash 'rule (hash 'start (symbol->string s1)
                          'input (symbol->string
                                  (if (equal? (length input1) (length input2))
                                      ;; if the inputs are the same then nothing was consumed
                                      EMP 
                                      (car i1)))
                          'end (symbol->string s2))
              'invPass #f))))

;; getCurRule: processed-list optional(listof rules) -> rule
;; Determins what the current rule is from the processed-list
;; The rules arg is only used for PDA's. See contruct-pda-rule for more info
;; on why this is needed.
(define (getCurRule processed-list (rules #f))
  (match 'dfa
    ['pda
     (get-pda-rule processed-list rules)]
    [(or 'tm 'tm-language-recognizer)
     (get-tm-rule processed-list)]
    [(or 'mttm 'mttm-language-recognizer)
     (get-mttm-rule processed-list)]))


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

  
 


;; get-pda-rule: processed-list listof(rules) -> pda-rule
;; Purpose: Determins if the rule to be made should be empty or a real rule
(define (get-pda-rule processed-list rules)
  (cond
    [(< (length processed-list) 2)  '((empty empty empty) (empty empty))]
    [else (construct-pda-rule processed-list rules)]))


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
     (if (not (equal? b1 b2)) next-stack (determin-pushed a1 a2))])

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


(module+ test
  (require rackunit)
  ;; ---------------
  ;; DFA/NDFA tests
  ;; ---------------
  (define a*a (make-dfa '(S F A)   
                        '(a b)       
                        'S            
                        '(F)          
                        '((S a F)
                          (F a F)
                          (F b A)
                          (A a F)
                          (A b A))))
  
  (define expected (hash 'transitions (list
                                       (hash 'rule (hash 'start "S" 'input "a" 'end "F")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "F" 'input "b" 'end "A")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "A" 'input "b"'end "A")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "A" 'input "b" 'end "A")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "A" 'input "a" 'end "F")
                                             'invPass #f)
                                       (hash 'rule (hash 'start "F" 'input (symbol->string EMP) 'end "accept")
                                             'invPass #f))))
  (define actual (transitions->jsexpr (sm-showtransitions a*a '(a a a b b b a)) 'dfa))
  (check-equal? actual expected  "A*A compute all transitions")
  

  

  ) ;;end module test