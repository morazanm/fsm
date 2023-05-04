#| This Module containes functions for converting parts for a fsa to a jsexpr |#
#lang racket
(require json "../../fsm-core/interface.rkt")
(provide fsa->jsexpr
         state->jsexpr
         rule->jsexpr
         transitions->jsexpr)


;; fsa->jsexpr :: fsa optional(listof(cons symbol string)) -> jsexpr(fsa)
;; converts a fsm-core fsa to a jsexpr to be sent to the GUI
;; NOTE: invariants are a pair of a symbol representing the state name
;; and the invarinat function as a string.
;; call the `eval` function on it at runtime.
(define (fsa->jsexpr fsa [invariants '()])
  (hash 'states (map (lambda (s) (state->jsexpr s fsa invariants)) (sm-states fsa))
        'alpha (map symbol->string (sm-sigma fsa))
        'rules (map rule->jsexpr (sm-rules fsa))
        'type (symbol->string (sm-type fsa))))

;; state->jsexpr :: symbol fsa optional(listof(cons symbol string))-> jsexpr(state)
;; converts the fsm-core state to a jsexpr that containes the state name
;; state type and string representation of the invariant
;; NOTE: invariants are a pair of a symbol representing the state name
;; and the invarinat function as a string.
;; call the `eval` function on it at runtime.
(define (state->jsexpr state fsa [invariants '()])
  (define inv-state (findf (match-lambda [(cons s f) (equal? s state)]) invariants))
  (hash 'name (symbol->string state)
        'type (cond
                [(and (or (equal? 'tm (sm-type fsa))
                          (equal? 'tm-language-recognizer (sm-type fsa)))
                      (equal? state (sm-accept fsa))) "accept"]
                [(and (member state (sm-finals fsa))
                      (equal? state (sm-start fsa))) "startfinal"]
                [(equal? state (sm-start fsa)) "start"]
                [(member state (sm-finals fsa)) "final"]
                [else "normal"])
        'invFunc (if inv-state (cdr inv-state) (json-null))))


;; rule->jsexpr :: rule -> jsexpr(rule)
;; converts a fsm-core rule to a jsexpr
(define/match (rule->jsexpr rule)
  [(`(,s ,i ,e)) (hash 'start (symbol->string s)
                       'input (symbol->string i)
                       'end (symbol->string e))]
  [(_) (error "TODO")])


;; transitions->jsexpr :: listof(transitions) symbol -> symbol listof(cons symbol string) listof(symbol-> listof(jsexpr(transition))
;; Given the list of fsm-core transitions, computes the jsexpr form of the transitions
(define (transitions->jsexpr transitions type start invariants full-input)
  ;; transition->jsexpr :: transition transition type -> jsexpr(transition)
  ;; Computes a single jsexpr(transition) from 2 fsm-core transitions
  (define (transition->jsexpr t1 t2)
    (match type
      [(or 'dfa 'ndfa) (dfa-transition->jsexpr t1 t2 invariants full-input)]
      [_ (error "TODO")]))
  ;; we alyays need to append the start transiton to the front for the gui.
  ;; EX: (hash 'start "A" 'input (json-null) 'end "start")
  (define start-transition
    (match type
      [(or 'dfa 'ndfa) (hash 'start (symbol->string start)                              
                             'invPass (compute-inv start invariants '()))]
      [_ (error "TODO")]))

  (define (loop transitions)
    (match transitions
      [`(,f ,n ,xs ...) (cons (transition->jsexpr f n)
                              (loop (cons n xs)))]
      [_ '()]))
  (cons start-transition (loop transitions)))

;; dfa-transition->jsexpr :: transition transition listof(cons symbol string) listof(symbol) -> jsexpr(transition)
;; computes the jsexpr(transition) for a dfa given 2 fsm-core transitions
(define (dfa-transition->jsexpr t1 t2 invariants full-input)
  (if (or (equal? t2 'reject) (equal? t2 'accept))
      (hash 'end (symbol->string (cadr t1))
            'action (symbol->string t2)
            'invPass (compute-inv (cadr t1) invariants full-input))
      (match-let* ([`(,(and input1 `(,i1 ...)) ,s1) t1]
                   [`(,(and input2 `(,i2 ...)) ,s2) t2]
                   [consumed-input (drop-right full-input (length input2))])
        (hash 'rule (hash 'start (symbol->string s1)
                          'input (symbol->string
                                  (if (equal? (length input1) (length input2))
                                      ;; if the inputs are the same then nothing was consumed
                                      EMP 
                                      (car i1)))
                          'end (symbol->string s2))
              'invPass (compute-inv s2 invariants consumed-input)))))

;; compute-inv :: symbol listof(cons symbol string) list(symbol) -> boolean | json-null
;; computes the invariant and returns the result. If there is not a invariant associated
;; with the state then json-null is returned
(define (compute-inv state invariants consumed-input)
  (define inv (findf (lambda (i) (equal? (car i) state)) invariants))
  (if inv
      (let ([res (run-invariant (cdr inv) consumed-input)])
        (if (string? res) #f res))
      (json-null)))

;; run-invariant :: string  listof(symbol)-> boolean | string
;; runs the invariant. If there is a syntax error then the error
;; message is returned
(define (run-invariant invariant consumed-input)
  (define ns (make-base-namespace))
  (with-handlers ([exn:fail? (lambda (e) (displayln e)(exn-message e))])
    (define result (eval `(,(read (open-input-string invariant))
                           ',consumed-input) ns))
    (if result #t #f)))

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
  (require (for-syntax syntax/parse)
           rackunit
           rackunit/text-ui
           syntax/to-string)

  (define-syntax (inv->string! stx)
    (syntax-parse stx
      [(_ a)
       #`(syntax->string #'(a))]))

  (define transition->jsexpr-tests
    (test-suite "Tests for function transition->jsexpr"
                (test-case "dfa"
                  (define a*a (make-dfa '(S F A)
                                        '(a b)
                                        'S
                                        '(F)
                                        '((S a F)
                                          (F a F)
                                          (F b A)
                                          (A a F)
                                          (A b A))))

                  (define expected (list
                                    (hash 'start "S"
                                          'invPass #t)
                                    (hash 'rule (hash 'start "S" 'input "a" 'end "F")
                                          'invPass #f)
                                    (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                          'invPass #f)
                                    (hash 'rule (hash 'start "F" 'input "a" 'end "F")
                                          'invPass #f)
                                    (hash 'rule (hash 'start "F" 'input "b" 'end "A")
                                          'invPass (json-null))
                                    (hash 'rule (hash 'start "A" 'input "b"'end "A")
                                          'invPass (json-null))
                                    (hash 'rule (hash 'start "A" 'input "b" 'end "A")
                                          'invPass (json-null))
                                    (hash 'rule (hash 'start "A" 'input "a" 'end "F")
                                          'invPass #f)
                                    (hash 'end "F"
                                          'action "accept"
                                          'invPass #f)))

                  (define invariants (list (cons 'S "(lambda (v) #t)")
                                           (cons 'F "(lambda (v) #f)")))
                  (define actual (transitions->jsexpr
                                  (sm-showtransitions a*a '(a a a b b b a))
                                  'dfa
                                  (sm-start a*a)
                                  invariants
                                  '(a a a b b b a)))
                  (check-equal? actual expected  "A*A compute all transitions"))))

  (run-tests transition->jsexpr-tests)

  (define fsa->jsexpr-tests
    (test-suite "tests for function fsa->jsexpr"
                (test-case "dfa"
                  (define a*a (make-dfa '(S A F D)
                                        '(a b)
                                        'S
                                        '(F)
                                        '((S a F)
                                          (F a F)
                                          (S b A)
                                          (A a F)
                                          (A b A)
                                          ;; rules to dead state D
                                          (F b D)
                                          (D a D)
                                          (D b D))'nodead))
                  (define invariants (list (cons 'S (inv->string! (lambda (consumed-input) 1)))
                                           (cons 'F (inv->string! (lambda (consumed-input) 2)))
                                           (cons 'D (inv->string! (lambda (consumed-input) 3)))))
                  (check-equal? (fsa->jsexpr a*a invariants)
                                (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(lambda (consumed-input) 1)")
                                                    (hash 'name "A" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "F" 'type "final" 'invFunc "(lambda (consumed-input) 2)")
                                                    (hash 'name "D" 'type "normal" 'invFunc "(lambda (consumed-input) 3)"))
                                      'alpha '("a" "b")
                                      'rules (list (hash 'start "S" 'input "a" 'end "F")
                                                   (hash 'start "F" 'input "a" 'end "F")
                                                   (hash 'start "S" 'input "b" 'end "A")
                                                   (hash 'start "A" 'input "a" 'end "F")
                                                   (hash 'start "A" 'input "b" 'end "A")
                                                   (hash 'start "F" 'input "b" 'end "D")
                                                   (hash 'start "D" 'input "a" 'end "D")
                                                   (hash 'start "D" 'input "b" 'end "D"))
                                      'type "dfa")))
                (test-case "ndfa"
                  (define KLEENESTAR-abUaba (make-ndfa '(Q-0 Q-1 Q-2 Q-3 Q-4 Q-5)
                                                       '(a b)
                                                       'Q-0
                                                       '(Q-0)
                                                       `((Q-0 a Q-1)
                                                         (Q-1 b Q-2)
                                                         (Q-2 a Q-3)
                                                         (Q-3 ,EMP Q-0)
                                                         (Q-0 a Q-4)
                                                         (Q-4 b Q-5)
                                                         (Q-5 ,EMP Q-0))))

                  (check-equal? (fsa->jsexpr KLEENESTAR-abUaba)
                                (hash 'states (list (hash 'name "Q-0" 'type "startfinal" 'invFunc (json-null))
                                                    (hash 'name "Q-1" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "Q-2" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "Q-3" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "Q-4" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "Q-5" 'type "normal" 'invFunc (json-null)))
                                      'alpha '("a" "b")
                                      'rules (list (hash 'start "Q-0" 'input "a" 'end "Q-1")
                                                   (hash 'start "Q-1" 'input "b" 'end "Q-2")
                                                   (hash 'start "Q-2" 'input "a" 'end "Q-3")
                                                   (hash 'start "Q-3" 'input (symbol->string EMP) 'end "Q-0")
                                                   (hash 'start "Q-0" 'input "a" 'end "Q-4")
                                                   (hash 'start "Q-4" 'input "b" 'end "Q-5")
                                                   (hash 'start "Q-5" 'input (symbol->string EMP) 'end "Q-0"))
                                      'type "ndfa")))))
  (run-tests fsa->jsexpr-tests)
  

  

  ) ;;end module test
