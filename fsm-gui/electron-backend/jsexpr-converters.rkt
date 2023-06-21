#| This Module containes functions for converting parts for a fsa to a jsexpr |#
#lang racket
(require json
         racket/hash
         "../../fsm-core/interface.rkt"
         "../../fsm-gviz/interface.rkt")

(provide fsa->jsexpr
         state->jsexpr
         rule->jsexpr
         transitions->jsexpr)


;; fsa->jsexpr :: fsa listof(cons symbol string | proc) -> jsexpr(fsa)
;; converts a fsm-core fsa to a jsexpr to be sent to the GUI
;; NOTE: invariants are a pair of a symbol representing the state name
;; and the invarinat function as a string.
(define (fsa->jsexpr fsa invariants (test-mode #f))
  (define base-hash (hash 'states (map (lambda (s) (state->jsexpr s fsa invariants)) (sm-states fsa))
                          'alpha (map symbol->string (sm-sigma fsa))
                          'rules (map rule->jsexpr (sm-rules fsa))
                          'type (symbol->string (sm-type fsa))))
  (hash-union
   (if (or test-mode (not (has-dot-executable?))) (hash) (hash 'filepath (path->string (fsa->svg fsa 0 "vizTool_electron1"))))
   (match (sm-type fsa)
     [(or 'dfa 'ndfa) base-hash]
     ['pda (hash-set base-hash 'stackAlpha (map symbol->string (sm-gamma fsa)))]
     [(or 'tm 'tm-language-recognizer) base-hash]
     [ _ (error 'fsa->jsexpr "invalid machine type: ~a" (sm-type fsa))])))

;; state->jsexpr :: symbol fsa optional(listof(cons symbol string | proc))-> jsexpr(state)
;; converts the fsm-core state to a jsexpr that containes the state name
;; state type and string representation of the invariant
;; NOTE: invariants are a pair of a symbol representing the state name
;; and the invarinat function as a string.
(define (state->jsexpr state fsa [invariants '()])
  (define inv-state (findf (match-lambda [(cons s f) (equal? s state)]) invariants))
  (hash 'name (symbol->string state)
        'type (cond
                [(and (equal? 'tm-language-recognizer (sm-type fsa))
                      (equal? state (sm-accept fsa))) "accept"]
                [(and (member state (sm-finals fsa))
                      (equal? state (sm-start fsa))) "startFinal"]
                [(equal? state (sm-start fsa)) "start"]
                [(member state (sm-finals fsa)) "final"]
                [else "normal"])
        'invFunc (if inv-state (cdr inv-state) (json-null))))


;; rule->jsexpr :: rule -> jsexpr(rule)
;; converts a fsm-core rule to a jsexpr
(define/match (rule->jsexpr rule)
  [(`(,s ,i ,e))
   (hash
    'start (symbol->string s)
    'input (symbol->string i)
    'end (symbol->string e))]
  [(`((,s ,i ,stack1) (,e ,stack2)))
   (hash
    'start (symbol->string s)
    'input (symbol->string i)
    'popped (if (list? stack1) (map symbol->string stack1) '())
    'end (symbol->string e)
    'pushed (if (list? stack2) (map symbol->string stack2) '()))]
  [(`((,s ,i1) (,e ,i2)))
   ;; we keep tm actions as string but the rest as a list. This helps with logic for the GUI
   (define (transform s) (if (member s (list LM LEFT RIGHT BLANK))
                             (symbol->string s)
                             (list (symbol->string s))))
   (hash
    'start (symbol->string s)
    'startTape (if (list? i1) (map symbol->string i1) (transform i1))
    'end (symbol->string e)
    'endTape (if (list? i2) (map symbol->string i2) (transform i2)))]
  [(_) (error 'rule->jsexpr "Invalid rule supplied ~a" rule)])


;; transitions->jsexpr :: fsa listof(cons symbol string | proc) listof(string) number -> jsexpr(transitons) | string
;; Given an fsa computes the jsexpr form of the transitions. If there is an error then a string
;; containing the error is returned
(define (transitions->jsexpr fsa invariants input (tape-index 0))
  (define type (sm-type fsa))
  (define trans (if (or (equal? type 'tm) (equal? type 'tm-language-recpgnizer))
                    (sm-showtransitions fsa input tape-index)
                    (sm-showtransitions fsa input)))
  (match/values (values type trans)
    [(_ 'reject) "The given input for the machine was rejected"]
    [((or 'dfa 'ndfa) _)
     (dfa-transitions->jsexpr trans (sm-start fsa) invariants input)]
    [('pda _) (pda-transitions->jsexpr trans (sm-start fsa) invariants input (sm-rules fsa))]
    [((or 'tm 'tm-language-recognizer) _) (tm-transitions->jsexpr trans (sm-start fsa) invariants input tape-index)]
    [(_ _) (error 'transitions->jsexpr "Invalid machine type: ~a" type)]))

;; tm-transitions->jsexpr :: listof(transitions) symbol listof(cons symbol string | proc) listof(symbol) number -> listof(jsexpr(transition))
;; Given the list of fsm-core transitions, computes the jsexpr form of the transitions
(define (tm-transitions->jsexpr transitions start invariants initial-tape tape-start-index)
  (define (symbol-list->string lst) (map (lambda (s) (symbol->string s)) lst))
  ;; tm-transition->jsexpr :: transition transition listof(cons symbol string | proc) -> jsexpr(transition)
  ;; computes the jsexpr(transition) for a tm/lang-rec given 2 fsm-core transitions
  (define (tm-transition->jsexpr t1 t2)
    (match-define `(,cur-state ,cur-pos ,cur-tape) t1)
    (match-define `(,next-state ,next-pos ,next-tape) t2)
    (define-values (cur-action next-action) (cond
                                              [(< cur-pos next-pos)
                                               ;; tape incriments so we move Right
                                               (values (list-ref cur-tape cur-pos) RIGHT)]
                                              [(> cur-pos next-pos)
                                               ;; tape decriments so we move LEFT
                                               (values (list-ref cur-tape cur-pos) LEFT)]
                                              ;; Otherwise we write to the tape
                                              [else
                                               (values (list-ref cur-tape cur-pos)
                                                       (list-ref next-tape next-pos))]))
    (hash 'rule (rule->jsexpr `((,cur-state ,cur-action) (,next-state ,next-action)))
          'tapeIndex next-pos
          'tape (symbol-list->string next-tape)
          'invPass (compute-inv next-state invariants next-tape next-pos)))
  ;; we alyays need to append the start transiton to the front for the gui.
  ;; EX: (hash 'start "A" 'input (json-null) 'end "start")
  (define start-transition (hash 'start (symbol->string start)
                                 'tapeIndex tape-start-index
                                 'tape (symbol-list->string initial-tape)
                                 'invPass (compute-inv start invariants initial-tape tape-start-index)))
  ;; if a tm builds then input always halts
  (define end-transition (hash 'end (symbol->string (first (last transitions)))
                               'tapeIndex (second (last transitions))
                               'tape (symbol-list->string (third (last transitions)))
                               'invPass (compute-inv (first (last transitions))
                                                     invariants
                                                     (third (last transitions))
                                                     (second (last transitions)))))
  (define (loop transitions)
    (match transitions
      [`(,f ,n ,xs ...) (cons (tm-transition->jsexpr f n) (loop (cons n xs)))]
      [_ '()]))
  (append (list start-transition) (loop transitions) (list end-transition)))


;; dfa-transitions->jsexpr :: listof(transitions) symbol listof(cons symbol string | proc) listof(symbol)-> listof(jsexpr(transition))
;; Given the list of fsm-core transitions, computes the jsexpr form of the transitions
(define (dfa-transitions->jsexpr transitions start invariants full-input)
  ;; dfa-transition->jsexpr :: transition transition listof(cons symbol string | proc) listof(symbol) -> jsexpr(transition)
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
  ;; we alyays need to append the start transiton to the front for the gui.
  ;; EX: (hash 'start "A" 'input (json-null) 'end "start")
  (define start-transition (hash 'start (symbol->string start)
                                 'invPass (compute-inv start invariants)))
  (define (loop transitions)
    (match transitions
      [`(,f ,n ,xs ...) (cons (dfa-transition->jsexpr f n invariants full-input)
                              (loop (cons n xs)))]
      [_ '()]))
  (cons start-transition (loop transitions)))




;; pda-transitions->jsexpr :: listof(transitions) symbol listof(cons symbol string | proc) listof(symbol) listof(rules) -> listof(jsexpr(transition))
;; Given the list of fsm-core transitions, computes the jsexpr form of the transitions
(define (pda-transitions->jsexpr transitions start invariants input rules)
  ;; pda-transition->jsexpr: transition transition listof(rules) listof(cons symbol string | proc) listof(symbol) listof(symbol) -> values(pda-rule stack)
  ;; Purpose: Constructes a pda rule from the given transitions
  ;; NOTE: There is no way to distinguish between
  ;; ((S a (y )) (A (y )) and ((S a ,EMP) (A ,EMP))
  ;; because both do the same and leave the stack unchanged. Therefore, either can
  ;; be picked. When we come across this case we will search the rule for which form is
  ;; present and choose that one. If both are present then we will pick the first that
  ;; applies.
  (define (pda-transition->jsexpr t1 t2 rules invariants full-input current-stack)
    (cond
      [(or (equal? t2 'accept) (equal? t2 'reject))
       (values (hash 'end (symbol->string (car t1))
                     'action (symbol->string t2)
                     'invPass (compute-inv (car t1) invariants full-input current-stack))
               current-stack)]
      [else
       (match-define `(,next-state ,next-input ,next-stack) t2)
       (match-define `(,init-state ,init-input ,init-stack) t1)

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

       ;; What is pushed on the stack
       (define pushed (determin-pushed init-stack next-stack))
       (define pushed-as-list (if (symbol? pushed) '() pushed))
       ;; What is popped off the stack
       (define popped (determin-poped init-stack next-stack))
       (define popped-as-list (if (symbol? popped) '() popped))
       ;; The current rule based off the 2 transitions
       (define cur-rule `((,init-state ,consumed-input ,popped) (,next-state ,pushed)))
       ;; What the stack looks like after the rule
       (define new-stack (append pushed-as-list (drop current-stack (length popped-as-list))))
       (values (hash 'rule (rule->jsexpr (match cur-rule
                                           [(list (list a b '()) (list c '()))
                                            (if (member (list (list a b EMP) (list c EMP)) rules)
                                                cur-rule
                                                `((,init-state ,consumed-input ,init-stack)
                                                  (,next-state ,next-stack)))]
                                           [_ cur-rule]))
                     'invPass (compute-inv next-state
                                           invariants
                                           (drop-right full-input (length next-input))
                                           new-stack)
                     'stack (map symbol->string new-stack))
               new-stack)]))


  ;; we alyays need to append the start transiton to the front for the gui.
  ;; EX: (hash 'start "A" 'input (json-null) 'end "start")
  (define start-transition (hash 'start (symbol->string start)
                                 'invPass (compute-inv start invariants '() '())))
  (define (loop transitions current-stack)
    (match transitions
      [`(,f ,n ,xs ...)
       (let-values (((jsexpr stack) (pda-transition->jsexpr f n rules invariants input current-stack)))
         (cons jsexpr (loop (cons n xs) stack)))]
      [_ '()]))
  (cons start-transition (loop transitions '())))


;; compute-inv :: symbol listof(cons symbol string | proc) vargs(any) -> boolean | json-null
;; computes the invariant and returns the result. If there is not a invariant associated
;; with the state then json-null is returned
;; TODO: Handle error messages
(define (compute-inv state invariants . args)
  ;; run-racket-code :: string -> boolean | string
  ;; runs the invariant. If there is a syntax error then the error message is returned
  ;; TODO: Document this more once I have a better understanding of racket macro namespaces https://stackoverflow.com/questions/57927786/how-to-obtain-namespace-of-a-custom-lang-in-racket
  (define (run-racket-code invariant)
    (define (truthy? x) (not (false? x)))
    (define ns (make-base-namespace))
    (namespace-attach-module (current-namespace) 'racket ns)
    (parameterize ([current-namespace (make-base-namespace)])
      (namespace-require 'racket)
      (with-handlers ([exn:fail? (lambda (e)
                                   (displayln (format "[FSM Internal Error @ (invariant check)]: ~s" (exn-message e)))
                                   (exn-message e))])
        (define res (eval (read (open-input-string invariant))))
        (define inv-args (if (null? args) (list null) args))
        (truthy? (match res
                   ;; if res is void then its in the form (define (...) ...) so we need to apply the name
                   [(? void?)
                    (define f-name (regexp-replace
                                    #px"\\(define *\\([\\s]*"
                                    (first (regexp-match #px"\\(define *\\([\\s]*[^ ]*" invariant))
                                    ""))
                    (apply (eval (string->symbol f-name)) inv-args)]
                   [_ (apply res inv-args)])))))
  (define inv (findf (lambda (i) (equal? (car i) state)) invariants))
  (match inv
    ;; for hot realoading code
    [(cons _ (? string?))
     (define res (run-racket-code (cdr inv)))
     (if (string? res) #f res)]
    ;; classic way for computing invariants
    [(cons _ (? procedure?))
     (with-handlers ([exn:fail? (lambda (e) #f)])
       (apply (cdr inv) (if (null? args) (list null) args)))]
    [_ (json-null)]))



(module+ test
  (require (for-syntax syntax/parse)
           rackunit
           rackunit/text-ui
           syntax/to-string)


  (define (symbol-list->string lst) (map (lambda (s) (symbol->string s)) lst))

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

                  (define invariants (list (cons 'S "(define (test-1 v) #t)")
                                           (cons 'F "(lambda (v) #f)")))
                  (define actual (transitions->jsexpr
                                  a*a
                                  invariants
                                  '(a a a b b b a)))
                  (check-equal? actual expected  "A*A compute all transitions"))

                (test-case "pda"
                  (define pda=2ba (make-ndpda '(S M1 F)
                                              '(a b)
                                              '(a b)
                                              'S
                                              '(F)
                                              `(((S ,EMP ,EMP) (M1 ,EMP))
                                                ((M1 a ,EMP) (M1 (a a)))
                                                ((M1 b ,EMP) (M1 (b)))
                                                ((M1 a (b)) (M1 (a)))
                                                ((M1 a (b b)) (M1 ,EMP))
                                                ((M1 b (a)) (M1 ,EMP))
                                                ((M1 ,EMP ,EMP) (F ,EMP)))))
                  (define EMP-str (symbol->string EMP))

                  (define expected (list
                                    (hash 'start "S"
                                          'invPass #t)
                                    (hash 'rule (hash 'start "S" 'input EMP-str 'popped '() 'end "M1" 'pushed '())
                                          'invPass #t
                                          'stack (list))
                                    (hash 'rule (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                          'invPass #t
                                          'stack (list "b"))
                                    (hash 'rule (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                          'invPass #t
                                          'stack (list "b" "b"))
                                    (hash 'rule (hash 'start "M1" 'input "a" 'popped (list "b" "b") 'end "M1" 'pushed '())
                                          'invPass #t
                                          'stack (list))
                                    (hash 'rule (hash 'start "M1" 'input EMP-str 'popped '() 'end "F" 'pushed '())
                                          'invPass #t
                                          'stack (list))
                                    (hash 'end "F"
                                          'action "accept"
                                          'invPass #t)))

                  (define invariants (list (cons 'S (inv->string! (lambda (i s)
                                                                    (and (empty? s) (empty? i)))))
                                           (cons 'M1 (inv->string! (lambda (i s)
                                                                     (if (not (empty? i))
                                                                         (if (equal? (first i) 'a)
                                                                             (let ((as (length (filter (lambda (v) (equal? v 'a)) i)))
                                                                                   (bs (length (filter (lambda (v) (equal? v 'b)) i))))
                                                                               (equal? (length s) (- (* 2 as) bs)))
                                                                             (or (empty? s) (equal? (first s) 'b)))
                                                                         #t))))

                                           (cons 'F (inv->string! (lambda (i s)
                                                                    (define as (length (filter (lambda (v) (equal? v 'a)) i)))
                                                                    (define bs (length (filter (lambda (v) (equal? v 'b)) i)))
                                                                    (and (equal? bs (* 2 as))
                                                                         (empty? s)))))))
                  (check-equal? (transitions->jsexpr pda=2ba invariants '(b b a)) expected))


                (test-case "tm"
                  ;; write a on tape
                  (define Ma (make-tm '(S H)
                                      `(a b ,LM)
                                      `(((S a) (H a))
                                        ((S b) (H a))
                                        ((S ,BLANK) (H a)))
                                      'S
                                      '(H)))

                  (define invariants '())

                  (check-equal? (transitions->jsexpr Ma invariants '(b b) 0)
                                (list (hash 'start "S"
                                            'tapeIndex 0
                                            'tape '("b" "b")
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "S" 'startTape '("b") 'end "H" 'endTape '("a"))
                                            'tapeIndex 0
                                            'tape '("a" "b")
                                            'invPass (json-null))
                                      (hash 'end "H"
                                            'tapeIndex 0
                                            'tape '("a" "b")
                                            'invPass (json-null)))))

                (test-case "tm-language-recognizer"
                  ;; write a on tape
                  (define a^nb^nc^n (make-tm '(S B C D E Y N)
                                             '(a b c z)
                                             `(((S a) (B z))
                                               ((S b) (N b))
                                               ((S c) (N c))
                                               ((S ,BLANK) (Y ,BLANK))
                                               ((S z) (N z))
                                               ((E z) (E ,RIGHT))
                                               ((E ,BLANK) (Y ,BLANK))
                                               ((E a) (N a))
                                               ((E b) (N b))
                                               ((E c) (N c))
                                               ((B a) (B ,RIGHT))
                                               ((B b) (C z))
                                               ((B c) (N c))
                                               ((B ,BLANK) (N ,BLANK))
                                               ((B z) (B ,RIGHT))
                                               ((C a) (N a))
                                               ((C b) (C ,RIGHT))
                                               ((C c) (D z))
                                               ((C ,BLANK) (N ,BLANK))
                                               ((C z) (C ,RIGHT))
                                               ((D a) (S a))
                                               ((D b) (D ,LEFT))
                                               ((D c) (D ,LEFT))
                                               ((D ,BLANK) (N ,BLANK))
                                               ((D z) (D ,LEFT))
                                               ((D ,LM) (E R)))
                                             'S
                                             '(Y N)
                                             'Y))
                  (define invariants '())

                  (check-equal? (transitions->jsexpr a^nb^nc^n invariants '(@ a b c) 0)
                                (list (hash 'start "S"
                                            'tapeIndex 0
                                            'tape '("@" "a" "b" "c")
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "S" 'startTape "@" 'end "S" 'endTape "R")
                                            'tapeIndex 1
                                            'tape '("@" "a" "b" "c")
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "S" 'startTape '("a") 'end "B" 'endTape '("z"))
                                            'tapeIndex 1
                                            'tape '("@" "z" "b" "c")
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "B" 'startTape '("z") 'end "B" 'endTape "R")
                                            'tapeIndex 2
                                            'tape '("@" "z" "b" "c")
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "B" 'startTape '("b") 'end "C" 'endTape '("z"))
                                            'tape '("@" "z" "z" "c")
                                            'tapeIndex 2
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "C" 'startTape '("z") 'end "C" 'endTape "R")
                                            'tape '("@" "z" "z" "c")
                                            'tapeIndex 3
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "C" 'startTape '("c") 'end "D" 'endTape '("z"))
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 3
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "D" 'startTape '("z") 'end "D" 'endTape "L")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 2
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "D" 'startTape '("z") 'end "D" 'endTape "L")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 1
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "D" 'startTape '("z") 'end "D" 'endTape "L")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 0
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "D" 'startTape "@" 'end "E" 'endTape "R")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 1
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "E" 'startTape '("z") 'end "E" 'endTape "R")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 2
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "E" 'startTape '("z") 'end "E" 'endTape "R")
                                            'tape '("@" "z" "z" "z")
                                            'tapeIndex 3
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "E" 'startTape '("z") 'end "E" 'endTape "R")
                                            'tape '("@" "z" "z" "z" "_")
                                            'tapeIndex 4
                                            'invPass (json-null))
                                      (hash 'rule (hash 'start "E" 'startTape "_" 'end "Y" 'endTape "_")
                                            'tape '("@" "z" "z" "z" "_")
                                            'tapeIndex 4
                                            'invPass (json-null))
                                      (hash 'end "Y"
                                            'tape '("@" "z" "z" "z" "_")
                                            'tapeIndex 4
                                            'invPass (json-null)))))))

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
                  (define invariants (list (cons 'S (inv->string! (define (test consumed-input) 1)))
                                           (cons 'F (inv->string! (lambda (consumed-input) 2)))
                                           (cons 'D (inv->string! (lambda (consumed-input) 3)))))
                  (check-equal? (fsa->jsexpr a*a invariants #t)
                                (hash 'states (list (hash 'name "S" 'type "start" 'invFunc "(define (test consumed-input) 1)")
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

                  (check-equal? (fsa->jsexpr KLEENESTAR-abUaba '() #t)
                                (hash 'states (list (hash 'name "Q-0" 'type "startFinal" 'invFunc (json-null))
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
                                      'type "ndfa")))



                (test-case "pda"
                  (define pda=2ba (make-ndpda '(S M1 F)
                                              '(a b)
                                              '(a b)
                                              'S
                                              '(F)
                                              `(((S ,EMP ,EMP) (M1 ,EMP))
                                                ((M1 a ,EMP) (M1 (a a)))
                                                ((M1 b ,EMP) (M1 (b)))
                                                ((M1 a (b)) (M1 (a)))
                                                ((M1 a (b b)) (M1 ,EMP))
                                                ((M1 b (a)) (M1 ,EMP))
                                                ((M1 ,EMP ,EMP) (F ,EMP)))))
                  (define EMP-str (symbol->string EMP))

                  (check-equal? (fsa->jsexpr pda=2ba '() #t)
                                (hash 'states (list (hash 'name "S" 'type "start" 'invFunc (json-null))
                                                    (hash 'name "M1" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "F" 'type "final" 'invFunc (json-null)))
                                      'alpha '("a" "b")
                                      'stackAlpha '("a" "b")
                                      'rules (list (hash 'start "S" 'input EMP-str 'popped '() 'end "M1" 'pushed '())
                                                   (hash 'start "M1" 'input "a" 'popped '() 'end "M1" 'pushed (list "a" "a"))
                                                   (hash 'start "M1" 'input "b" 'popped '() 'end "M1" 'pushed (list "b"))
                                                   (hash 'start "M1" 'input "a" 'popped (list "b") 'end "M1" 'pushed (list "a"))
                                                   (hash 'start "M1" 'input "a" 'popped (list "b" "b") 'end "M1" 'pushed '())
                                                   (hash 'start "M1" 'input "b" 'popped (list "a") 'end "M1" 'pushed '())
                                                   (hash 'start "M1" 'input EMP-str 'popped '() 'end "F" 'pushed '()))
                                      'type "pda")))
                (test-case "tm"
                  ;; write a on tape
                  (define Ma (make-tm '(S H)
                                      `(a b ,LM)
                                      `(((S a) (H a))
                                        ((S b) (H a))
                                        ((S ,BLANK) (H a)))
                                      'S
                                      '(H)))

                  (check-equal? (fsa->jsexpr Ma '() #t)
                                (hash 'states (list (hash 'name "S" 'type "start" 'invFunc (json-null))
                                                    (hash 'name "H" 'type "final" 'invFunc (json-null)))
                                      'alpha '("a" "b" "@")
                                      'rules (list (hash 'start "S" 'startTape "@" 'end "S" 'endTape "R")
                                                   (hash 'start "S" 'startTape '("a") 'end "H" 'endTape '("a"))
                                                   (hash 'start "S" 'startTape '("b") 'end "H" 'endTape '("a"))
                                                   (hash 'start "S" 'startTape "_" 'end "H" 'endTape '("a")))
                                      'type "tm")))

                (test-case "tm-language-recognizer"
                  ;; write a on tape
                  (define a^nb^nc^n (make-tm '(S B C D E Y N)
                                             '(a b c z)
                                             `(((S a) (B z))
                                               ((S b) (N b))
                                               ((S c) (N c))
                                               ((S ,BLANK) (Y ,BLANK))
                                               ((S z) (N z))
                                               ((E z) (E ,RIGHT))
                                               ((E ,BLANK) (Y ,BLANK))
                                               ((E a) (N a))
                                               ((E b) (N b))
                                               ((E c) (N c))
                                               ((B a) (B ,RIGHT))
                                               ((B b) (C z))
                                               ((B c) (N c))
                                               ((B ,BLANK) (N ,BLANK))
                                               ((B z) (B ,RIGHT))
                                               ((C a) (N a))
                                               ((C b) (C ,RIGHT))
                                               ((C c) (D z))
                                               ((C ,BLANK) (N ,BLANK))
                                               ((C z) (C ,RIGHT))
                                               ((D a) (S a))
                                               ((D b) (D ,LEFT))
                                               ((D c) (D ,LEFT))
                                               ((D ,BLANK) (N ,BLANK))
                                               ((D z) (D ,LEFT))
                                               ((D ,LM) (E R)))
                                             'S
                                             '(Y N)
                                             'Y))
                  (check-equal? (fsa->jsexpr a^nb^nc^n '() #t)
                                (hash 'states (list (hash 'name "S" 'type "start" 'invFunc (json-null))
                                                    (hash 'name "B" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "C" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "D" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "E" 'type "normal" 'invFunc (json-null))
                                                    (hash 'name "Y" 'type "accept" 'invFunc (json-null))
                                                    (hash 'name "N" 'type "final" 'invFunc (json-null)))
                                      'alpha '("@" "a" "b" "c" "z")
                                      'rules (list (hash 'start "S" 'startTape "@" 'end "S" 'endTape "R")
                                                   (hash 'start "B" 'startTape "@" 'end "B" 'endTape "R")
                                                   (hash 'start "C" 'startTape "@" 'end "C" 'endTape "R")
                                                   (hash 'start "D" 'startTape "@" 'end "D" 'endTape "R")
                                                   (hash 'start "E" 'startTape "@" 'end "E" 'endTape "R")
                                                   (hash 'start "S" 'startTape '("a") 'end "B" 'endTape '("z"))
                                                   (hash 'start "S" 'startTape '("b") 'end "N" 'endTape '("b"))
                                                   (hash 'start "S" 'startTape '("c") 'end "N" 'endTape '("c"))
                                                   (hash 'start "S" 'startTape "_" 'end "Y" 'endTape "_")
                                                   (hash 'start "S" 'startTape '("z") 'end "N" 'endTape '("z"))
                                                   (hash 'start "E" 'startTape '("z") 'end "E" 'endTape "R")
                                                   (hash 'start "E" 'startTape "_" 'end "Y" 'endTape "_")
                                                   (hash 'start "E" 'startTape '("a") 'end "N" 'endTape '("a"))
                                                   (hash 'start "E" 'startTape '("b") 'end "N" 'endTape '("b"))
                                                   (hash 'start "E" 'startTape '("c") 'end "N" 'endTape '("c"))
                                                   (hash 'start "B" 'startTape '("a") 'end "B" 'endTape "R")
                                                   (hash 'start "B" 'startTape '("b") 'end "C" 'endTape '("z"))
                                                   (hash 'start "B" 'startTape '("c") 'end "N" 'endTape '("c"))
                                                   (hash 'start "B" 'startTape "_" 'end "N" 'endTape "_")
                                                   (hash 'start "B" 'startTape '("z") 'end "B" 'endTape "R")
                                                   (hash 'start "C" 'startTape '("a") 'end "N" 'endTape '("a"))
                                                   (hash 'start "C" 'startTape '("b") 'end "C" 'endTape "R")
                                                   (hash 'start "C" 'startTape '("c") 'end "D" 'endTape '("z"))
                                                   (hash 'start "C" 'startTape "_" 'end "N" 'endTape "_")
                                                   (hash 'start "C" 'startTape '("z") 'end "C" 'endTape "R")
                                                   (hash 'start "D" 'startTape '("a") 'end "S" 'endTape '("a"))
                                                   (hash 'start "D" 'startTape '("b") 'end "D" 'endTape "L")
                                                   (hash 'start "D" 'startTape '("c") 'end "D" 'endTape "L")
                                                   (hash 'start "D" 'startTape "_" 'end "N" 'endTape "_")
                                                   (hash 'start "D" 'startTape '("z") 'end "D" 'endTape "L")
                                                   (hash 'start "D" 'startTape "@" 'end "E" 'endTape "R"))
                                      'type "tm-language-recognizer")))))
  (run-tests fsa->jsexpr-tests)




  ) ;;end module test
