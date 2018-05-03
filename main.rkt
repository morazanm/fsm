; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module fsm racket
  (require "fsa.rkt" "cfg.rkt"  "pda.rkt" 
           "regular-grammar.rkt" "csg.rkt" "tm.rkt" 
           "regexp.rkt" "constants.rkt" "word.rkt" "misc.rkt"
           "state.rkt" "sm-getters.rkt" "grammar-getters.rkt"
           "grammar-predicate.rkt" "machine-predicate.rkt"
           "regexp-predicate.rkt")
  
  (provide 
   empties

   ; sm constructors
   make-dfa make-ndfa make-ndpda make-tm
   regexp->fsa ndfa->dfa fsa->regexp
   sm-rename-states 
   sm-union sm-concat sm-kleenestar sm-complement sm-intersection grammar->sm

   ; sm observers
   sm-apply sm-showtransitions sm-type
   sm-getstates sm-getalphabet sm-getrules sm-getfinals sm-getstart sm-getstackalphabet

   ; sm testers
   sm-sameresult? sm-testequiv? sm-test

   ; ctm constructor and observer
   combine-tms ctm-run

   ; grammar constructors
   make-rg make-cfg make-csg 
   sm->grammar grammar-rename-nts 
   grammar-union grammar-concat
   
   ; grammar observers
   grammar-derive grammar-gettype
   grammar-getnts grammar-getalphabet grammar-getrules grammar-getstart 

   ;grammar testers
   both-derive both-testequiv grammar-test

   ; regexp constructors
   empty-regexp singleton-regexp union-regexp concat-regexp kleenestar-regexp

   ; regexp observers
   simplify-regexp printable-regexp
   regexp? singleton-regexp? concat-regexp? union-regexp? kleenestar-regexp? null-regexp? empty-regexp?

   ; regexp transformers
   fsa->regexp

   ; some helpful functions
   los->symbol symbol->list generate-symbol symbol->fsmlos symbol-upcase

   ; constants
   EMP DEAD RIGHT LEFT LM BLANK BRANCH GOTO ARROW VAR)
  
  ; Primitive constructors imported from other modules
  
  ; (listof state) fsm --> fsm
  (define (sm-rename-states sts m)
    (let ((t1 (sm-type m)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (rename-states-fsa sts m)]
            [(eq? t1 'pda) (rename-states-pda sts m)]
            [(eq? t1 'tm) (tm-rename-states sts m)]
            [else (error "Incorrect input to sm-rename-states")])))
  
  ; fsm fsm --> fsm
  (define (sm-union m1 m2)
    (let ((t1 (sm-type m1))
          (t2 (sm-type m2)))
      (cond [(not (eq? t1 t2)) 
             (error "Machines have different types: union-fsm")]
            [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (union-fsa m1 m2)]
            [(eq? t1 'pda) (union-pda m1 m2)]
            [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-union m1 m2)]
            [else (error (format "Unknown/Invalid machine types as input to sm-union: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
  
  ; fsm fsm --> fsm
  (define (sm-concat m1 m2)
    (let ((t1 (sm-type m1))
          (t2 (sm-type m2)))
      (cond [(not (eq? t1 t2)) 
             (error "Machines have different types: concat-fsm")]
            [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (concat-fsa m1 m2)]
            [(eq? t1 'pda) (concat-pda m1 m2)]
            [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-concat m1 m2)]
            [else (error (format "Unknown/Invalid machine types as input to sm-concat: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
  ; fsm --> fsm
  (define (sm-kleenestar m)
    (let ((t1 (sm-type m)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa)) 
             (kleenestar-fsa m)]
            [(eq? t1 'pda) (kleenestar-pda m )]
            [(eq? t1 'tm-language-recognizer) (tm-kleenestar m )]
            [else (error (format "Unknown/Invalid machine type as input to sm-kleenestar: input is of type ~s" t1))])))
  
  ; fsm --> fsm or error
  (define (sm-complement m)
    (let (( t1 (sm-type m)))
      (cond [(eq? t1 'pda)
             (error "Cannot complement a pda")]
            [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (complement-fsa m)]
            [(eq? t1 'tm-language-recognizer) (tm-complement m )]
            [else (error (format "Unknown/Invalid machine type as input to sm-complement: input is of type ~s" t1))])))
  
  
  ; sm sm --> sm or error
  (define (sm-intersection m1 m2)
    (let ((t1 (sm-type m1))
          (t2 (sm-type m2)))
      (cond [(not (eq? t1 t2)) 
             (error "Machines have different types: intersection-fsm")]
            [(eq? t1 'pda)
             (error "Cannot intersect two pdas")]
            [(or (eq? t1 'dfa)
                 (eq? t2 'ndfa))
             (intersection-fsa m1 m2)]
            [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-intersection m1 m2)]
            [else (error (format "Unknown/Invalid machine types as input to sm-intersection: first input is of type ~s and second input is of type ~s." t1 t2))])))
  
  ; grammar --> sm 
  (define (grammar->sm g)
    (let ((t1 (grammar-gettype g)))
      (cond [(eq? t1 'rg) (rg->fsa g)]
            [(eq? t1 'cfg) (cfg->pda g)]
            [(eq? t1 'csg) (error (format "Converting a Context-Sensitive Grammar to a Turing machine is not yet implemented....stay tuned!"))]
            [else (error "Unknown grammar type")])))
  
  ; fsm word [natnum] --> 'accept or 'reject
  (define (sm-apply M w . l)
    (let ((head (if (null? l) 0 (car l)))
          (t1 (sm-type M)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (apply-fsa M w)]
            [(eq? t1 'pda) (apply-pda M w)]
            [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-apply M w head)]
            [else (error "Incorrect input to apply-fsm")])))
  
  ; ctm word [natnum] --> (list state natnum tape)
  (define (ctm-run M w . l)
    (let ((res (ctm-apply M w (if (null? l) 0 (car l)))))
      (list (tmconfig-state res) (tmconfig-index res) (tmconfig-tape res))))
  
  ; fsm word [natnum] --> path
  (define (sm-showtransitions M w . l)  
    (let ((head (if (null? l) 0 (car l)))
          (t1 (sm-type M)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (show-transitions-fsa M w)]
            [(eq? t1 'pda) (show-transitions-pda M w)]
            [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-showtransitions M w head)]
            [(eq? t1 'dfst) (M 'show-transitions)]
            [else (error "Incorrect input to show-transitions")])))
  
  ; fsm fsm word --> boolean
  (define (sm-sameresult? M1 M2 w)
    (eq? (sm-apply M1 w) (sm-apply M2 w)))
  
  ; fsm fsm [natnum] --> true or (listof words)
  (define (sm-testequiv?  M1 M2 . l)
    (define number-tests (if (null? l) NUM-TESTS (car l)))
    (let* ((test-m1 (generate-words number-tests (sm-getalphabet M1) null))
           (test-m2 (generate-words number-tests (sm-getalphabet M2) null))
           (test-words (append test-m1 test-m2))
           (res-m1 (map (lambda (w) (list w (sm-apply M1 w))) test-words))
           (res-m2 (map (lambda (w) (list w (sm-apply M2 w))) test-words)))
      (cond [(equal? res-m1 res-m2) #t]
            [else (get-differences res-m1 res-m2 test-words)])))
  
  ; sm [natnum] --> (listof (list word symbol))
  (define (sm-test M . l)
    (let ((numtests (if (null? l) NUM-TESTS (car l)))
          (t1 (sm-type M)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (test-fsa M numtests)]
            [(eq? t1 'pda) (test-pda M numtests)]
            [(eq? t1 'tm) (tm-test M numtests)]
            [else (error "Incorrect input to test-fsm")])))

  
  
  ;;; GRAMMARS
  
  ; grammar --> fsm
  (define (sm->grammar m)
    (let ((t1 (sm-type m)))
      (cond [(or (eq? t1 'dfa)
                 (eq? t1 'ndfa))
             (fsa->rg m)]
            [(eq? t1 'pda) (error (format "Converting a PDA to a Context-Free Grammar is not yet implemented....stay tuned!"))]
            [(eq? t1 'tm) (error (format "Converting a Turing machine to a Context-Sensitive Grammar is not yet implemented....stay tuned!"))]
            [else (error "Input is not a valid grammar.")])))
  
  ; (listof symbol) grammar --> grammar
  (define (grammar-rename-nts nts g)
    (define type (grammar-gettype g))
    (cond [(eq? type 'rg) (rg-rename-nts nts g)]
          [(eq? type 'cfg) (cfg-rename-nts nts g)]
          [(eq? type 'csg) (csg-rename-nts nts g)]
          [else (error (format "In grammar-rename-nts: Unknown grammar type"))]))
  
  ; grammar grammar --> grammar
  (define (grammar-union g1 g2)
    (let ((same-type (eq? (grammar-gettype g1) (grammar-gettype g2))))
      (cond [(not same-type) (error (format "grammar-union: the input grammars are not the same type."))]
            [(rg? g1)
             (let* ((m1 (grammar->sm g1))
                    (m2 (grammar->sm g2))
                    (newm (ndfa->dfa (sm-union m1 m2))))
               (sm->grammar newm))]
            [(cfg? g1) (cfg-union g1 g2)]
            [(csg? g1) (csg-union g1 g2)]
            [else (error (format "Unknown grammar type"))])))
  
  ; grammar grammar --> grammar
  (define (grammar-concat g1 g2)
    (let ((same-type (eq? (grammar-gettype g1) (grammar-gettype g2))))
      (cond [(not same-type) (error (format "grammar-concat: the input grammars are not the same type."))]
            [(rg? g1)
             (let* ((m1 (grammar->sm g1))
                    (m2 (grammar->sm g2))
                    (newm (ndfa->dfa (sm-concat m1 m2))))
               (sm->grammar newm))]
            [(cfg? g1) (cfg-concat g1 g2)]
            [(csg? g1) (csg-concat g1 g2)]
            [else (error (format "Unknown grammar type"))])))
  
  ; grammar word -> derivation or "Not a member"
  (define (grammar-derive g w)
    (cond [(rg? g) (rg-derive g w)]
          [(cfg? g) (cfg-derive g w)]
          [(csg? g) (csg-derive g w)]
          [else (error (format "Unknown grammar type"))]))
  
  ; grammar grammar word --> boolean
  (define (both-derive g1 g2 w)
    (let ((r1 (grammar-derive g1 w))
          (r2 (grammar-derive g2 w)))
      (or (and (string? r1) (string? r2))
          (and (symbol? r1) (symbol? r2)))))
  
  ; grammar word -> derivation or "Not a member"
  (define (grammar-test g . l)
    (let ((numtests (if (null? l) NUM-TESTS (car l))))
      (cond [(rg? g) (test-rg g numtests)]
            [(cfg? g) (test-cfg g numtests)]
            [(csg? g) (error (format "test-grammar: A context-sensitive grammar must be tested manually."))]
            [else (error (format "Unknown grammar type"))])))
  
  
  ; grammar word -> (or true (listof word))
  (define (both-testequiv g1 g2 . l)
    (let* ((numtests (if (null? l) NUM-TESTS (car l)))
           (sigma1 (cond [(rg? g1) (rg-getalphabet g1)]
                         [(cfg? g1) (cfg-get-alphabet g1)]
                         [(csg? g1) (csg-getsigma g1)]))
           (sigma2 (cond [(rg? g2) (rg-getalphabet g2)]
                         [(cfg? g2) (cfg-get-alphabet g2)]
                         [(csg? g2) (csg-getsigma g2)]))
           (testlist (append (generate-words (floor (/ numtests 2)) sigma1 '())
                             (generate-words (ceiling (/ numtests 2)) sigma2 '())))
           (res1 (map (lambda (w) 
                        (let ((r (grammar-derive g1 w)))
                          (if (string? r) r (last r))))
                      testlist))
           (res2 (map (lambda (w) 
                        (let ((r (grammar-derive g2 w)))
                          (if (string? r) r (last r))))
                      testlist))
           (diffs (get-differences res1 res2 testlist)))
      (if (null? diffs) true diffs)))


  

  ;new constructors with contracts

  (define (format-error the-list)
    (if (list? the-list)
        (if (null? the-list) " "
            (local [(define a-list (filter (lambda (x) (not (equal? x #t))) (flatten the-list)))]
              (string-append (car a-list)
                             (string-append "
"
                                            (format-error (cdr a-list))))))
        #t))

  ; make-dfa: (listof state) alphabet state (listof state) (listof rule)) [symbol] --> dfsa
  (define (make-dfa states sigma start finals deltas . adddead)
    (local [(define sts (if (list? states) #t
                            "the list of states must be a list"))
            (define srt (if (symbol? start) #t "the starting state must be a symbol from the given state list"))
            (define sga (if (list? sigma) #t
                            "the sigma must be a list"))
            (define fls (if (list? finals) #t
                            "the list of final states must be a list"))
            (define dts (if (list? deltas) (local [(define temp (filter (lambda (x) (not (and (list? x)
                                                                                              (= (length x) 3)))) deltas))]
                                             (if (empty? temp) #t
                                                 (format "the rules ~s must be lists of length 3" temp)))
                            "the delta must be a list"))
            (define error-list (filter string? (list sts srt sga fls dts)))]
      (cond [(empty? error-list)  (local [(define tentative (make-unchecked-dfa states
                                                                                sigma
                                                                                start
                                                                                finals
                                                                                deltas
                                                                                adddead))
                                          (define final-dfa (valid-machine? tentative))]
                                    (if (string? final-dfa) (error final-dfa)
                                        tentative))]
            [else (error (format-error error-list))]


           
            )
      )
    )

  ; make-ndfa: (listof states) alphabet state (listof state) (listof rule)
  ;            -->
  ;            (list-of symbol) [symbol] --> symbol OR 
  ;                                           (list path symbol) OR
  ;                                           (list-of rule)
  (define (make-ndfa states sigma start finals deltas . adddead)
    (local [(define sts (if (list? states) #t
                            "the list of states must be a list"))
            (define srt (if (symbol? start) #t "the starting state must be a symbol from the given state list"))
            (define sga (if (list? sigma) #t
                            "the sigma must be a list"))
            (define fls (if (list? finals) #t
                            "the list of final states must be a list"))
            (define dts (if (list? deltas) (local [(define temp (filter (lambda (x) (not (and (list? x)
                                                                                              (= (length x) 3)))) deltas))]
                                             (if (empty? temp) #t
                                                 (format "the rules ~s must be lists of length 3" temp)))
                            "the delta must be a list"))
            (define error-list (filter string? (list sts srt sga fls dts)))]
      (cond [(empty? error-list)  (local [(define tentative (make-unchecked-ndfa states
                                                                                 sigma
                                                                                 start
                                                                                 finals
                                                                                 deltas
                                                                                 adddead))
                                          (define final-ndfa (valid-machine? tentative))
           
                                          ]
                                    (if (string? final-ndfa) (error final-ndfa)
                                        tentative)
                                    )]
            [else (error (format-error error-list))])
      )
    )

  ; make-ndpda: (listof states) alphabet alphabet state (listof states) (listof pdarules) --> ndpda
  (define (make-ndpda K sigma gamma start finals pdarules . adddead)
    (local [(define sts (if (list? K) #t
                            "the list of states must be a list"))
            (define srt (if (symbol? start) #t "the starting state must be a symbol from the given state list"))
            (define sga (if (list? sigma) #t
                            "the sigma must be a list"))
            (define gma (if (list? gamma) #t
                            "the gamma must be a list"))
            (define fls (if (list? finals) #t
                            "the list of final states must be a list"))
            
            ;correct-length?: rule --> boolean/listof strings
            ;purpose: checks to see if the rule meets all lenght requirements
            (define (correct-length? rule)
              (local [;check-list?: --> boolean/string
                      ;purpose: check if the rule is a list
                      (define (check-list?)
                        (cond [(list? rule) (if (empty? rule) (format "the rule ~s cannot be empty" rule)
                                                (if (= (length rule) 2) #t
                                                    (format "the rule ~s must be a two list of lists" rule)))]
                              [else (format "the given rule ~s must be a two-list of lists" rule)]))

                      ;inner-length?: string, listof symbols, number --> boolean/string
                      ;purpose: checks that the left side of the rule is correct length
                      (define (inner-length? rule-side rule-bit a-length)
                        (if (and (list? rule-bit)
                                 (= (length rule-bit) a-length)) #t
                                                                 (format "the ~s of the rule ~s must be a list of length ~s" rule-side rule a-length)))
                      
                      (define outer-check (check-list?))
                                        
                      (define compiled-list (if (string? outer-check) (list outer-check)
                                                (filter string? (list (inner-length? "lhs" (first rule) 3)
                                                                      (inner-length? "rhs" (second rule) 2)))))      
                      ]
                (if (empty? compiled-list) #t
                    compiled-list))
              )
                              
            (define dts (if (list? pdarules) (flatten (map correct-length? pdarules))
                            (format "the delta ~s must be a list" pdarules)))
            
            (define error-list (filter string? (flatten (list sts srt sga gma fls dts))))]
      (cond [(empty? error-list)  (local [(define tentative (make-unchecked-ndpda K
                                                                                  sigma
                                                                                  gamma
                                                                                  start
                                                                                  finals
                                                                                  pdarules
                                                                                  ))
                                          (define final-ndpda (valid-machine? tentative))
                                          ]
                                    (if (string? final-ndpda) (error final-ndpda)
                                        tentative
                                        )
                                    )]
            [else (error (format-error error-list))])
      )
    )
  
  ;make-tm (listof state) (listof symbol) (listof (list state symbol) (list state symbol)) (listof state) state --> tm
  (define (make-tm K SIGMA delta s H . accept)
    (local [(define sts (if (list? K) #t
                            "the list of states must be a list"))
            (define sga (if (list? SIGMA) #t
                            "the list of sigma must be a list"))
            (define fls (if (list? H) #t
                            "the list of final states must be a list"))
            (define srt (if (symbol? s) #t "the starting state must be a symbol from the list"))

            ;correct-length?: rule --> boolean/listof strings
            ;purpose: checks to see if the rule meets all lenght requirements
            (define (correct-length? rule)
              (local [;check-list?: --> boolean/string
                      ;purpose: check if the rule is a list
                      (define (check-list?)
                        (cond [(list? rule) (if (empty? rule) (format "the rule ~s cannot be empty" rule)
                                                (if (= (length rule) 2) #t
                                                    (format "the rule ~s must be a two list of lists" rule)))]
                              [else (format "the given rule ~s must be a list" rule)]))

                      ;inner-length?: string listof symbols --> boolean/string
                      ;purpose: checks that the left side of the rule is correct length
                      (define (inner-length? rule-side rule-bit)
                        (if (and (list? rule-bit)
                                 (= (length rule-bit) 2)) #t
                                                          (format "the ~s of the rule ~s must be a list of length 2" rule-side rule)))
                      
                      (define outer-check (check-list?))
                                        
                      (define compiled-list (if (string? outer-check) (list outer-check)
                                                (filter string? (list (inner-length? "lhs" (first rule))
                                                                      (inner-length? "rhs" (second rule))))))      
                      ]
                (if (empty? compiled-list) #t
                    compiled-list))
              )
                              
            (define dts (if (list? delta) (flatten (map correct-length? delta))
                            (format "the delta ~s must be a list of two-lists" delta)))
            
            (define error-list (filter string? (flatten (list sts srt sga fls dts))))
            ]
      (cond [(empty? error-list) (local [(define tentative (if (null? accept) (make-unchecked-tm K SIGMA delta s H)
                                                               (make-unchecked-tm K SIGMA delta s H accept)))
                                         (define final-tm (valid-machine? tentative))
                                         ]
                                   (if (string? final-tm) (error final-tm)
                                       tentative)
                                   )]
            [else (error (format-error error-list))])
      )
    )


  ;grammar-lengths: V sigma R --> list
  (define (grammar-lengths V sigma R)
    (local [(define vs (if (list? V) #t
                           "the list of nonterminals must be a list"))
            (define sg (if (list? sigma) #t
                           "the sigma must be a list"))
            (define dt (if (list? R) (local [(define inner (filter (lambda (x) (not (boolean? x))) (map (lambda (x) (if (and (list? x)
                                                                                                                             (= (length x) 3)) #t
                                                                                                                                               x))
                                                                                                        R)))]
                                       (if (empty? inner) #t
                                           (format "the rules ~s must be lists of length 3" inner)))
                                       
                           "the delta must be a list"))
            (define error-list (filter string? (flatten (list vs sg dt))))]
      error-list)
    )

  ;(make-cfg V sigma R S), where V and sigma are a (listof symbol), R
  ; is a (listof cfg-rule), and S is a symbol
  (define (make-cfg V sigma R S)
    (local [(define error-list (grammar-lengths V sigma R))]
      (cond [(empty? error-list) (local [(define tentative (make-unchecked-cfg V sigma R S))
                                         (define final (valid-grammar? tentative))]
                                   (if (string? final) (error final)
                                       tentative)
                                   )]
            [else (error (format-error error-list))]
            )
      )
    )

  ;make-csg V sigma R S), where V and sigma are a (listof symbol), R
  ; is a (listof csg-rule), and S is a symbol
  (define (make-csg V sigma R S)
    (local [(define error-list (grammar-lengths V sigma R))]
      (cond [(empty? error-list) (local [(define tentative (make-unchecked-csg V sigma R S))
                                         (define final (valid-grammar? tentative))]
                                   (if (string? final) (error final)
                                       tentative)
                                   )]
            [else (error (format-error error-list))])
      )
    )

  ;(make-rg N A R S), such that
  ; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
  ; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
  (define (make-rg V sigma R S)
    (local [(define error-list (grammar-lengths V sigma R))]
      (cond [(empty? error-list) (local [(define tentative (make-unchecked-rg V sigma R S))
                                         (define final (valid-grammar? tentative))]
                                   (if (string? final) (error final)
                                       tentative)
                                   )]
            [else (error (format-error error-list))]
            )
      )
    )



  (define (singleton-regexp a)
    (local [(define tentative (make-unchecked-singleton a))
            (define final (valid-regexp? tentative))]
      (if (string? final) (error final)
          tentative)
      )
    )
  
  (define (concat-regexp a b)
    (local [(define tentative (if (and (regexp? a) (regexp? b)) (make-unchecked-concat a b)
                                  (if (regexp? a) (format "~s must be a regexp to be a valid rhs for the concat-regexp ~s ~s" b a b)
                                      (format "~s must be a regexp to be a valid rhs for the concat-regexp ~s ~s" a a b))))
            (define final (if (string? tentative) tentative
                              (valid-regexp? tentative)))]
      (if (string? final) (error final)
          tentative)
      )
    )
  
  (define (union-regexp a b)
    (local [(define tentative (if (and (regexp? a) (regexp? b)) (make-unchecked-union a b)
                                  (if (regexp? a) (format "~s must be a regexp to be a valid rhs for the union-regexp ~s ~s" b a b)
                                      (format "~s must be a regexp to be a valid rhs for the union-regexp ~s ~s" a a b))))
            (define final (if (string? tentative) tentative
                              (valid-regexp? tentative)))]
      (if (string? final) (error final)
          tentative)
      )
    )
  
  (define (kleenestar-regexp a)
    (local [(define tentative (if (regexp? a) (make-unchecked-kleenestar a)
                                  (format "~s must be a regexp to be a valid input to the kleenestar-regexp" a)))
            (define final (if (string? tentative) tentative
                              (valid-regexp? tentative)))]
      (if (string? final) (error final)
          tentative)
      )
    )
)
 ; closes module


