; FSM Library Version 1.0
; Copyright (C) 2020 by Marco T. Morazan
; Written by: Marco T. Morazan

(module tm racket
  (require "constants.rkt" "misc.rkt" "word.rkt")
  (provide tm-rename-states
           make-unchecked-tm
           combine-tms
           tm-union tm-concat tm-kleenestar tm-complement tm-intersection
           ctm-apply tmconfig-state tmconfig-index tmconfig-tape
           tm-apply tm-showtransitions tm-test
           )
  
  ; tmrule
  ; A tmrule is a structure, (tmrule k1 a k2 b), where k1 and k2 are states, a is a symbol, and
  ; b is a symbol
  (struct tmrule (froms read tos action) #:transparent)
  
  ; remove-LM-rule: (listof ((list state symbol) (state action))) --> (listof tm-rule)
  (define (remove-LM-rule-unparsed-tm-rules rls)
    (filter (lambda (rl) (not (eq? (cadar rl) LM))) rls))
  
  ; remove-LM-rule: (listof tm-rule --> (listof tm-rule)
  (define (remove-LM-rule-tm-rules rls)
    (filter (lambda (rl) (not (eq? (tmrule-read rl) LM))) rls))
  
  ; unparse-tmrules: (listof tm-rule) --> (listof ((list state symbol) (state action)))
  (define (unparse-tmrules rls)
    (map (lambda (r) (list 
                      (list (tmrule-froms r) (tmrule-read r))
                      (list (tmrule-tos r) (tmrule-action r))))
         rls))
  
  ; get-unparsed-rules-without-LM-move-R-default-rule: (listof tm-rule) --> (listof ((list state symbol) (state action)))
  ; Purpose: Unparse the given list of tm-rule and remove the left marker rule that always moves right (added by make-unchecked-tm)
  (define (unparse-rules-without-LM-move-R-default-rule rls)
    (remove-LM-rule-unparsed-tm-rules (unparse-tmrules)))
  
  ; tm (listof states) --> tm
  (define (tm-rename-states sts m)
    (let* (
           (rename-table (map (lambda (s) (list s (generate-symbol s sts)))
                              (tm-getstates m)))
           (new-states (map (lambda (s) (cadr (assoc s rename-table)))
                            (tm-getstates m)))
           (new-start (cadr (assoc (tm-getstart m) rename-table)))
           (new-finals (map (lambda (s) (cadr (assoc s rename-table)))
                            (tm-getfinals m)))
           (new-rules (filter (lambda (rl) (not (eq? (cadar rl) LM))) 
                              (map (lambda (r) (list 
                                                (list (cadr (assoc (tmrule-froms r) rename-table))
                                                      (tmrule-read r))
                                                (list (cadr (assoc (tmrule-tos r) rename-table))
                                                      (tmrule-action r))))
                                   (tm-getdelta m)))))
      (make-unchecked-tm new-states (tm-getalphabet m) 
                         new-rules 
                         new-start 
                         new-finals 
                         (if (null? (tm-getaccept m)) 
                             null 
                             (cadr (assoc (tm-getaccept m) rename-table))))))
  
  (define (mk-to-reject-rules-for-st st rej sigma)
    (map (lambda (s) (list (list st s) (list rej s))) sigma))
  
  ; tm-union: tm tm --> tm
  ; ASSUMPTION: ***** The given tms are language recognizers *****
  (define (tm-union m1 m2)
    (cond [(and (eq? (tm-whatami? m1) 'tm-language-recognizer)
                (eq? (tm-whatami? m2) 'tm-language-recognizer))
           (let* ((rm2 (tm-rename-states (tm-getstates m2) m2))
                  (m1-rules (remove-LM-rule-unparsed-tm-rules (tm-getrules m1)))
                  (rm2-rules (remove-LM-rule-unparsed-tm-rules (tm-getrules rm2)))
                  (new-sigma (remove-duplicates (append (tm-getalphabet m1) (tm-getalphabet rm2))))
                  (states (append (tm-getstates m1) (tm-getstates rm2)))
                  (newS (generate-symbol START states))
                  (new-accept (generate-symbol 'accept (cons newS states)))
                  (new-reject (generate-symbol 'reject (cons new-accept (cons newS states))))
                  (new-states (cons new-reject (cons new-accept (cons newS states))))
                  (new-finals (list new-reject new-accept))
                  (new-rules-m1states-to-new-reject-with-sigma2
                   (map  (lambda (st) (mk-to-reject-rules-for-st st new-reject (cons BLANK (tm-getalphabet rm2))))
                         (tm-getstates m1)))
                  (new-rules-rm2states-to-new-reject-with-sigma1
                   (map  (lambda (st) (mk-to-reject-rules-for-st st new-reject (cons BLANK (tm-getalphabet m1))))
                         (tm-getstates rm2)))
                  (new-rules-from-new-start (append 
                                             (map (lambda (s) (list (list newS s) (list (tm-getstart m1) s))) (cons BLANK (tm-getalphabet m1)))
                                             (map (lambda (s) (list (list newS s) (list (tm-getstart rm2) s))) (cons BLANK (tm-getalphabet rm2)))))
                  (new-rules-to-new-accept (append (map (lambda (s) (list (list (tm-getaccept m1) s) (list new-accept s))) (cons BLANK (tm-getalphabet m1)))
                                                   (map (lambda (s) (list (list (tm-getaccept rm2) s) (list new-accept s))) (cons BLANK (tm-getalphabet rm2)))))
                  (m1-reject (car (filter (lambda (s) (not (eq? s (tm-getaccept m1)))) (tm-getfinals m1))))
                  (rm2-reject (car (filter (lambda (s) (not (eq? s (tm-getaccept rm2)))) (tm-getfinals rm2))))
                  (new-rules-to-new-reject (append (map (lambda (s) (list (list m1-reject s) (list new-reject s))) (cons BLANK (tm-getalphabet m1)))
                                                   (map (lambda (s) (list (list rm2-reject s) (list new-reject s))) (cons BLANK (tm-getalphabet rm2)))))
                  (new-rules (append new-rules-from-new-start
                                     new-rules-to-new-accept
                                     new-rules-to-new-reject
                                     m1-rules
                                     rm2-rules))
                  ; A RULE FOR EACH ELEMENT OF new-sigma TO MOVE TO THE TWO OLD START STATES
                  )
             (make-unchecked-tm new-states new-sigma new-rules newS new-finals new-accept))]
          [else (error (format "tm-union: Both inputs must be language recognizers. First is a ~s and second is a ~s." (tm-whatami? m1) (tm-whatami? m2)))]))
  
  ; ASSUMPTION: ***** The given tms are language recognizers *****
  (define (tm-getreject m)
    (if (eq? (tm-getaccept m) (car (tm-getfinals m))) 
        (cadr (tm-getfinals m))
        (car (tm-getfinals m))))
  
  ; tm-concat: tm tm --> tm
  ; ASSUMPTION: ***** The given tms are language recognizers *****
  ; *** BUG: m1 only reaches Y by seeing a blank the concat machine never reaches m2's start state ***
  (define (tm-concat m1 m2)
    (let* ((rm2 (tm-rename-states (tm-getstates m2) m2))
           (m1-reject (tm-getreject m1))
           (m1-rules (remove-LM-rule-unparsed-tm-rules (tm-getrules m1)))
           (rm2-rules (remove-LM-rule-unparsed-tm-rules (tm-getrules rm2)))
           (dd (displayln rm2-rules))
           (new-sigma (remove-duplicates (append (tm-getalphabet m1) (tm-getalphabet rm2))))
           (new-start (tm-getstart m1))
           (new-states (append (tm-getstates m1) (tm-getstates rm2)))
           (new-finals (tm-getfinals rm2))
           (new-accept (tm-getaccept rm2))
           (new-reject (tm-getreject rm2))
           (new-rules-startM1-to-new-reject (mk-to-reject-rules-for-st new-start new-reject (cons BLANK (tm-getalphabet rm2))))
           ;(d (displayln new-rules-startM1-to-new-reject))
           (new-rules-rejectM1-to-new-reject (mk-to-reject-rules-for-st m1-reject new-reject (cons BLANK new-sigma)))
           (new-rules-acceptM1-to-startRM2 (append-map (lambda (s) 
                                                         (list (list (list (tm-getaccept m1) s) (list (tm-getaccept m1) RIGHT))
                                                               (list (list (tm-getaccept m1) s) (list (tm-getstart rm2) RIGHT)))) 
                                                       (tm-getalphabet m1))) ; w = xy --> rules to consume all of x before processing y
           (new-rules (append m1-rules
                              rm2-rules
                              new-rules-acceptM1-to-startRM2
                              new-rules-startM1-to-new-reject
                              )))
      (make-unchecked-tm new-states new-sigma new-rules new-start new-finals new-accept)))
  
  ; tm-kleenestar: tm --> tm
  ; ASSUMPTION: ***** The given tm is a language recognizer *****
  ; *** BUG: m1 only reaches Y by reading a blank then it never cycles back to m1's start state ***
  (define (tm-kleenestar m1)
    (let* ((m1-rules (remove-LM-rule-tm-rules (tm-getrules m1)))
           (new-sigma (tm-getalphabet m1))
           (new-start (generate-symbol START (tm-getstates m1)))
           (new-states (cons new-start (tm-getstates m1)))
           (new-finals (tm-getfinals m1))
           (new-accept (tm-getaccept m1))
           (new-rules-from-new-start (map (lambda (s) (list (list new-start s) (list (tm-getstart m1) s)))
                                          (tm-getalphabet m1)))
           (new-rules-acceptM1-to-startM1 (map (lambda (s) 
                                                 (list (list (tm-getaccept m1) s) (list (tm-getstart m1) RIGHT)))
                                               (tm-getalphabet m1)))
           (new-rules (append (list (list (list new-start BLANK) (list new-accept BLANK)))
                              new-rules-from-new-start 
                              (unparse-tmrules m1-rules) 
                              new-rules-acceptM1-to-startM1)))
      (make-unchecked-tm new-states new-sigma new-rules new-start new-finals new-accept)))
  
  ; tm-complement: tm --> tm
  ; ASSUMPTION: ***** The given deterministic tm is a language recognizer *****
  (define (tm-complement m1)
    (let* ((m1-rules (remove-LM-rule-tm-rules (tm-getrules m1)))
           (new-sigma (tm-getalphabet m1))
           (new-start (tm-getstart m1))
           (new-states (tm-getstates m1))
           (new-finals (tm-getfinals m1))
           (new-accept (let ((a (tm-getaccept m1))) (if (eq? a (car new-finals)) (cadr new-finals) (car new-finals)))) ; assumes there are only two states in new-finals as required by the construction for tm language recognizer
           (new-rules (unparse-tmrules m1-rules)))
      (make-unchecked-tm new-states new-sigma new-rules new-start new-finals new-accept)))
  
  ; tm-intersection: tm tm --> tm
  ; ASSUMPTION: ***** The given tms are language recognizers *****
  (define (tm-intersection m1 m2)
    
    (define (generate-reject-rules sts tost s)
      (map (lambda (st) (list (list st s) (list tost s))) sts))
    
    (define reject-m1 (let ((fnls (tm-getfinals m1))) (if (eq? (car fnls) (tm-getaccept m1)) (cadr fnls) (car fnls))))
    
    (define reject-m2 (let ((fnls (tm-getfinals m2))) (if (eq? (car fnls) (tm-getaccept m2)) (cadr fnls) (car fnls))))
    
    (define M1-NEW-REJECT-RULES (append-map (lambda (s) (generate-reject-rules (tm-getstates m1) reject-m1 s)) 
                                            (tm-getalphabet m2)))
    
    (define M2-NEW-REJECT-RULES (append-map (lambda (s) (generate-reject-rules (tm-getstates m2) reject-m2 s)) 
                                            (tm-getalphabet m1)))
    
    (define NEW-M1 (make-unchecked-tm (tm-getstates m1) 
                                      (tm-getalphabet m1)
                                      (append M1-NEW-REJECT-RULES
                                              (unparse-tmrules (remove-LM-rule-tm-rules (tm-getrules m1))))
                                      (tm-getstart m1)
                                      (tm-getfinals m1)
                                      (tm-getaccept m1)))
    
    (define NEW-M2 (make-unchecked-tm (tm-getstates m2) 
                                      (tm-getalphabet m2)
                                      (append M2-NEW-REJECT-RULES
                                              (unparse-tmrules (remove-LM-rule-tm-rules (tm-getrules m2))))
                                      (tm-getstart m2)
                                      (tm-getfinals m2)
                                      (tm-getaccept m2)))
    
    (lambda (w pos . L)
      (if (null? L)
          (let* ((r1 (NEW-M1 w 0)))
            (if (not (eq? r1 'accept))
                'reject
                (NEW-M2 w 0)))
          (error (format "The functionality ~s is not implemented yet....stay tuned!" (car L))))))
  
  ; tmconfig
  ; A tmconfig is a structure, (tmconfig a b c), where a is a state, b is a natnum, and c (listof symbol)
  (struct tmconfig (state index tape) #:transparent)
  
  ; (listof state) (listof symbol) (listof (list state symbol) (list state symbol)) state (listof state) --> tm
  ; FOR LANGUAGE RECOGNIZERS:
  ;  0. The optional argument, accept, must be a state
  ;  1. accept must be a symbol in H
  ;  2. H must contain only two states: one that is accepting and one that is rejecting
  ;  3. H is a subset of K
  (define (make-unchecked-tm K SIGMA delta s H . accept)
    
    ; (listof (list state symbol) (list state symbol)) --> (listof tmrule)
    (define (parse-tm-rules rls)
      (map (lambda (r) (tmrule (caar r) (cadar r) (caadr r) (cadadr r))) rls))
    
    ; (listof state) (listof symbol) (listof tmrule) state (listof state) --> tm
    (define (mk-tm K SIGMA delta s H . accept)
      
      ; tmconfig (listof tmrule) --> (listof tmconfig)
      (define (gen-newtm-configs c rls)
        
        (define (gen-new-tape i write-symb tape)
          (cond [(= i 0) (cons write-symb (cdr tape))]
                [else (cons (car tape) (gen-new-tape (- i 1) write-symb (cdr tape)))]))
        
        (map (lambda (r) 
               (cond [(eq? (tmrule-action r) RIGHT) (tmconfig (tmrule-tos r)
                                                              (+ (tmconfig-index c) 1)
                                                              (if (= (+ (tmconfig-index c) 1) (length (tmconfig-tape c)))
                                                                  (append (tmconfig-tape c) (list BLANK))
                                                                  (tmconfig-tape c)))]
                     [(eq? (tmrule-action r) LEFT) (tmconfig (tmrule-tos r)
                                                             (- (tmconfig-index c) 1)
                                                             (tmconfig-tape c))]
                     [else (tmconfig (tmrule-tos r) (tmconfig-index c) (gen-new-tape (tmconfig-index c) 
                                                                                     (tmrule-action r)
                                                                                     (tmconfig-tape c)))]))
             rls))
      
      ; (listof tmconfig) (list (listof tmconfig)) --> (listof tmconfig) 
      (define (consume visited tovisit)
        (cond [(null? tovisit) tovisit]
              [(and (not (null? accept)) ; the TM is a language recognizer & reached accept
                    (eq? (car accept) (tmconfig-state (caar tovisit))))
               (car tovisit)]
              [(and (not (null? accept)) ; the TM is a language recognizer & reached reject
                    (member (tmconfig-state (caar tovisit)) H))
               (consume visited (cdr tovisit))]
              [(and (null? accept)
                    (member (tmconfig-state (caar tovisit)) H))
               (car tovisit)]
              [else (let* ((path (car tovisit))
                           (config (car path))
                           (st (tmconfig-state config))
                           (read (list-ref (tmconfig-tape config) (tmconfig-index config)))
                           (rls (filter (lambda (r) (and (eq? st (tmrule-froms r)) (eq? read (tmrule-read r)))) delta))
                           (newconfigs (filter (lambda (c) (not (member c visited))) (gen-newtm-configs config rls))))
                      (consume (cons (caar tovisit) visited)
                               (append (cdr tovisit) 
                                       (map (lambda (c) (cons c path)) newconfigs))))]))
      
      ; tmconfig --> (list state index tape)
      (define (unparse-tmconfig c)
        (list (tmconfig-state c) (tmconfig-index c) (tmconfig-tape c)))
      
      (lambda (w i . L)
        (cond [(eq? w 'whatami) (if (null? accept) 'tm 'tm-language-recognizer)]
              [(empty? L) 
               (let* ((res (consume '() (list (list (tmconfig s i w)))))) ; res = (listof tmconfig)
                 (cond [(and (not (null? accept)) (null? res))
                        'reject]
                       [(null? res) "Failed computation: did not reach a halting state. Check your transition rules."]
                       [else (cond [(null? accept) (list 'Halt: (tmconfig-state (car res)))]
                                   [(eq? (car accept) (tmconfig-state (car res))) 'accept]
                                   [else 'reject])]))]
              [(eq? (car L) 'transitions) 
               (let ((res (consume '() (list (list (tmconfig s i w))))))
                 (cond [(and (not (null? accept)) (null? res))
                        'reject]
                       [(null? res) "Failed computation: did not reach a halting state. Check your transition rules."]
                       [else (reverse (map unparse-tmconfig res))]))]
              [(eq? (car L) 'lastconfig)
               (let ((res (consume '() (list (list (tmconfig s i w)))))) ; res = (listof tmconfig)
                 (cond [(null? res) "Failed computation: did not reach a halting state. Check your transition rules."]
                       [else (car res)]))]
              [(eq? (car L) 'get-delta) delta] ; parsed rules
              [(eq? (car L) 'get-rules) (unparse-tmrules delta)] ;;; NEED TO UNPARSE THE RULES!!!!
              [(eq? (car L) 'get-states) K]
              [(eq? (car L) 'get-alphabet) SIGMA]
              [(eq? (car L) 'get-start) s]
              [(eq? (car L) 'get-finals) H]
              [(eq? (car L) 'get-accept) (if (null? accept) null (car accept))]
              [else (error (format "Unknown request to tm: ~s" (car L)))])))
    (if (null? accept)
        (mk-tm K
               (remove-duplicates (cons LM SIGMA))
               (remove-duplicates
                (parse-tm-rules (append (map (lambda (s) (list (list s LM) (list s RIGHT)))
                                             (remove* H K))
                                        delta)))
               s
               H)
        (mk-tm K
               (remove-duplicates (cons LM SIGMA))
               (remove-duplicates
                (parse-tm-rules (append (map (lambda (s) (list (list s LM) (list s RIGHT)))
                                             (remove* H K))
                                        delta)))
               s
               H
               (car accept))))
  
  (define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
  (define (tm-getstates m) (m '() 0 'get-states))
  
  (define (tm-getfinals m) (m '() 0 'get-finals))

  (define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules
  
  (define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules
  
  (define (tm-getstart m) (m '() 0 'get-start))
  
  (define (tm-getaccept m) (m '() 0 'get-accept))
  
  (define (tm-whatami? m) (m 'whatami 0 'whatami))
  
  (define (tm-apply m w pos) (m w pos))
  
  (define (ctm-apply m w pos) (m w pos))
  
  (define (tm-showtransitions m w pos) (m w pos 'transitions))
  
  ; tm [natnum] --> (listof (list word symbol))
  (define (tm-test m . l)
    (define number-tests (if (null? l) NUM-TESTS (car l)))
    
    (let ((test-words
           (map (λ (w) (cons LM w))
                (cons `(,BLANK) 
                      (filter (lambda (w) (not (null? w)))
                              (generate-words number-tests
                                              (remove LM (tm-getalphabet m))
                                              null))))))
      (map (lambda (w) (list w (tm-apply m w 1))) test-words)))
  
  #| 
   A combined TM description, ctm, is either:
  1. empty list
  2. (cons tm ctm)
  3. (cons LABEL ctm)
  4. (cons symbol ctm)
  5. (cons (BR (listof (list symbol ctm))) ctm)
  6. (cons (goto LABEL) ctm)
  7. (cons ((var symbol) ctm) ctm)
  
  A LABEL is a natnum.  
  |#
  
  
  ; ctmd alphabet --> ctm
  (define (combine-tms inputctm sigma)
    
    ; symbol --> tm
    (define (make-writer a)
      (make-unchecked-tm '(s h) 
                         sigma 
                         (map (lambda (symb) (list (list 's symb) (list 'h a))) (cons BLANK sigma))
                         's
                         '(h)))
    
    (define WRITERS (map (lambda (s) (list s (make-writer s))) (cons BLANK sigma)))
    
    (lambda (tape i . l)      
      
      (define label? number?)
      
      ; ctm --> (listof (list number ctm))
      (define (label-pairs m)
        ;(define d (displayln (format "label-pairs m: ~s \n" m)))
        (cond [(null? m) null]
              [(procedure? (car m)) (label-pairs (cdr m))]
              [(label? (car m)) (cons (list (car m) (cdr m)) (label-pairs (cdr m)))]
              [(symbol? (car m)) (label-pairs (cdr m))]
              [(and (list? (car m)) (eq? (caar m) BRANCH)) 
               (append-map (lambda (ctm) (label-pairs ctm)) (cons (cdr m) (map cdr (cdar m))))]
              [(and (list? (car m)) (eq? (caar m) GOTO)) (label-pairs (cdr m))]        
              [(and (list? (car m)) (list? (caar m)) (eq? (caaar m) VAR)) (append (label-pairs (cdar m)) (label-pairs (cdr m)))]
              [else (error (format "~s found unknown element in ctm: ~s" "label-pairs" (car m)))]))
      
      
      (define (mt-env) null)
      
      (define (extend-env var val env)
        (cons (list var val) env))
      
      (define mt-env? null?)
      
      (define (apply-env env var)
        (cond [(mt-env? env) (error (format "While running a ctm found an unbound variable or label: ~s" var))]
              [(eq? (caar env) var) (cadar env)]
              [else (apply-env (cdr env) var)]))
      
      ; ctm env --> tmconfig
      ; ASSUMPTION: env is populated with all labels
      (define (eval m env laststate)
        (cond [(null? m) (tmconfig laststate i tape)] 
              [(symbol? (car m)) 
               (let ((val (apply-env env (car m))))
                 (eval (cons (cadr (assoc val WRITERS)) (cdr m)) env laststate))]
              [(procedure? (car m)) 
               (let* ((res ((car m) tape i 'lastconfig))
                      ;(d (displayln (format "state: ~s" res)))
                      )
                 (begin
                   (set! tape (tmconfig-tape res))
                   (set! i (tmconfig-index res))
                   (eval (cdr m) env (tmconfig-state res))))]
              [(label? (car m))
               (eval (cdr m) env laststate)]
              [(and (list? (car m)) (eq? (caar m) BRANCH)) 
               (let* ((branches (cdar m))
                      (the-branch-assoc (assoc (list-ref tape i) branches))
                      (the-branch (if (null? (cdr the-branch-assoc)) null (cdr the-branch-assoc))))
                 (eval the-branch env laststate))]
              [(and (list? (car m)) (eq? (caar m) GOTO)) (eval (apply-env env (cadar m)) env laststate)]
              [(and (list? (car m)) (list? (caar m)) (eq? (caaar m) VAR))
               (eval (append (cdar m) (cdr m)) (extend-env (cadaar m) (list-ref tape i) env) laststate)]
              [else (error (format "~s found unknown element in ctm: ~s" "eval" (car m)))]))
      
      (if (null? inputctm)
          (tmconfig HALT i tape)
          (eval inputctm (label-pairs inputctm) START))))

  ; L = a*
  (define Alla (make-unchecked-tm '(S Y N)
                                  `(a b ,LM)
                                  `(((S a) (S ,RIGHT))
                                    ((S b) (N b))
                                    ((S ,BLANK) (Y ,BLANK)))
                                  'S
                                  '(Y N)
                                  'Y))

  ; L = b*
  (define Allb (make-unchecked-tm '(S Y N)
                                  `(a b ,LM)
                                  `(((S b) (S ,RIGHT))
                                    ((S a) (N b))
                                    ((S ,BLANK) (Y ,BLANK)))
                                  'S
                                  '(Y N)
                                  'Y))

  (define Alla-Allb (tm-union Alla Allb))

  ) ; closes module