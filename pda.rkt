; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module pda racket
  (require "word.rkt" "constants.rkt" "cfg.rkt" "misc.rkt")
  
  (provide make-unchecked-ndpda rename-states-pda cfg->pda pda->cfg
           pdarule-fromstate pdarule-readsymb pdarule-pop pdarule-tostate pdarule-push
           pda-getrules pda-getalphabet pda-getgamma pda-getstates pda-getfinals pda-getstart
           pda-showtransitions  show-transitions-pda apply-pda test-pda
           union-pda concat-pda kleenestar-pda)
  ;;; pdaconfig
  
  ; A pdaconfig is a (list state wi stack). 
  
  (define pdaconfig-state car)
  
  (define pdaconfig-wi cadr)
  
  (define pdaconfig-stack caddr)
  
  (define mk-pdaconfig list)
  
  ;;;; end pdaconfig
  
  ;;; pdarule
  
  ; A pda-rule is (list (state symbol symbol*) (state symbol*))
  
  (define pdarule-fromstate caar)
  
  (define pdarule-readsymb cadar)
  
  (define pdarule-pop caddar)
  
  (define pdarule-tostate caadr)
  
  (define pdarule-push cadadr)
  
  (define (push-length pushstuff)
    (if (eq? pushstuff EMP) 0 (length pushstuff)))
  
  ;;; end pdarule
  
  ;;; pdapath
  
  ; A pdapath is a (listof config) that can be either
  ;  1. empty
  ;  2. (cons configuration (listof config))
  
  (define mk-pdapath cons)
  
  (define first-config-pdapath car)
  
  (define rest-config-pdapath cdr)
  
  (define empty-config-pdapath? null?)  
  
  ;;; end 
  
  ;;; stack
  
  ;;; STACK
  ;;; Observers: empty-stack?, top, pop
  ;;; Constructors: empty-stack, push
  
  (define (empty-stack) '())
  
  (define (stack-push v S) 
    (if (eq? v EMP) S (append v S)))
  
  (define empty-stack? null?)
  
  (define stack-size length)
  
  (define (stack-top S)
    (cond [(empty-stack? S) (error "Top of an empty stack error")]
          [else (car S)]))
  
  (define (stack-top-n S n)
    (cond [(= n 0) null]
          [(empty-stack? S) (error "Top-n of small stack error")]
          [else (cons (stack-top S) (stack-top-n (stack-pop S (list (stack-top S))) (- n 1)))]))
  
  (define (stack-pop S v)
    (cond [(eq? v EMP) S]
          [(empty-stack? S) (error "Pop of an empty stack error")]
          [else (list-tail S (length v))]))
  
  (define (stack-pop-n S n)
    (cond [(< (length S) n) (error "Pop-n of small stack error")]
          [else (list-tail S n)]))
  
  ;;; end stack
  
  
  ; (listof pdaconfig) (listof pdapaths) --> (listof pdaconfigs)
  ; Purpose: returns the list of all generated pdaconfigurations
  (define (generated-configs visited tovisit) (append visited (map first-config-pdapath tovisit)))
  
  ; (listof config) (listof config) --> (listof config)
  (define (filter-pdaconfigs new-configs gen-configs)
    (filter (lambda (c) (not (member c gen-configs))) new-configs))
  
  
  
  ; make-unchecked-ndpda: (listof states) alphabet alphabet state (listof states) (listof pdarules) --> ndpda
  (define (make-unchecked-ndpda K sigma gamma start finals pdarules)
    ; (listof pdaconfigs) word --> (listof pdaconfigs)
    (define (get-pdaconfig-accepts new-configs w)
      (filter (lambda (c) (and (member (pdaconfig-state c) finals)
                               (empty-stack? (pdaconfig-stack c))
                               (= (word-length w) (pdaconfig-wi c))))    
              new-configs))
    
    ; rule --> (listof symbol)
    (define (convert-pop r)
      (let ((pop-val (pdarule-pop r)))
        (if (eq? pop-val EMP) '() pop-val)))
    
    ; pdaconfig word --> (listof pdaconfig)
    (define (mk-pdatransitions c w)
      ; pdarule --> pdaconfig
      (define (mk-pdatransition r)
        (mk-pdaconfig (pdarule-tostate r)
                      (if (eq? (pdarule-readsymb r) EMP) (pdaconfig-wi c) (+ (pdaconfig-wi c) 1))
                      (stack-push (pdarule-push r) (stack-pop (pdaconfig-stack c) (pdarule-pop r)))))
      
      (let* ((state (pdaconfig-state c))
             (wi (pdaconfig-wi c))
             (S (pdaconfig-stack c))
             (read (if (= wi (length w)) '() (get-symb-word w wi)))
             (rls (filter (lambda (r) (and (eq? state (pdarule-fromstate r))
                                           (or (eq? read (pdarule-readsymb r)) (eq? EMP (pdarule-readsymb r)))
                                           (or (eq? (pdarule-pop r) EMP)
                                               (and (>= (stack-size S) (length (convert-pop r)))
                                                    (equal? (convert-pop r) 
                                                            (stack-top-n S (length (convert-pop r))))))))
                          pdarules)))
        (map mk-pdatransition rls)))
    
    ; word (listof pdaconfig) (listof pdapaths) --> pdapath or 'reject
    ; Purpose: BFS search for accepting pdapath
    (define (consume w visited tovisit)
      (define (new-pda-paths new-configs path)
        (map (lambda (c) (cons c path)) new-configs))
      
      (cond [(null? tovisit) 'reject]
            [else
             (let* ((first-path (car tovisit))
                    (config (first-config-pdapath first-path))
                    (new-configs (filter-pdaconfigs (mk-pdatransitions config w)
                                                    (generated-configs visited tovisit)))
                    (accepts (get-pdaconfig-accepts (cons config new-configs) w)))
               (if (not (null? accepts))
                   (reverse (mk-pdapath (car accepts) first-path))
                   (consume w (cons config visited) (append (cdr tovisit)
                                                            (new-pda-paths new-configs first-path)))))]))
    (lambda (w . L)
      (cond [(eq? w 'whatami) 'pda]
            [(empty? L) 
             (let ((res (consume w '() (list (list (mk-pdaconfig start 0 (empty-stack)))))))
               (if (list? res) 'accept 'reject))]
            [(eq? (car L) 'transitions) 
             (let ((res (consume w '() (list (list (mk-pdaconfig start 0 (empty-stack)))))))
               (if (eq? res 'reject)
                   'reject
                   (append (map (lambda (config) (mk-pdaconfig (pdaconfig-state config)
                                                               (list-tail w (pdaconfig-wi config))
                                                               (pdaconfig-stack config)))
                                res)
                           (list 'accept))
                   ))]
            [(eq? (car L) 'get-deltas) pdarules]
            [(eq? (car L) 'get-rules) pdarules]
            [(eq? (car L) 'get-states) K]
            [(eq? (car L) 'get-sigma) sigma]
            [(eq? (car L) 'get-gamma) gamma]
            [(eq? (car L) 'get-start) start]
            [(eq? (car L) 'get-finals) finals] 
            [else (error (format "Unknown request to pda: ~s" (car L)))])))
  
  (define (pda-getrules m) (m '() 'get-rules))
  
  (define (pda-getstates m) (m '() 'get-states))
  
  (define (pda-getgamma m) (m '() 'get-gamma))
  
  (define (pda-getstart m) (m '() 'get-start))
  
  (define (pda-getfinals m) (m '() 'get-finals))
  
  (define (pda-getalphabet m) (m '() 'get-sigma))
  
  (define (pda-apply m w) (m w))
  
  (define (pda-showtransitions m w) (m w 'transitions))
  
  ; cfg --> ndpda
  (define (cfg->pda g)
    ; (listof symbol) (listof cfg-rule) symbol --> (listof pdarules)
    (define (make-pda-rules sigma grules S)
      (let ((r1 `(((p ,EMP ,EMP) (q ,(list S)))))
            (r2 (map (lambda (r) `((q ,EMP ,(list (cfg-rule-lhs r))) (q ,(if (equal? (cfg-rule-rhs r) (list EMP))
                                                                      EMP
                                                                      (cfg-rule-rhs r))))) 
                     grules))
            (r3 (map (lambda (a) `((q ,a ,(list a)) (q ,EMP))) sigma)))
        (append r1 r2 r3)))
    
    (let ((K '(p q))
          (sigma (cfg-get-alphabet g))
          (gamma (cfg-get-v g))
          (S 'p)
          (F '(q))
          (delta (make-pda-rules (cfg-get-alphabet g) 
                                 (cfg-get-the-rules g) 
                                 (cfg-get-start g))))
      (make-unchecked-ndpda K sigma gamma S F delta)))
  
  
  ; ndpda -> ndpda
  ; convert the given pda into a simple pda
  (define (pda->spda m)
    
    ;(listof pdarule) --> (listof pdarule)
    (define (pdarules->simplepdarules rules newstart newgamma newstates)
      
      ; (listof symbol) state (listof pdarule) (listof state) --> (list (listof pdarule) (listof state))
      (define (create-push-rules pushlist froms tos rls states)
        (if (null? (cdr pushlist))
            (cons (cons (list (list (car states) EMP EMP) (list tos pushlist)) rls)
                  states)
            (let* ((ns (gen-symbol froms states))
                   (newrule (list (list (car states) EMP EMP) (list ns (list (car pushlist))))))
              (create-push-rules (cdr pushlist) tos (cons newrule rls) (cons ns states)))))
      
      ; (listof symbol) state (listof state) -->
      ;pdarule --> (listof pdarule)
      ; ASSUMPTION: r pushes 2 or more elements
      (define (convert-push-rule r states)
        (let* ((poplist (pdarule-pop r))
               (pushstuff (reverse (pdarule-push r)))
               (readelem (pdarule-readsymb r))
               (froms (pdarule-fromstate r))
               (tos (pdarule-tostate r))
               (ns (gen-symbol froms states))
               (res (create-push-rules (cdr pushstuff) froms tos (list (list (list froms readelem poplist) (list ns (list (car pushstuff))))) (cons ns states))))
          res))
      
      
      
      ; (listof pdarules) (listof pdarules) (listof state) --> (list (listof pdarule) (listof state))
      (define (convert-push-rules rls savedrls states)
        (if (null? rls)
            (cons savedrls states)
            (let* ((res (convert-push-rule (car rls) states))
                   (newrls (car res))
                   (newstates (cdr res)))
              (convert-push-rules (cdr rls) (append savedrls newrls) newstates))))
      
      ; (listof symbol) (listof symbol) symbol state state (listof state) --> (list (listof pdarule) (listof state))
      (define (create-pop-rules poplist pushstuff readelem froms tos rls states)
        (if (null? (cdr poplist))
            (cons (cons (list (list (car states) readelem poplist) (list tos pushstuff)) rls)
                  states)
            (let* ((ns (gen-symbol froms states))
                   (newrule (list (list (car states) EMP (list (car poplist))) (list ns EMP))))
              (create-pop-rules (cdr poplist) pushstuff readelem froms tos (cons newrule rls) (cons ns states)))))
      
      ;pdarule  (listof state) --> (list (listof pdarule) state+))
      ; ASSUMPTION: r pops 2 or more elements
      (define (convert-pop-rule r states)
        (let* ((poplist (pdarule-pop r))
               (pushstuff (pdarule-push r))
               (readelem (pdarule-readsymb r))
               (froms (pdarule-fromstate r))
               (tos (pdarule-tostate r))
               (ns (gen-symbol froms states))
               (res (create-pop-rules (cdr poplist) pushstuff readelem froms tos (list (list (list froms EMP (list (car poplist))) (list ns EMP))) (cons ns states))))
          res))
      
      ; (listof pdarules) (listof pdarules) (listof states) --> (list (listof pdarules) states+)
      (define (convert-pop-rules rls rules states)
        (if (null? rls)
            (cons rules states)
            (let* ((res (convert-pop-rule (car rls) states))
                   (newrls (car res))
                   (newstates (cdr res)))
              (convert-pop-rules (cdr rls) (append newrls rules) newstates))))
      
      ; pdarule (listof pdarules) (listof symbol) --> (list (listof pdarule) state+)
      (define (convert-emptypop-rule r rls gamma)
        (if (null? gamma)
            rls
            (let ((newrule (list (list (pdarule-fromstate r) (pdarule-readsymb r) (list (car gamma)))
                                 (list (pdarule-tostate r) 
                                       (if (eq? (pdarule-push r) EMP)
                                           (list (car gamma))
                                           (list (car (pdarule-push r)) (car gamma)))))))
              (convert-emptypop-rule r (cons newrule rls) (cdr gamma)))))
      
      ; (listof pdarules) (listof pdarules) (listof state) --> (listof pdarules)
      (define (convert-emptypop-rules rls savedrules)
        (if (null? rls)
            savedrules
            (let* ((res (convert-emptypop-rule (car rls) savedrules newgamma)))
              (convert-emptypop-rules (cdr rls) res))))
      
      
      (let* ((poprules (filter (lambda (r) (and (not (eq? (pdarule-pop r) EMP))
                                                (>= (length (pdarule-pop r)) 2)))
                               rules))
             (res1 (convert-pop-rules poprules 
                                      (filter (lambda (r) (not (member r poprules))) rules)
                                      newstates))
             (newpdarules1 (car res1))
             (newpdastates1 (cdr res1))
             (pushrules (filter (lambda (r) (and (not (eq? (pdarule-push r) EMP))
                                                 (>= (length (pdarule-push r)) 2))) 
                                newpdarules1))
             (res2 (convert-push-rules pushrules
                                       (filter (lambda (r) (not (member r pushrules))) newpdarules1)
                                       newpdastates1))
             (newpdarules2 (car res2))
             (newpdastates2 (cdr res2))
             (emptypoprules (filter (lambda (r) (and (not (eq? (pdarule-fromstate r) newstart))
                                                     (eq? (pdarule-pop r) EMP))) 
                                    newpdarules2))
             (newpdarules3 (convert-emptypop-rules emptypoprules
                                                   (filter (lambda (r) (not (member r emptypoprules))) newpdarules2))))
        (cons newpdarules3 newpdastates2)))
    
    (let* ((newS (gen-symbol 'S (pda-getstates m)))
           (newF (gen-symbol  'F (cons newS (pda-getstates m))))
           (K (cons newS (cons newF (pda-getstates m))))
           (sigma (pda-getalphabet m))
           (Z (gen-symbol 'Z (pda-getgamma m)))
           (gamma (cons Z (pda-getgamma m)))
           (res (pdarules->simplepdarules (append (cons (list (list newS EMP EMP) 
                                                              (list (pda-getstart m) (list Z)))
                                                        (map (lambda (f) 
                                                               (list (list f EMP (list Z)) (list newF EMP)))
                                                             (pda-getfinals m)))
                                                  (pda-getrules m)) 
                                          newS 
                                          gamma
                                          K))
           (delta (car res))
           (finalK (cdr res)))
      (make-unchecked-ndpda finalK sigma gamma newS (list newF) delta)))
  
  ; ndpda --> cfg
  (define (pda->cfg m)
    
    ; (simple)ndpda symbol --> (listof cfg-rule)
    (define (cfgrules-pda->cfg sm S)
      
      ; (listof pdarule) (listof state) --> (listof pdarule)
      ; ASSUMPTION: The rules push < 2 elems
      (define (gen-type2-rules rls K Gamma)
        
        ; pdarule -> (listof cfgrule)
        (define (mk-type2-rules r)
          
          ; state --> (listof cfgrule)
          (define (t2-maker s) 
            (map (lambda (g) (cfg-rule (let* ((new-nt (los->symbol 
                                                       (list (pdarule-fromstate r) 
                                                             (if (eq? (pdarule-pop r) EMP) 
                                                                 EMP 
                                                                 (car (pdarule-pop r))) 
                                                             s))))
                                         new-nt)
                                       (if (eq? (pdarule-readsymb r) EMP)
                                           (list (los->symbol (list (pdarule-tostate r) g s)))
                                           (list (pdarule-readsymb r) (los->symbol (list (pdarule-tostate r) g s))))))
                 Gamma))
          
          (append-map (lambda (s) (t2-maker s)) K))
        
        (append-map (lambda (r) (mk-type2-rules r)) rls))
      
      ; (listof pdarule) (listof state) --> (listof cfg-rule)
      ; ASSUMPTION: The rules push 2 elems
      (define (gen-type3-rules rls states)
        
        ; pdarule --> (listof cfg-rule)
        (define (t3-maker r)
          
          ; state --> (listof cfg-rule)
          (define (gen-t3 p)
            (map (lambda (p1) (cfg-rule (los->symbol (list (pdarule-fromstate r) (if (eq? (pdarule-pop r) EMP) EMP (car (pdarule-pop r))) p))
                                        (if (eq? (pdarule-readsymb r) EMP)
                                            (list (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                                  (los->symbol (list p1 (cadr (pdarule-push r)) p)))
                                            (list (pdarule-readsymb r)
                                                  (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                                  (los->symbol (list p1 (cadr (pdarule-push r)) p))))))
                 states))
          
          (append-map (lambda (p) (gen-t3 p)) states))
        
        (append-map (lambda (r) (t3-maker r)) rls))
      
      ; (listof state) --> (listof cfg-rule)
      (define (gen-type4-rules states) 
        (map (lambda (s) (cfg-rule (los->symbol (list s EMP s)) (list EMP))) states))
      
      (let* ((R1 (list (cfg-rule S (list (los->symbol (list (pda-getstart m) 'Z (car (pda-getfinals sm))))))))
             (R2 (gen-type2-rules (filter (lambda (r) (< (push-length (pdarule-push r)) 2))
                                          (pda-getrules sm))
                                  (pda-getstates sm)
                                  (cons EMP (pda-getgamma sm))))
             (R3 (gen-type3-rules (filter (lambda (r) (= (push-length (pdarule-push r)) 2))
                                          (pda-getrules sm))
                                  (cons EMP (pda-getstates sm))))
             (R4 (gen-type4-rules (pda-getstates sm)))
             )
        (append R1 R2 R3 R4)))
    
    (let* ((sm (pda->spda m))
           (S 'S)
           (SIGMA (pda-getalphabet sm))
           (R (cfgrules-pda->cfg sm S))
           (V (append (remove-duplicates (map (lambda (r) (cfg-rule-lhs r)) R)) SIGMA))
           )
      (cfg V SIGMA R S)))
  
  ; pda --> pda
  (define (rename-states-pda los m)
    (let* ((sts (pda-getstates m))
           (rename-table (map (lambda (s) (list s (generate-symbol s los)))
                              sts))
           (new-states (map (lambda (s) (cadr (assoc s rename-table))) sts))
           (new-start (cadr (assoc (pda-getstart m) rename-table)))
           (new-finals (map (lambda (s) (cadr (assoc s rename-table))) (pda-getfinals m)))
           (new-rules (map (lambda (r) (list (list (cadr (assoc (pdarule-fromstate r) rename-table))
                                                   (pdarule-readsymb r)
                                                   (pdarule-pop r))
                                             (list (cadr (assoc (pdarule-tostate r) rename-table))
                                                   (pdarule-push r))))
                           (pda-getrules m))))
      (make-unchecked-ndpda new-states 
                  (pda-getalphabet m) 
                  (pda-getgamma m) 
                  new-start 
                  new-finals 
                  new-rules)))
  
  ; union-pda: pda pda --> pda
  (define (union-pda p1 p2)
    (let* ((newp2 (rename-states-pda (pda-getstates p2) p2))
           (sts (append (pda-getstates p1) (pda-getstates newp2)))
           (newS (gen-symbol 'S sts))
           (newStates (cons newS sts))
           (newSigma (remove-duplicates (append (pda-getalphabet p1) (pda-getalphabet newp2))))
           (newGamma (remove-duplicates (append (pda-getgamma p1) (pda-getgamma newp2))))
           (newRules (append `((( ,newS ,EMP ,EMP) ( ,(pda-getstart p1) ,EMP))
                               (( ,newS ,EMP ,EMP) ( ,(pda-getstart newp2) ,EMP)))
                             (pda-getrules p1)
                             (pda-getrules newp2)))
           (newFinals (append (pda-getfinals p1) (pda-getfinals newp2))))
      (make-unchecked-ndpda newStates newSigma newGamma newS newFinals newRules)))
  
  ; concat-pda: pda pda --> pda
  (define (concat-pda p1 p2)
    (let* ((newp2 (rename-states-pda (pda-getstates p2) p2))
           (newStates (append (pda-getstates p1) (pda-getstates newp2)))
           (newS (pda-getstart p1))
           (newSigma (remove-duplicates (append (pda-getalphabet p1) (pda-getalphabet newp2))))
           (newGamma (remove-duplicates (append (pda-getgamma p1) (pda-getgamma newp2))))
           (newRules (append (map (lambda (f) (list (list f EMP EMP) (list (pda-getstart newp2) EMP)))
                                  (pda-getfinals p1))
                             (pda-getrules p1)
                             (pda-getrules newp2)))
           (newFinals (pda-getfinals newp2)))
      (make-unchecked-ndpda newStates newSigma newGamma newS newFinals newRules)))
  
  ; kleenestar-pda: pda --> pda 
  (define (kleenestar-pda p1)
    (let* ((newS (generate-symbol START (pda-getstates p1)))
           (newStates (cons newS (pda-getstates p1)))
           (newFinals (cons newS (pda-getfinals p1)))
           (newRules (append (list (list (list newS EMP EMP) (list (pda-getstart p1) EMP))) 
                             (map (lambda (s)(list (list s EMP EMP) (list (pda-getstart p1) EMP))) 
                                  (pda-getfinals p1))
                             (pda-getrules p1))))
      (make-unchecked-ndpda newStates (pda-getalphabet p1) (pda-getgamma p1) newS newFinals newRules)))
  
  ; show-transitions-pda : pda word --> listof pda-config
  (define (show-transitions-pda p1 w)
    (p1 w 'transitions))
  
  ; apply-pda: pda word --> symbol 
  (define (apply-pda p1 w) (p1 w))
  
  ; test-pda: pda [natnum] --> (listof (list word symbol))
  (define (test-pda p1 . l)
    (define number-tests (if (null? l) NUM-TESTS (car l)))
    (let ((test-words (generate-words number-tests (pda-getalphabet p1) null)))
      (map (lambda (w) (list w (apply-pda p1 w))) test-words)))
  
  ); closes module

