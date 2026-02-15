; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan, 2015

#lang racket/base
(require "constants.rkt"
         "misc.rkt"
         "word.rkt"
         racket/list
         "cfg-leftmost-derive.rkt"
         "cfg-rightmost-derive.rkt"
         "cfg-levelmost-left-derive.rkt"
         "cfg-levelmost-right-derive.rkt"
         "cfg-struct.rkt")
(provide remove-dead-nts cfg-union cfg-concat cfg-star cfg-rename-nts cfg-derive test-cfg)
  
; cfg --> cfg
(define (remove-dead-nts g) 2)

; cfg word --> (listof symbol) or string
(define (cfg-derive g w #:deriv-order [deriv-order 'left])
  (cond [(eq? deriv-order 'left) (cfg-derive-leftmost g w)]
        [(eq? deriv-order 'right) (cfg-derive-rightmost g w)]
        [(eq? deriv-order 'level-left) (cfg-derive-level-leftmost g w)]
        [(eq? deriv-order 'level-right) (cfg-derive-level-rightmost g w)]
        [else (error "Not a valid derivation order")]))
  
;cfg --> (listof symbol)
(define (cfg-get-nts g)
  ;get-nts: (listof symbol) (listof symbol)--> (listof symbol)
  (define (get-nts v s)
    (filter (lambda (a) (not (member a s))) v ))
  (let ((gV (cfg-get-v g))
        (gsigma (cfg-get-alphabet g)))
    (get-nts gV gsigma)))
  
;cfg-union : cfg cfg -> cfg
(define (cfg-union g1 g2)
  (let* ((newg2 (cfg-rename-nts (append (cfg-get-nts g1)
                                        (cfg-get-nts g2))
                                g2))
         (newnts (append (cfg-get-nts g1) (cfg-get-nts newg2)))
         (newsigma (remove-duplicates (append (cfg-get-alphabet g1) (cfg-get-alphabet newg2))))
         (newS (gen-symbol 'S newnts))
         (newV (cons newS newnts))
         (newR (cons (cfg-rule newS (list (cfg-get-start g1))) 
                     (cons (cfg-rule newS (list (cfg-get-start newg2)))
                           (append (cfg-get-the-rules g1) (cfg-get-the-rules newg2))))))
    (cfg newV newsigma newR newS)))
  
;cfg-concatenation: cfg cfg -> cfg
(define (cfg-concat g1 g2)
  (let* ((newg2 (cfg-rename-nts (append (cfg-get-nts g1)
                                        (cfg-get-nts g2))
                                g2))
         (newnts (append (cfg-get-nts g1) (cfg-get-nts newg2)))
         (newsigma (remove-duplicates (append (cfg-get-alphabet g1) (cfg-get-alphabet newg2))))
         (newS (gen-symbol 'S newnts))
         (newV (cons newS (append newnts newsigma)))
         (newR (cons (cfg-rule newS (list (cfg-get-start g1) (cfg-get-start newg2)))
                     (append (cfg-get-the-rules g1) (cfg-get-the-rules newg2)))))
    (cfg newV newsigma newR newS)))
  
;cfg-star: cfg -> cfg
(define (cfg-star g1)
  (let* ((newsigma (cfg-get-alphabet g1))
         (newS (gen-symbol 'S (cfg-get-v g1)))
         (newV (cons newS (cfg-get-v g1)))
         (newR (cons (cfg-rule newS (list EMP))
                     (cons (cfg-rule newS (list (cfg-get-start g1) newS))
                           (append (cfg-get-the-rules g1))))))
    (cfg newV newsigma newR newS)))
  
; cfg-rename-nts : (listof symbol) cfg -> cfg       
(define (cfg-rename-nts nts1 g2)
    
  ;update-rhs: sym sym (listof symbol) --> (lisof symbol)
  (define (update-rhs old new rls)
    (map (lambda (s) (if (eq? s old) new s)) rls))
    
  ;update-rl: symb symb (listof rule)->(listof rule)
  (define (update-rl old new lrs)      
    (let* (( lhs (map (lambda (rl) (cfg-rule-lhs rl)) lrs))
           ( rhs (map (lambda (rl) (cfg-rule-rhs rl)) lrs))
           ( newlhs (map (lambda (s) (if (eq? s old) new s)) lhs))
           ( newrhs (map (lambda (r) (update-rhs old new r)) rhs)))
      (map (lambda (l r) (cfg-rule l r)) newlhs newrhs)))
    
  ;cfg-update-rule: (listof symbol) (listof symbol) (listof rule)(listof symbol) symb -> ((listof symbol) (listof rule)symb)
  (define (cfg-update-rule nt1 nt2 rls2 newnt2 newS)      
    (cond [(null? nt2) (list newnt2 rls2 newS)]
          [(member (car nt2) nt1) 
           (let* ((newsym (gen-nt (append nt1 nt2 newnt2 (list newS)))
                          #;(gen-symbol (car nt2) (append nt1 nt2 newnt2 (list newS))))
                  (newrls2 (update-rl (car nt2) newsym rls2)))
             (cfg-update-rule nt1 (cdr nt2) newrls2 (cons newsym newnt2) (if (eq? (car nt2) newS) newsym newS)))]
          [else (cfg-update-rule nt1 (cdr nt2) rls2 (cons (car nt2) newnt2) newS)]))
    
  (let* (( nts2 (cfg-get-nts g2))
         (rules2 (cfg-get-the-rules g2))
         (res (cfg-update-rule nts1 nts2 rules2 '() (cfg-get-start g2)))
         (newnts2 (car res))
         (newsigma (cfg-get-alphabet g2))
         (newV newnts2)
         (newR (cadr res))
         (newnts (append nts1 newnts2))
         (newS (caddr res))
         (newg (cfg newV newsigma newR newS )))
    newg))
  
; cfg --> boolean
(define (Lcfg-isempty? g)
    
  ; cfg-rule (listof symbol) --> (listof cfg-rule)
  (define (only-accum-elems? rule accum)
    (and (not (member (cfg-rule-lhs rule) accum))
         (andmap (lambda (s) (member s accum)) (cfg-rule-rhs rule))))
    
  ; (listof cfg-rules) nonterminal (listof symbol) --> boolean
  ; Invariant: The accumulator contains the terminal symbols and the lhss that
  ;            whose rhss only contain symbols in the accumulator
  (define (isempty? rls S accum)
    (cond [(member S accum) #f]
          [else
           (let ((newmembers (map cfg-rule-lhs (filter (lambda (r) (only-accum-elems? r accum))rls))))
             (cond [(null? newmembers) #t]
                   [else (isempty? rls S (append newmembers accum))]))]))
    
  (let ((rls (cfg-get-the-rules g))
        (S (cfg-get-start g))
        (sigma (cfg-get-alphabet g)))
    (isempty? rls S (cons EMP sigma)))
  )
  
; cfg natnum --> (listof (list word (derivation or string)))
(define (test-cfg g . l)
  (define number-tests (if (null? l) NUM-TESTS (car l)))
    
  (let ((test-words (generate-words number-tests (cfg-get-alphabet g) null)))
    (map (lambda (w) (list w (cfg-derive g w))) test-words)))
  
;;;;;; Chomsky Normal Form Functions
  
(define (transform-rules-for-terminals rls sigma)
    
  (define (update-rules-for-singletons rls table)
    (define (process-rule r)
      (cons (car r) 
            (cons (cadr r) 
                  (list (map (lambda (a)
                               (let ((p (assoc a table)))
                                 (if p (cadr p) a)))
                             (caddr r))))))
    (map process-rule rls)
    )
    
  (let* ((table (map (lambda (a) 
                       (list a 
                             (generate-symbol (string->symbol (string-upcase (symbol->string a))) sigma))) 
                     sigma))
         (new-singleton-rules (map (lambda (p) (list (cadr p) ARROW (list (car p)))) table)) 
         (updated-rules (update-rules-for-singletons rls table)))
    (append updated-rules new-singleton-rules)))
  
(define (transform-long-rules rls)
  (define (transform-rule lhs rule)
    (cond [(= (length (caddr rule)) 2) (list rule)]
          [else (let ((newsymb (generate-symbol lhs null)))
                  (cons (list (car rule) ARROW (list (caaddr rule) newsymb))
                        (transform-rule lhs (list newsymb ARROW (cdaddr rule)))))]))
  (append-map (lambda (r) (transform-rule (car r) r)) rls))
  
(define (substitute-start start rls)
  (define (sub-start-in-rule start new r)
    (list (if (eq? (car r) start) new (car r))
          ARROW
          (map (lambda (s) (if (eq? s start) new s)) (caddr r))))
  (let ((new-to-S (generate-symbol start null)))
    (cons (list start ARROW (list new-to-S))
          (map (lambda (r) (sub-start-in-rule start new-to-S r)) rls))))
  
(define (get-nts->emp rls res)
  (let ((emp-rules (filter (lambda (r) (null? (caddr r))) rls)))
    (if (null? emp-rules)
        res
        (let* ((new-symbs (map car emp-rules))
               (new-res (remove-duplicates (append new-symbs res)))
               (new-rules (map (lambda (r) 
                                 (cons (car r) 
                                       (cons ARROW
                                             (cond [(= (length (caddr r)) 1)
                                                    (if (member (caaddr r) new-symbs) 
                                                        (list null) 
                                                        (list (list (caaddr r))))]
                                                   [(and (member (caaddr r) new-symbs)
                                                         (member (cadr (caddr r)) new-symbs))
                                                    (list null)]
                                                   [(member (caaddr r) new-symbs) 
                                                    (list (list (cadr (caddr r))))]
                                                   [(member (cadr (caddr r)) new-symbs) 
                                                    (list (list (caaddr r)))]
                                                   [else (list (list (caddr r)))]))))
                               (filter (lambda (r) (not (null? (caddr r)))) rls))))
          (get-nts->emp new-rules new-res)))))
  
(define (remove-to-emp-rules rls gen-emp start)
  (append 
   (filter (lambda (r) (= (length (caddr r)) 1)) rls) ; keep rules with lhs of length 1 
   (append-map (lambda (r) 
                 (let* ((lhs (car r))
                        (rhs (caddr r))
                        (frst (car rhs))
                        (sec (cadr rhs))
                        (mf (member frst gen-emp))
                        (ms (member sec gen-emp)))
                   (cond [(and (eq? lhs start) (null? rhs)) (list r)]
                         [(and (eq? frst sec) mf) (list r (list lhs ARROW (list frst)))]
                         [(and mf ms) (list r 
                                            (list lhs ARROW (list sec)) 
                                            (list lhs ARROW (list frst)))]
                         [mf (list r
                                   (list lhs ARROW (list sec)))]
                         [ms (list r 
                                   (list lhs ARROW (list frst)))]
                         [else (list r)])))
               (filter (lambda (r) (= (length (caddr r)) 2))
                       rls))))
  
(define (remove-unit-rules rls sigma)
    
  (define (find-neighs a rls)
    (append-map caddr (filter (lambda (r) (eq? a (car r))) rls)))
    
  (define (compute-sccs G toprocess stack res)
      
    (define (in-scc? a sccs)
      (ormap (lambda (scc) (member a scc)) sccs))
      
    (define (stacked? n s)
      (member n (map car s)))
      
    (define (get-node n g)
      (car (filter (lambda (node) (eq? n (car node))) g)))
      
    (define (compute-scc n st)
      (cond [(null? st) null]
            [(eq? n (caar st)) (list (caar st))]
            [else (cons (caar st) (compute-scc n (cdr st)))]))
      
    (define (get-res-sccs scc res)
      (filter (lambda (a) (member-scc a res)) scc))
      
    (define (member-scc a res)
      (cond [(null? res) null]
            [(member a (car res)) (cons (car res) (member-scc a (cdr res)))]
            [else (member-scc a (cdr res))]))
      
    (define (sort-los l)
      (sort l (lambda (i j) (string<=? (symbol->string i) (symbol->string j)))))
      
    (define (update-sccs scc res)
      (let* ((to-merge (list (sort-los (get-res-sccs scc res))))
             (temp-res (filter (lambda (sc) (not (member sc to-merge))) res)))
        (append (list (sort-los (remove-duplicates (append scc (flatten to-merge))))) 
                temp-res)))
      
    (cond [(and (null? toprocess) (null? stack)) res]
          [(null? stack) 
           (compute-sccs G (cdr toprocess) (list (get-node (car toprocess) G)) res)]
          [else
           (let* ((node (car stack))
                  (name (car node))
                  (neighs (cadr node)))
             (cond [(null? neighs)
                    (if (in-scc? name res)
                        (compute-sccs G toprocess (cdr stack) res)
                        (compute-sccs G toprocess (cdr stack) (cons (list name) res)))]
                   [(not (stacked? (car neighs) stack))
                    (let* ((members (map car G))
                           (p (member (car neighs) members))
                           (nd (if p (get-node (car neighs) G) null)))
                      (if (null? nd)
                          (compute-sccs G 
                                        toprocess
                                        (cons (list name (cdr neighs)) (cdr stack))
                                        res)
                          (compute-sccs G 
                                        toprocess
                                        (cons nd
                                              (cons (list name (cdr neighs)) (cdr stack)))
                                        res)))]
                   [else 
                    (let* ((name (car neighs))
                           (scc (compute-scc name stack))
                           (new-st (cons (list name (cdr neighs)) (cdr stack)))
                           (new-res (update-sccs scc res))
                           )
                      (compute-sccs G toprocess new-st new-res))]))]))
    
  (define (remove-nts rls nt-sccs)
      
    (define (process-scc sc rules)
      (map (lambda (r) (update-rule r sc)) rules))
      
    (define (update-rule r sc)
      (let* ((new (car sc))
             (new-lhs (if (member (car r) sc) new (car r)))
             (new-rhs (map (lambda (a) (if (member a sc) new a)) (caddr r))))
        (list new-lhs ARROW new-rhs)))
      
    (cond [(null? nt-sccs) rls]
          [else (remove-nts (process-scc (car nt-sccs) rls) (cdr nt-sccs))]))
    
  (define (trim-neighs g goodneighs)
    (filter (lambda (nd) (not (null? (cadr nd))))
            (map (lambda (node) (list (car node) 
                                      (filter (lambda (n) (member n goodneighs)) (cadr node)))) 
                 g)))
    
  (define (reverse-topological-order g tovisit)
    (if (null? g)
        null
        (let* ((first-nodes (filter
                             (lambda (nm) (no-neigh-in-tovisit nm tovisit g))
                             (map car 
                                  (filter 
                                   (lambda (nd) (not (member (caadr nd) tovisit)))
                                   g))))
               (new-g (filter (lambda (nd) (not (member (car nd) first-nodes))) g))
               (new-tovisit (remove* first-nodes tovisit)))
          (append first-nodes (reverse-topological-order new-g new-tovisit)))))
    
  (define (no-neigh-in-tovisit nm tovisit g)
    (let* ((nm-neighs (append-map (lambda (nd)
                                    (if (eq? (car nd) nm) (cadr nd) null))
                                  g)))
      (andmap (lambda (n) (not (member n tovisit))) nm-neighs)))
    
  (define (rm-unit-rls rules unitrls order sigma)
      
    (define (gen-rls target-rls rules res)
      (cond [(null? target-rls) res]
            [else
             (let* ((firstrl (car target-rls))
                    (lhs (car firstrl))
                    (rhs (caaddr firstrl))
                    (rhs-rules (filter (lambda (r) (eq? rhs (car r)))
                                       rules))
                    (to-add-rls (map (lambda (r) (list lhs ARROW (caddr r))) rhs-rules))
                    )
               (gen-rls (cdr target-rls) rules (append to-add-rls res)))]))
      
      
      
    (cond [(null? order) rules]
          [else
           (let* ((curr (car order))
                  (target-rls (filter (lambda (r) (and (eq? curr (car r))
                                                       (not (member (caaddr r) sigma)))) 
                                      unitrls))
                  (new-rules (gen-rls target-rls rules null))
                  )
             (rm-unit-rls (append (filter (lambda (r) (not (member r target-rls))) rules) 
                                  new-rules)
                          (filter (lambda (r) (not (member r target-rls))) unitrls) 
                          (cdr order) 
                          sigma))]))
    
  (let* ((unit-rules (filter (lambda (r) (= (length (caddr r)) 1)) rls))
         (lhss (remove-duplicates (map car unit-rules)))
         (graph (map (lambda (nt) (list nt (find-neighs nt unit-rules))) lhss))
         (sccs (compute-sccs graph lhss '() null))
         (nontrivial-sccs (filter (lambda (sc) (> (length sc) 1)) sccs))
         (new-rls1 (filter (lambda (r) 
                             (or (not (= (length (caddr r)) 1))
                                 (and (= (length (caddr r)) 1)
                                      (not (eq? (car r) (caaddr r))))))
                           (remove-duplicates (remove-nts rls nontrivial-sccs))))
         (new-unit-rules (filter (lambda (r) (= (length (caddr r)) 1)) new-rls1))
         (new-lhss (remove-duplicates (map car new-unit-rules)))
         (new-graph (map (lambda (nt) (list nt (find-neighs nt new-unit-rules))) new-lhss))
         (rev-topological-order (reverse-topological-order new-graph new-lhss))
         (transformed-rules (rm-unit-rls new-rls1 new-unit-rules rev-topological-order sigma))
         )
    transformed-rules))
  


#| BUG: Keeps eliminated NTs in the RHS of rules

  ; cfg --> cfg
  ; Purpose: Converts the given grammar to Chomsky Normal Form
  (define (convert-to-cnf g)  
    (let* ((nts (cfg-get-v g))
           (sigma (cfg-get-alphabet g))
           (start (cfg-get-start g))
           (rls (map (lambda (r)
                       (if (equal? (caddr r) (list EMP))
                           (list (car r) ARROW null)
                           r))
                     (map (lambda (r)
                            (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                          (cfg-get-rules g))))
           (new-rls1 (transform-rules-for-terminals rls sigma))
           (long-rules (filter (lambda (r) (> (length (caddr r)) 2)) new-rls1))
           (short-rules (filter (lambda (r) (<= (length (caddr r)) 2)) new-rls1))
           (new-long-rules (transform-long-rules long-rules))
           (new-rls2 (append short-rules new-long-rules))
           (new-rls3 (substitute-start start new-rls2))
           (nts-gen-emp (get-nts->emp new-rls3 '()))
           (new-rls4 (remove-to-emp-rules new-rls3 nts-gen-emp start))
           (new-rls5 (remove-unit-rules new-rls4 sigma))
           (new-rls6 (map (lambda (r) (list (car r) ARROW (fsmlos->symbol (caddr r)))) 
                          new-rls5))
           )
      (make-cfg (remove-duplicates (map car new-rls6)) sigma new-rls6 start)))
  |#

; cfg --> cfg
; Purpose: Converts the given grammar to Chomsky Normal Form (START,TERM,BIN,DEL,UNIT)
(define (convert-to-cnf G)
  (define (start-grammar G)
    (let* ((nts (cfg-get-v G))
           (sigma (cfg-get-alphabet G))
           (start (cfg-get-start G))
           (rls (map (lambda (r)
                       (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                     (cfg-get-rules G)))
           (newS (generate-symbol start nts)))
      (make-cfg (cons newS nts)
                sigma
                (cons (list newS ARROW start)
                      (map (λ (r)
                             (list (car r) ARROW (los->symbol (caddr r))))
                           rls))
                newS)))

  ;; cfg --> cfg
  ;; Purpose: Eliminate rules with nonsolitary terminals
  (define (term-grammar G)
    (define (populate-hash! ht sigma nts)
      (if (null? sigma)
          ht
          (let ((new-nt (generate-symbol (symbol-upcase (car sigma))
                                         (append sigma nts))))
            (begin
              (hash-set! ht (car sigma) new-nt)
              (populate-hash! ht (cdr sigma) (cons new-nt nts))))))

    (define (convert-non-solitary rule sigma ht)
      (let ((rhs (caddr rule)))
        (if (= (length rhs) 1)
            rule
            (list (car rule)
                  ARROW
                  (map (λ (s)
                         (if (member s sigma)
                             (hash-ref ht s)
                             s))
                       rhs)))))
    
    (let* ((nts (cfg-get-v G))
           (sigma (cfg-get-alphabet G))
           (start (cfg-get-start G))
           (rls (map (lambda (r)
                       (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                     (cfg-get-rules G)))
           (ht (populate-hash! (make-hash) sigma nts))
           ;(ddd (displayln ht))
           (solitary-new-rules (map (λ (a) (list (hash-ref ht a) ARROW (list a)))
                                    sigma))
           (new-rls (map (λ (r) (convert-non-solitary r sigma ht)) rls)))
      (make-cfg (append nts
                        (map (λ (a) (hash-ref ht a)) sigma))
                sigma
                (map (λ (r)
                       (list (car r) ARROW (los->symbol (caddr r))))
                     (append new-rls solitary-new-rules))
                start)))

  ;; cfg --> cfg
  ;; Purpose: Eliminate rules with a rhs that has more than 2 nts
  ;; Assume: For all rhs, if length > 2 it contains only nts
  (define (bin-grammar G)

    (define (convert-rule r nts)
      (if (= (length (caddr r)) 2)
          (list r)
          (cons (list (car r) (cadr r) (list (car (caddr r)) (car nts)))
                (convert-rule (list (car nts) ARROW (cdr (caddr r)))
                              (cdr nts)))))

    (define (make-new-rls rls new-nts)
      (if (null? rls)
          '()
          (append (convert-rule (car rls)
                                (take new-nts (- (length (caddr (car rls))) 2)))
                  (make-new-rls (cdr rls)
                                (drop new-nts (- (length (caddr (car rls))) 2))))))
      
    (let* ((nts (cfg-get-v G))
           (sigma (cfg-get-alphabet G))
           (start (cfg-get-start G))
           (trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
           (short-trls
            (filter (λ (r)
                      (<= (length (caddr r)) 2))
                    trls))
           (to-process-trls
            (filter (λ (r)
                      (> (length (caddr r)) 2))
                    trls))
           (num-new-nts (foldl (λ (r a) (+ (- (length (caddr r)) 2) a))
                               0
                               to-process-trls))
           (new-nts (build-list num-new-nts (λ (i) (generate-symbol 'T (cons 'T nts)))))
           (new-rls (make-new-rls to-process-trls new-nts))
           ;(ddd (displayln (format "~s" (append nts new-nts))))
           )
      (make-cfg (append nts new-nts)
                sigma
                (map (λ (r)
                       (list (car r) ARROW (los->symbol (caddr r))))
                     (append short-trls new-rls))
                start)))

  ;; (listof cfg-rule) --> (listof cfg-rule)
  ;; Purpose: Remove rules with nts in the rhs that do not appear in a lhs
  (define (remove-silly-rules rls start sigma)
    (let ((lhs (map car rls)))
      (filter (λ (r)
                (and (or (eq? (car r) start)
                         (member (car r) (append-map third rls)))
                     (let ((r-nts (filter (λ (a) (not (member a sigma))) (caddr r))))
                       (andmap (λ (nt) (member nt (cons EMP lhs))) r-nts))))
              rls)))

  ;; cfg --> cfg
  ;; Purpose: Remove e-rules from given grammar
  (define (del-grammar G)

    (define (compute-nullables rls nulls visited-nulls)
      (let* ((new-nullables (map
                             car
                             (filter (λ (r) (andmap (λ (a) (and (not (member (car r) visited-nulls))
                                                                (member a nulls)))
                                                    (caddr r)))
                                     rls)))
             ;;(ddd (displayln (format "nulls: ~s \n new: ~s\n\n" nulls new-nullables)))
             )
        (if (null? new-nullables)
            (append nulls visited-nulls)
            (compute-nullables
             rls
             (remove-duplicates new-nullables)
             (append nulls visited-nulls)))))

    (define (convert-rule r nullables)
        
      (define (nullables-pos rhs pos)
        (cond [(null? rhs) '()]
              [(member (car rhs) nullables)
               (cons pos (nullables-pos (cdr rhs) (add1 pos)))]
              [else (nullables-pos (cdr rhs) (add1 pos))]))
              
      (let* ((lhs (car r))
             (rhs (caddr r))
             (nullables-at (nullables-pos rhs 0)))
        (if (null? nullables-at)
            (list r)
            (cons r (map (λ (pos)
                           (let ((tmp-rhs (append (take rhs pos)
                                                  (drop rhs (add1 pos)))))
                             (list lhs
                                   ARROW
                                   (if (null? tmp-rhs)
                                       (list EMP)
                                       tmp-rhs))))                               
                         nullables-at)))))
      
    (let* ((nts (cfg-get-v G))
           (sigma (cfg-get-alphabet G))
           (start (cfg-get-start G))
           (trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
           ;(start-trl (car (filter (λ (r) (eq? (car r) start)) trls)))
           ;(ddd (display (format "trls: \n ~s \n" trls)))
           (nullables (compute-nullables trls
                                         (map car (filter {λ (r) (equal? (caddr r) (list EMP))}
                                                            trls))
                                         '()))
           (new-rules (map (λ (r)
                             (list (car r) ARROW (los->symbol (caddr r))))
                           (remove-silly-rules
                            (filter (λ (r)
                                      (or (eq? (car r) start)
                                          (and (not (eq? (car r) start))
                                               (not (equal? (caddr r) (list EMP))))))
                                    (append-map (λ (r)
                                                  (remove-duplicates (convert-rule r nullables)))
                                                trls))
                            start
                            sigma)))
           ;(dd (display (format "new rules: \n ~s \n" new-rules)))
           (new-nts (map car new-rules)))
      (make-cfg new-nts
                sigma
                new-rules
                start)))

  ;; cfg --> cfg
  ;; Purpose: Remove unit rules
  (define (unit-grammar G)

    (define (rm-unit-rules rls nts)

      (define (new-rules-for-urule rls urule)
        ;; urule form: A -> B
        (let* ((A (car urule))
               (B (caddr urule))
               (B-rules (filter (λ (r) (eq? (car B) (car r))) rls)))
          (map (λ (r) (list A ARROW (caddr r))) B-rules)))
      (let ((urules (filter (λ (r) (and (= (length (caddr r)) 1)
                                        (member (car (caddr r)) nts)))
                            rls)))
        (if (null? urules)
            rls
            (rm-unit-rules (append (remove (car urules) rls)
                                   (new-rules-for-urule
                                    (remove (car urules) rls)
                                    (car urules)))
                           nts))))

      
    (let* ((nts (cfg-get-v G))
           (sigma (cfg-get-alphabet G))
           (start (cfg-get-start G))
           (trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
           (unit-rules (filter (λ (r)
                                 (and (= (length (caddr r)) 1)
                                      (member (car (caddr r)) nts)))
                               trls))
           (new-rules (map (λ (r)
                             (list (car r) ARROW (los->symbol (caddr r))))
                           (remove-silly-rules
                            (rm-unit-rules trls nts)
                            start
                            sigma)))
           (new-nts (map car new-rules))
           ;(d (display (format "trls: \n ~s \n\n" trls)))
           )
      (make-cfg (remove-duplicates new-nts)
                sigma
                new-rules
                start)
      ;(display (format "ORIG rules: \n ~s \n \n RES new-rules: ~s \n" trls new-rules))
      ;new-rules
      ))
                  
      
  (unit-grammar
   (del-grammar (bin-grammar (term-grammar (start-grammar G))))))

(define G (make-cfg '(X Y Z A)
                    '(a b c e)
                    (list '(X -> ε)
                          '(Y -> XXZ)
                          '(Y -> XX)
                          '(Z -> abc)
                          '(A -> e))
                    'Y))

(define BPG (make-cfg '(S)
                      '(o c)
                      `((S ,ARROW ,EMP)
                        (S ,ARROW SS)
                        (S ,ARROW oSc))
                      'S))
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))



;;;;; End Chomsky Normal Form Functions
  
;closes module
