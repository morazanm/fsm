; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module cfg racket
  (require "constants.rkt" "misc.rkt" "word.rkt")
  (provide cfg make-unchecked-cfg cfg-rule cfg-get-alphabet cfg-get-v cfg-get-rules cfg-get-start cfg-get-the-rules cfg-rule-lhs cfg-rule-rhs
           remove-dead-nts cfg-union cfg-concat cfg-star cfg? cfg-rename-nts cfg-derive test-cfg) 
  
  ; A cfg-rule is a structure, (make-srule L R), where L is a symbol (non-terminal) and R
  ; is a (listof symbol).
  (struct cfg-rule (lhs rhs) #:transparent) ; simple rule
  
  ; A cfg is a structure, (cfg V sigma R S), where V and sigma are a (listof symbol), R
  ; is a (listof cfg-rule), and S is a symbol
  (struct cfg (v sigma rules s) #:transparent)
  (define (make-unchecked-cfg v sigma rules s)
    (make-cfg v sigma rules s))
  
  ; cfg --> (listof symbol)
  (define cfg-get-v cfg-v)
  
  ; cfg --> (listof symbol)
  (define cfg-get-alphabet cfg-sigma)
  
  ; cfg --> (listof (list symbol -> symbol))
  (define (cfg-get-rules g)  (unparse-cfg-rules (cfg-rules g)))
  
  ; cfg --> (listof cfg-rule)
  (define (cfg-get-the-rules g)  (cfg-rules g))
  
  ; cfg --> symbol
  (define cfg-get-start cfg-s)
  
  ; cfg --> cfg
  (define (remove-dead-nts g) 2)
  
  ; (listof cfg-rule) --> (listof (list symbol -> symbol))
  (define (unparse-cfg-rules rls)
    (map (lambda (r) (list (cfg-rule-lhs r) ARROW (fsmlos->symbol (cfg-rule-rhs r)))) rls))
  
  ;(listof (list symbol '-> symbol)) --> (listof cfg-rule)
  (define (parse-cfg-rules R)
    (map (lambda (r) (cfg-rule (car r) (symbol->fsmlos (caddr r)))) R))
  
  ; (listof symbol) (listof symbol) (listof (list symbol '-> symbol)) symbol --> cfg
  (define (make-cfg V sigma R S)
    (cfg V sigma (parse-cfg-rules R) S))
  
  ; cfg word --> (listof symbol) or string
  (define (cfg-derive g w)
    
    (define (get-first-nt st)
      (cond [(empty? st) #f]
            [(not (member (car st) (cfg-get-alphabet g))) (car st)]
            [else (get-first-nt (cdr st))]))
    
    (define (get-rules nt g) (filter (lambda (r) (eq? nt (cfg-rule-lhs r))) 
                                     (cfg-get-the-rules g)))
    
    ; ASSUMPTION: state has at least one NT
    (define (subst-first-nt state rght)
      (cond [(not (member (car state) (cfg-get-alphabet g)))
             (if (eq? (car rght) EMP) (cdr state) (append rght (cdr state)))]
            [else (cons (car state) (subst-first-nt (cdr state) rght))]))
    
    ; (listof (listof symbol)) --> (listof symbol)
    (define (get-starting-terminals st)
      (cond 
        [(not (member (car st) (cfg-get-alphabet g))) '()]
        [else (cons (car st) (get-starting-terminals (cdr st)))]))
    
    ; (listof (listof symbol)) natnum --> (listof symbol)
    (define (get-first-n-terms w n)
      (cond [(= n 0) '()]
            [else (cons (car w) (get-first-n-terms (cdr w) (- n 1)))]))
    
    
    ; (list (listof symbol)) --> boolean
    (define (check-terminals? st)
      (let* ((start-terms-st (get-starting-terminals st))
             (start-terms-w (if (> (length start-terms-st) (length w))
                                #f
                                (get-first-n-terms w (length start-terms-st)))))
        (cond [(false? start-terms-w) #f]
              [else (equal? start-terms-st start-terms-w)])))
    
    
    (define (make-deriv visited derivs g chomsky)
      
      (define (count-terminals st sigma)
        (length (filter (lambda (a) (member a sigma)) st)))
      
      (cond [(empty? derivs) (format "~s is not in L(G)" w)]
            [(or (and chomsky (> (length (caar derivs)) (+ 2 (length w))))
                 (> (count-terminals (caar derivs) (cfg-get-alphabet g)) (length w)))
             (make-deriv visited (cdr derivs) g chomsky)]
            [else 
             (let* ((fderiv (car derivs))
                    (state (car fderiv))
                    (fnt (get-first-nt state))
                    )
               (if (false? fnt)
                   (if (equal? w state)
                       (append-map (lambda (l) (if (equal? w l) 
                                                   (if (null? l) (list EMP) (list (los->symbol l))) 
                                                   (list (los->symbol l) ARROW))) 
                                   (reverse fderiv))
                       (make-deriv visited (cdr derivs) g chomsky))
                   (let*
                       ((rls (get-rules fnt g))
                        (rights (map cfg-rule-rhs rls))
                        (new-states (filter (lambda (st) (and (not (member st visited))
                                                              (check-terminals? state))) 
                                            (map (lambda (rght) (subst-first-nt state rght)) rights))))
                     (make-deriv (append new-states visited)
                                 (append (cdr derivs) 
                                         (map (lambda (st) (cons st fderiv)) 
                                              new-states))
                                 g
                                 chomsky))))]))
    (if (< (length w) 2)
        (format "The word ~s is too short to test." w)
        (let* (
               (ng (convert-to-cnf g))
               (ng-derivation (make-deriv (list (list (cfg-get-start ng))) 
                                          (list (list (list (cfg-get-start ng))))
                                          ng
                                          true))
               )
          (if (string? ng-derivation)
              ng-derivation
              (make-deriv (list (list (cfg-get-start g))) 
                          (list (list (list (cfg-get-start g))))
                          g
                          false)))))
  
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
    (let* ((newg2 (cfg-rename-nts (cfg-get-nts g1) g2))
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
    (let* ((newg2 (cfg-rename-nts (cfg-get-nts g1) g2))
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
             (let* ((newsym (gen-symbol (car nt2) (append nt1 nt2 newnt2 (list newS))))
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
               (cond [(empty? newmembers) #t]
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
  
  (define (fsmlos->symbol l) 
    (define (lostr->string l)
      (cond [(null? l) ""]
            [else (string-append (car l) (lostr->string (cdr l)))]))
    (string->symbol (lostr->string (map symbol->string l))))
  
  ; cfg --> cfg
  ; Purpose: Converts the given grammar to Chomsky Normal Form
  (define (convert-to-cnf g)  
    (let* ((nts (cfg-get-v g))
           (sigma (cfg-get-alphabet g))
           (start (cfg-get-start g))
           (rls (map (lambda (r) (if (equal? (caddr r) (list EMP)) (list (car r) ARROW null) r))
                     (map (lambda (r) (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
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
  
  ;;;;; End Chomsky Normal Form Functions
  
  ) ;closes module