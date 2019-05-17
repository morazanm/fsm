; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module csg racket
  (require "constants.rkt" "misc.rkt")
  (provide csg make-unchecked-csg csg-rule csg-getsigma csg-getv csg-getrules csg-getstart csg-get-unparsed-rules 
           csg-rule-lhs csg-rule-rhs csg? csg-rename-nts csg-derive csg-union csg-concat)
  
  ; A csg-rule is a structure, (make-srule L R), where L is a (listof symbol) and R
  ; is a (listof symbol).
  (struct csg-rule (lhs rhs) #:transparent)
  
  ; A csg is a structure, (csg V sigma R S), where V and sigma are a (listof symbol), R
  ; is a (listof csg-rule), and S is a symbol
  (struct csg (v sigma rules s) #:transparent)
  (define (make-unchecked-csg v sigma rules s)
    (make-csg v sigma rules s))
  
  ; csg --> (listof symbol)
  (define csg-getv csg-v)
  
  ; csg --> (listof symbol)
  (define csg-getsigma csg-sigma)
  
  ; csg --> (listof (list symbol -> symbol))
  (define (csg-get-unparsed-rules g)  (unparse-csg-rules (csg-rules g)))
  
  ; csg --> (listof csg-rule)
  (define (csg-getrules g)  (csg-rules g))
  
  ; csg --> symbol
  (define csg-getstart csg-s)
  
  ; (listof csg-rule) --> (listof (list symbol -> symbol))
  (define (unparse-csg-rules rls)
    (map (lambda (r) (list (los->symbol (csg-rule-lhs r)) ARROW (los->symbol (csg-rule-rhs r)))) rls))
  
  ;(listof (list (listof symbol) '-> (listof symbol))) --> (listof csg-rule)
  (define (parse-csg-rules R)
    (map (lambda (r) (csg-rule (symbol->list (car r)) (symbol->list (caddr r)))) R))
  
  ; (listof symbol) (listof symbol) (listof (list symbol '-> symbol)) symbol --> csg
  (define (make-csg V sigma R S)
    (csg V sigma (parse-csg-rules R) S))
  
  (define (make-csg2 V sigma R S)
    (csg V sigma R S))
  
  ; csg word --> (listof symbol) or string
  (define (csg-derive g w)
    
    ; csg-rule natnum (listof symbol) --> (listof (listof symbol))
    (define (use-csg-rule r str i)
   
      ; (listof symbol) (listof symbol) natnum --> (listof (listof symbol))
      (define (helper lhs rhs i)
        (cond [(< (- (length str) i) (length lhs)) '()]
              [else
               (let* ((subword (sublist str i (length lhs)))
                      )
                 (cond [(equal? lhs subword)
                        (if (equal? rhs (list EMP))
                            (cons (subst-in-list str i (length lhs) '())
                                  (helper lhs rhs (+ i 1)))
                            (cons (subst-in-list str i (length lhs) rhs)
                                  (helper lhs rhs (+ i 1))))]
                       [else (helper lhs rhs (+ i 1))]))]))
      (let ((res (helper (csg-rule-lhs r) (csg-rule-rhs r) i)))
          res))
    
    ; (listof symbol) (listof csg-rule) --> (listof (listof symbol))
    (define (apply-one-step curr rls)
      (cond [(null? rls) '()]
            [else
             (append (use-csg-rule (car rls) curr 0) (apply-one-step curr (cdr rls)))]))
    
    ; (listof symbol) (listof (listof (listof symbol))) -> (listof (listof symbol))
    (define (bfs-deriv generated-derivations tovisit)
      
      (define (ins paths)
        
        (define (insert path sortedpaths)
          (cond [(null? sortedpaths) (list path)]
                [(< (length (car path)) (length (caar sortedpaths)))
                 (cons path sortedpaths)]
                [else (cons (car sortedpaths) (insert path (cdr sortedpaths)))]))
        
        (cond [(null? paths) '()]
              [else (insert (car paths) (ins (cdr paths)))]))
      
      
      (cond [(null? tovisit) '()]
            [else
             (let* ((firstpath (car tovisit))
                    (current (car firstpath))
                    )
               (cond [(equal? w current) firstpath]
                     [else (let* ((new-words (apply-one-step current (csg-getrules g)))
                                  (newstrings (filter (lambda (s) (not (member s generated-derivations)))
                                                      new-words))
                                  (newpaths (ins (append (cdr tovisit) (map (lambda (s) (cons s firstpath)) newstrings))))
                                  )
                             (bfs-deriv (append newstrings generated-derivations) newpaths))]))]))
    
    (let* ((res (bfs-deriv '() (list (list (list (csg-getstart g))))))
           )
      (if (null? res)
          (format "~s is not in L(G)." w)
          (append-map (lambda (l) (if (equal? w l) 
                                      (if (null? l) (list EMP) (list (los->symbol l))) 
                                      (list (los->symbol l) ARROW)))
                      (reverse res)))
      ))
  
  ; csg-rename-nts: (listof symbol) csg --> cfg 
  (define (csg-rename-nts nts1 csg2)
    
    ; substitute-inlist : symb symb los --> los
    (define (substitute-inlist old new los)
      (map (lambda (s) (if (eq? s old) new s)) los))
    
    ; generate-table: (listof symbol) (listof symbol) --> (listof (list symb symb))
    (define (generate-table nts1 nts2)
      (map (lambda (s) (list s (generate-symbol s nts1))) nts2))
    
    ; update-rls: symb symb (listof rule)->(listof rule)
    (define (update-rls old new lr)
      (let* (( lhs (map (lambda (rl) (csg-rule-lhs rl)) lr))
             ( rhs (map (lambda (rl) (csg-rule-rhs rl)) lr))
             ( newlhs (map (lambda (r) (substitute-inlist old new r)) lhs))
             ( newrhs (map (lambda (r) (substitute-inlist old new r)) rhs)))
        (map (lambda (l r) (csg-rule l r)) newlhs newrhs)))
    
    ; generate-new-rls: (listof symb) table (listof rules) --> (listof rules)
    (define (generate-new-rls nts t lrs)
      (cond [(empty? nts) lrs]
            [else (generate-new-rls (cdr nts) t (update-rls (car nts) (cadr (assoc (car nts) t)) lrs))]))
    
    (let* ((nts2 (csg-getv csg2))
           (table (generate-table nts1 nts2))
           (rules (csg-getrules csg2))
           (new-rules (generate-new-rls nts2 table rules))
           (unparsed-new-rules (unparse-csg-rules new-rules))
           )
      (make-csg2 (map cadr table)
                 (csg-getsigma csg2)
                 new-rules
                 (cadr (assoc (csg-getstart csg2) table)))))
  
  ;csg-union : csg csg -> csg
  (define (csg-union g1 g2)
    (let* ((newg2 (csg-rename-nts (csg-getv g1) g2))
           (newnts (append (csg-getv g1) (csg-getv newg2)))
           (newsigma (remove-duplicates (append (csg-getsigma g1) (csg-getsigma newg2))))
           (newS (gen-symbol 'S newnts))
           (newV (cons newS newnts))
           (newR (cons (csg-rule (list newS) (list (csg-getstart g1))) 
                       (cons (csg-rule (list newS) (list (csg-getstart newg2)))
                             (append (csg-getrules g1) (csg-getrules newg2))))))
      (csg newV newsigma newR newS)))
  
  ;csg-concat: csg csg -> csg
  (define (csg-concat g1 g2)
    (let* ((newg2 (csg-rename-nts (csg-getv g1) g2))
           (newnts (append (csg-getv g1) (csg-getv newg2)))
           (newsigma (remove-duplicates (append (csg-getsigma g1) (csg-getsigma newg2))))
           (newS (generate-symbol 'S newnts))
           (newV (cons newS newnts))
           (newR (cons (csg-rule (list newS) (list (csg-getstart g1) (csg-getstart newg2)))
                       (append (csg-getrules g1) (csg-getrules newg2)))))
      (csg newV newsigma newR newS)))
  
  ) ;;; closes module