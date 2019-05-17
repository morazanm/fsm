; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

(module regular-grammar racket
  (require "constants.rkt" "misc.rkt" "word.rkt")
  (provide erule erule?
           srule srule? srule-lhs srule-rhs
           crule crule? crule-lhs crule-rhs1 crule-rhs2
           rg rg? rg-nts rg-sigma rg-rules rg-s
           make-unchecked-rg parse-rrules printable-rrules
           rg-rule-lhs rg-rule-rhs2 rg-rule-rhs1 rg-getalphabet rg-getnts rg-getstart rg-getrules rg-getunparsedrules
           rg-rename-nts rg-derive test-rg
           )
  
  ; S --> e
  (struct erule (lhs) #:transparent)
  
  ; NT --> symb
  ; A srule is a structure, (make-srule L R), where L is a symbol (non-terminal) and R
  ; is a terminal (symbol).
  (struct srule (lhs rhs) #:transparent) ; simple rule
  
  ; A crule is a structure, (make-crule L R1 R2), where L is a symbol (non-terminal) and R1
  ; is a terminal (symbol), and R2 is a non-terminal (symbol)
  (struct crule (lhs rhs1 rhs2) #:transparent) ; compound rule
  
  ; A rrule is either:
  ;  1. erule
  ;  2. srule
  ;  3. crule
  
  (define (rrule-unparse r)
    (cond [(erule? r) (list (erule-lhs r) ARROW EMP)]
          [(srule? r) (list (srule-lhs r) ARROW (srule-rhs r))]
          [else (list (crule-lhs r) ARROW (los->symbol 
                                           (list (crule-rhs1 r)
                                                 (crule-rhs2 r))))]))
  
  ; A list of rule (lorr) is a (listof rule)
  
  (define (lorr-unparse L) (map rrule-unparse L))
  
  ; A regular grammar (rg) is a structure, (make-rg N A R S), such that
  ; N is a (listof symbol) (the non-terminals), A is a (listof symbol) (the
  ; alphabet), R is a (listof rrule), and S is a symbol (starting symbol)
  (struct rg (nts sigma rules s) #:transparent)
  
  ; lor --> (listof (list symbol ARROW symbol))
  (define (printable-rrules g)
    ; symbol symbol --> string
    (define (mk-symb  s1 s2)
      (string->symbol (string-append (symbol->string s1) (symbol->string s2))))
    (define (gen-prules rls)
      (cond [(empty? rls) rls]
            [(erule? (car rls)) (cons (list (rg-s g) ARROW EMP) (gen-prules (cdr rls)))]
            [(srule? (car rls)) (cons (list (srule-lhs (car rls)) 
                                            ARROW 
                                            (srule-rhs (car rls))) 
                                      (gen-prules (cdr rls)))]
            [(crule? (car rls)) (cons (list (crule-lhs (car rls)) 
                                            ARROW 
                                            (mk-symb (crule-rhs1 (car rls)) (crule-rhs2 (car rls)))) 
                                      (gen-prules (cdr rls)))]))
    (let ((rules (rg-rules g)))
      (gen-prules rules)))
  
  ; (listof (list symbol ARROW symbol)) symbol (start) --> lor
  (define (parse-rrules l S)
    (cond [(empty? l) l]
          [(and (eq? (caar l) S) (eq? (caddar l) EMP)) 
           (cons (erule S) (parse-rrules (cdr l) S))]
          [else
           (let* ((r (car l))
                  (lhs-string (symbol->string (caddr r)))
                  (lhs-length (string-length lhs-string)))
             (cond [(= lhs-length 1) (cons (srule (car r) (caddr r)) (parse-rrules (cdr l) S))]
                   [(= lhs-length 2) (cons (crule (car r)
                                                  (string->symbol (substring lhs-string 0 1))
                                                  (string->symbol (substring lhs-string 1 2)))
                                           (parse-rrules (cdr l) S))]
                   [else (error (format "Invalid rule: ~s" r))]))]))
  
  (define (make-unchecked-rg V sigma rules S)
    (rg V sigma (if (list? (car rules)) (parse-rrules rules S) rules) S))
  
  ; rg-rename-nts : (listof symbol) rg -> rg       
  (define (rg-rename-nts nts1 g2)
    (define (generate-table nts1 nts2)
      (map (lambda (s) (list s (generate-symbol s nts1))) nts2))
    
    (define (update-rg-rule rule table)
      (cond [(erule? rule) (erule (cadr (assoc (erule-lhs rule) table)))]
            [(srule? rule) (srule (cadr (assoc (srule-lhs rule) table)) (srule-rhs rule))]
            [else (crule (cadr (assoc (crule-lhs rule) table)) (crule-rhs1 rule) (cadr (assoc (crule-rhs2 rule) table)))]))
    
    (let* ((r2 (rg-getrules g2))
           (nts2 (rg-getnts g2))
           (new-nts2-table (generate-table nts1 nts2))
           (new-rules2 (map (lambda (r) (update-rg-rule r new-nts2-table)) r2)))
      (rg (map cadr new-nts2-table) (rg-getalphabet g2) new-rules2 (cadr (assoc (rg-getstart g2) new-nts2-table )))))
  
  (define (rg-getnts g) (rg-nts g))
  
  (define (rg-getrules g) (rg-rules g))
  
  (define (rg-getunparsedrules g) (lorr-unparse (rg-rules g)))
  
  (define (rg-getstart g) (rg-s g))
  
  (define (rg-getalphabet g) (rg-sigma g))
  
  (define (rg-rule-lhs r) 
    (cond [(erule? r) (erule-lhs r)]
          [(srule? r) (srule-lhs r)]
          [else (crule-lhs r)]))
  
  (define (rg-rule-rhs1 r) 
    (cond [(erule? r) EMP]
          [(srule? r) (srule-rhs r)]
          [else (crule-rhs1 r)]))
  
  (define (rg-rule-rhs2 r) 
    (cond [(or (erule? r) (srule? r)) (error (format "This rule, ~s, only has one right hand side element." r))]
          [else (crule-rhs2 r)]))
  
  (define (rg-derive g w)
    
    (define (generate-nexts current r seen res)
      
      (define (generate-rhs r)
        (cond [(erule? r) '()]
              [(srule? r) (list (rg-rule-rhs1 r))]
              [else (list (rg-rule-rhs1 r) (rg-rule-rhs2 r))]))
      
      (cond [(null? current) res]
            [(not (eq? (rg-rule-lhs r) (car current))) 
             (generate-nexts (cdr current) r (append seen (list (car current))) res)]
            [else (generate-nexts (cdr current) r (append seen (list (car current))) (list (append seen
                                                                                                   (generate-rhs r)
                                                                                                   (cdr current))))]))
    
    
    (define (apply-one-step current rules)
      (let* ((current-nts (remove-duplicates (filter (lambda (s) (member s (rg-getnts g))) current)))
             (rls (filter (lambda (r) (member (rg-rule-lhs r) current-nts)) rules)))
        (append-map (lambda (r) (generate-nexts current r '() '())) rls)))
    
    ; word word --> boolean
    ; ASSUMPTION: s != w
    (define (same-start s w)
      (or (null? s) 
          (member (car s) (rg-getnts g)) 
          (and (not (null? w)) (eq? (car s) (car w)) (same-start (cdr s) (cdr w)))))
    
    
    (define (derive visited tovisit)
      
      (cond [(null? tovisit) null]
            [else (let* ((firstpath (car tovisit))
                         (curr (car firstpath)))
                    (cond [(equal? curr w) firstpath]
                          [else (let* 
                                    ((new-words (apply-one-step curr (rg-getrules g)))
                                     (newstrings (if (null? new-words) 
                                                     null
                                                     (filter (lambda (s) (and (<= (length s) (add1 (length w)))
                                                                              (same-start s w) 
                                                                              (not (member s visited))))
                                                             new-words)))
                                     (newpaths (append (cdr tovisit)
                                                       (map (lambda (s) (cons s firstpath)) newstrings))))
                                  (derive (cons curr visited) newpaths))]))]))
    
    (let* ((res (derive '() (list (list (list (rg-getstart g)))))))
      (if (null? res)
          (format "~s is not in L(G)." w)
          (append-map (lambda (l) (if (equal? w l) 
                                      (if (null? w) (list EMP) (list (los->symbol l))) 
                                      (list (los->symbol l) ARROW)))
                      (reverse res)))))
  
  ; rg natnum --> (listof (list word symbol))
  (define (test-rg g . l)
    (define number-tests (if (null? l) NUM-TESTS (car l)))
    
    (let ((test-words (generate-words number-tests (rg-getalphabet g) null)))
      (map (lambda (w) (list w (rg-derive g w))) test-words)))
  
  ) ;closes module
