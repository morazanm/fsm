#lang racket/base

(require racket/set
         racket/list
         "cfg-struct.rkt"
         "misc.rkt"
         "constants.rkt")

(provide chomsky)

(define (filter-save pred lst)
  (define (filter-save-helper unprocessed-lst pred-lst rest-lst)
    (if (null? unprocessed-lst)
        (values pred-lst rest-lst)
        (if (pred (car unprocessed-lst))
            (call-with-values (lambda () (values (cdr unprocessed-lst) (cons (car unprocessed-lst) pred-lst) rest-lst))
                              filter-save-helper)
            (call-with-values (lambda () (values (cdr unprocessed-lst) pred-lst (cons (car unprocessed-lst) rest-lst)))
                              filter-save-helper))))
  (call-with-values (lambda () (values lst '() '())) filter-save-helper))

(define (gen-state disallowed)
  (lambda ()
    (define STS '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

    ;; state natnum --> symbol
    ;; Purpose: To append a dash and the given natnum to the given state
    (define (concat-n s n)
      (string->symbol (string-append (symbol->string s) "-" (number->string n))))

    ;; natnum (listof states) --> state
    ;; Purpose: Generate an allowed state
    (define (gen-helper n s-choices)
      (if (not (null? s-choices))
          (begin
            (set-add! disallowed (car s-choices))
            (car s-choices))
          (gen-helper (add1 n)
                      (filter (λ (s) (not (set-member? disallowed s)))
                              (map (λ (s) (concat-n s n)) STS)))))

    (gen-helper 0 (filter (λ (a) (not (set-member? disallowed a))) STS))))

(define (chomsky G)
  (define alphabet-set (list->seteq (cfg-get-alphabet G)))
  
  (define (start-grammar G)
    (define newS (gen-nt (cfg-get-v G)))
    (make-unchecked-cfg (cons newS (cfg-get-v G))
                        (cfg-get-alphabet G)
                        (cons (list newS ARROW (cfg-get-start G))
                              (cfg-get-rules G))
                        newS))
  
  ;; cfg --> cfg
  ;; Purpose: Eliminate rules with nonsolitary terminals
  (define (term-grammar G)
    (define ht (make-hash))
    (define nts (list->mutable-seteq (cfg-get-v G)))
    (define (populate-hash!)
      (define gen-nt (gen-state nts))
      (for ([sigma-elem (in-list (cfg-get-alphabet G))]
            #:do [(define new-nt (gen-nt))])
        (hash-set! ht sigma-elem new-nt)))

    (define (convert-non-solitary rule ht)
      (list (car rule)
            ARROW
            (los->symbol (map (λ (s)
                                (if (set-member? alphabet-set s)
                                    (hash-ref ht s)
                                    s))
                              (symbol->fsmlos (caddr rule))))))
    (populate-hash!)
    (make-unchecked-cfg (append (cfg-get-v G) (hash-values ht))
                        (cfg-get-alphabet G)
                        (append (map (λ (a) (list (hash-ref ht a) ARROW a))
                                     (cfg-get-alphabet G))
                                (map (λ (r) (convert-non-solitary r ht))
                                     (cfg-get-rules G)))
                        (cfg-get-start G)))

  ;; cfg --> cfg
  ;; Purpose: Eliminate rules with a rhs that has more than 2 nts
  ;; Assume: For all rhs, if length > 2 it contains only nts
  (define (bin-grammar G)
    (define nts (list->mutable-seteq (cfg-get-v G)))
    (define gen-nt (gen-state nts))
    
    (define (convert-rules rules)
      (define (convert-rule lhs rhs count)
        (if (= count 0)
            (list (list lhs ARROW (los->symbol rhs)))
            (let ([new-nt (gen-nt)])
              (cons (list lhs ARROW (los->symbol (list (car rhs) new-nt)))
                    (convert-rule new-nt (cdr rhs)
                                  (sub1 count))))))
      (cond [(null? rules)
             '()]
            [else
             (define rhs (symbol->fsmlos (caddr (car rules))))
             (define rhs-len (length rhs))
             (if (> rhs-len 2)
                 (append (convert-rule (caar rules) rhs (- rhs-len 2))
                         (convert-rules (cdr rules)))
                 (cons (car rules)
                       (convert-rules (cdr rules))))]))
    (define new-rules (convert-rules (cfg-get-rules G)))
    (make-unchecked-cfg (set->list nts)
                        (cfg-get-alphabet G)
                        new-rules
                        (cfg-get-start G)))

  ;; cfg -> cfg
  ;; Removes completely nullable nonterminals before proceeding with DEL step
  (define (remove-completely-nullable-nts G)
      (define trls (map (lambda (r)
                          (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                        (cfg-get-rules G)))
      (define nts (cfg-get-v G))
      (define nt-to-rules-rhs (make-hasheq))
      (for ([rule (in-list trls)])
        (define lhs (car rule))
        (define curr-lst (hash-ref nt-to-rules-rhs lhs '()))
        (hash-set! nt-to-rules-rhs lhs (cons (caddr rule) curr-lst)))
      (define completely-nullable-nts
        (for/list ([nt (in-list nts)]
                   #:do [(define nt-rules (hash-ref nt-to-rules-rhs nt))]
                   #:when (andmap (lambda (nt-rule-rhs)
                                    (andmap (lambda (elem) (equal? elem EMP)) nt-rule-rhs))
                                  nt-rules))
          (if (eq? nt (cfg-get-start G))
              (error "Starting nonterminal is completely nullable. Cannot continue with transformation")
              nt)))
      (cond [(null? completely-nullable-nts) G]
            [else
             (define new-rules (for/list ([rule (in-list trls)]
                                          #:when (not (member (car rule) completely-nullable-nts)))
                                 (list (car rule) (cadr rule) (fsmlos->symbol (for/list ([elem (in-list (caddr rule))]
                                                                         #:when (not (member elem completely-nullable-nts)))
                                                                elem)))))
             (define new-nts (for/list ([nt (in-list nts)]
                                        #:when (not (member nt completely-nullable-nts)))
                               nt))
             (remove-completely-nullable-nts
              (make-unchecked-cfg new-nts
                                  (cfg-get-alphabet G)
                                  new-rules
                                  (cfg-get-start G)))]))
  
  ;; cfg --> cfg
  ;; Purpose: Remove e-rules from given grammar
  (define (del-grammar G)
    
    (define trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
    
    (define (new-new-compute-nullables trls)
      (define nulls
        (list->mutable-seteq
         (for/list ([rule (in-list trls)]
                    #:when (equal? (caddr rule) (list EMP)))
           (car rule))))
      
      (define (all-nulls-on-rhs? rule)
        (and (not (set-member? nulls (car rule)))
             (andmap (lambda (elem) (set-member? nulls elem)) (caddr rule))))
      
      (define (found-new-nullables rules)
        (cond [(null? rules)
               (find-new-nullables trls)]
              [else
               (cond [(all-nulls-on-rhs? (car rules))
                      (set-add! nulls (car (car rules)))
                      (found-new-nullables (rest rules))]
                     [else (found-new-nullables (rest rules))])]))
      
      (define (find-new-nullables rules)
        (cond [(null? rules)
               (set->list nulls)]
              [else
               (cond [(all-nulls-on-rhs? (car rules))
                      (set-add! nulls (car (car rules)))
                      (found-new-nullables (rest rules))]
                     [else (find-new-nullables (rest rules))])]))
      
      (find-new-nullables trls))
    
    (define (remove-nts null-elem idx-lst count word)
      (if (null? word)
          '()
          (if (eq? (car word) null-elem)
              (if (member count idx-lst)
                  (remove-nts null-elem idx-lst (add1 count) (cdr word))
                  (cons (car word) (remove-nts null-elem idx-lst (add1 count) (cdr word))))
              (cons (car word) (remove-nts null-elem idx-lst count (cdr word))))))
    
    (define (convert-rules rules nulls-list)
      (for/fold ([new-rules rules])
                ([null-elem (in-list nulls-list)])
        (define-values (null-rules rest-rules)
          (filter-save (lambda (rule) (ormap (lambda (elem) (eq? elem null-elem)) (caddr rule))) new-rules))
        (for/fold ([new-rules rest-rules])
                  ([null-rule (in-list null-rules)])
          (append
           (for/list ([removed-elems (in-combinations (range (count (lambda (elem) (eq? elem null-elem)) (caddr null-rule))))]
                      #:do [(define new-rhs (remove-nts null-elem removed-elems 0 (caddr null-rule)))]
                      #:when (not (null? new-rhs)))
             (list (car null-rule)
                   ARROW
                   new-rhs))
           new-rules))))

    (define new-rules
      (map (λ (r)
             (list (car r) ARROW (los->symbol (caddr r))))
           (remove-duplicates (filter (λ (r)
                                        (or (eq? (car r) (cfg-get-start G))
                                            (and (not (eq? (car r) (cfg-get-start G)))
                                                 (not (equal? (caddr r) (list EMP))))))
                                      (convert-rules trls (new-new-compute-nullables trls))))))
    
    (make-unchecked-cfg (remove-duplicates (map car new-rules))
                        (cfg-get-alphabet G)
                        new-rules
                        (cfg-get-start G)))

  ;; cfg --> cfg
  ;; Purpose: Remove unit rules
  (define (unit-grammar G)
    (define nts (list->mutable-seteq (cfg-get-v G)))

    (define (unit-rule? rule)
      (and (= (length (caddr rule)) 1)
           (set-member? nts (car (caddr rule)))))
    (define (replace-unit-rule lhs rhs rls)
      (if (null? rls)
          '()
          (if (eq? (car (car rls)) rhs)
              (cons (list lhs (cadr (car rls)) (caddr (car rls)))
                    (cons (car rls) (replace-unit-rule lhs rhs (cdr rls))))
              (cons (car rls) (replace-unit-rule lhs rhs (cdr rls))))))
    (define (remove-unit-rules rls)
      (define (find-unit-rule rls checked-rls)
        (if (null? rls)
            (values #f (void))
            (if (unit-rule? (car rls))
                (values (car rls) (append (cdr rls) checked-rls))
                (find-unit-rule (cdr rls) (cons (car rls) checked-rls)))))
      (define-values (maybe-unit-rule other-rules) (find-unit-rule rls '()))
      (if maybe-unit-rule
          (remove-unit-rules (replace-unit-rule (car maybe-unit-rule) (car (caddr maybe-unit-rule)) other-rules))
          rls))
    (define new-rules
      (remove-duplicates
       (map (λ (r)
              (list (car r) ARROW (los->symbol (caddr r))))
            (remove-unit-rules (map (lambda (r)
                                      (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                                    (cfg-get-rules G))))))
    (make-unchecked-cfg (remove-duplicates (map car new-rules))
                        (cfg-get-alphabet G)
                        new-rules
                        (cfg-get-start G)))
  
  (unit-grammar (del-grammar (remove-completely-nullable-nts (bin-grammar (term-grammar (start-grammar G)))))))