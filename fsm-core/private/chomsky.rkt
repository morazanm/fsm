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
            (call-with-values (lambda () (values (cdr unprocessed-lst) (cons (car unprocessed-lst) pred-lst) rest-lst)) filter-save-helper)
            (call-with-values (lambda () (values (cdr unprocessed-lst) pred-lst (cons (car unprocessed-lst) rest-lst))) filter-save-helper))))
  (call-with-values (lambda () (values lst '() '())) filter-save-helper))

(define (chomsky G)
  (define alphabet-set (list->seteq (cfg-get-alphabet G)))
  (define (start-grammar G)
    (let ((newS (generate-symbol (cfg-get-start G) (cfg-get-v G))))
      (make-cfg (cons newS (cfg-get-v G))
                (cfg-get-alphabet G)
                (cons (list newS ARROW (cfg-get-start G))
                      (cfg-get-rules G))
                newS)))
  
  ;; cfg --> cfg
  ;; Purpose: Eliminate rules with nonsolitary terminals
  (define (term-grammar G)
    (define ht (make-hash))
    (define nts (list->mutable-seteq (cfg-get-v G)))
    (define (populate-hash!)
      (define (generate-symbol symb)
        (let ([new-symb (if (set-member? nts symb)
                            (string->symbol (string-append (symbol->string (gensym (string->symbol (string-append (symbol->string symb) "-"))))))
                            symb)])
          (if (not (set-member? nts new-symb))
              new-symb
              (generate-symbol symb))))
      (for ([sigma-elem (in-list (cfg-get-alphabet G))]
            #:do [(define new-nt (generate-symbol (symbol-upcase sigma-elem)))])
        (hash-set! ht sigma-elem new-nt)
        (set-add! nts new-nt)))

    (define (convert-non-solitary rule ht)
      (list (first rule)
            ARROW
            (los->symbol (map (λ (s)
                                (if (set-member? alphabet-set s)
                                    (hash-ref ht s)
                                    s))
                              (symbol->fsmlos (third rule))))))
    (populate-hash!)
    (make-cfg (append (cfg-get-v G) (hash-values ht))
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
    (define (generate-symbol! symb)
      (let ([new-symb (if (set-member? nts symb)
                          (string->symbol (string-append (symbol->string (gensym (string->symbol (string-append (symbol->string symb) "-"))))))
                          symb)])
        (if (not (set-member? nts new-symb))
            (begin
              (set-add! nts new-symb)
              new-symb)
            (generate-symbol! symb))))
    
    (define (convert-rules rules)
      (define (convert-rule lhs rhs count)
        (if (= count 0)
            (list (list lhs ARROW (los->symbol rhs)))
            (let ([new-nt (generate-symbol! 'T)])
              (cons (list lhs ARROW (los->symbol (list (car rhs) new-nt)))
                    (convert-rule new-nt (cdr rhs)
                                  (sub1 count))))))
      (if (empty? rules)
          '()
          (let* ([rhs (symbol->fsmlos (caddr (car rules)))]
                 [rhs-len (length rhs)])
            (if (> rhs-len 2)
                (append (convert-rule (caar rules) rhs (- rhs-len 2))
                        (convert-rules (rest rules)))
                (cons (car rules)
                      (convert-rules (rest rules)))))))
    (let ([new-rules (convert-rules (cfg-get-rules G))])
      (make-cfg (set->list nts)
                (cfg-get-alphabet G)
                new-rules
                (cfg-get-start G))))
  
  ;; cfg --> cfg
  ;; Purpose: Remove e-rules from given grammar
  (define (del-grammar G)
    (define trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
    
    (define nulls (list->mutable-seteq (filter (lambda (r) (equal? (third r) (list EMP))) trls)))
    
    (define (new-compute-nullables trls)
      (define (compute-any-nullables rules)
        (if (empty? (for/list ([rule (in-list rules)]
                               #:when (and (not (set-member? nulls (car rule)))
                                           (andmap (lambda (elem) (set-member? nulls elem)) rule)))
                      (set-add! nulls (first rule))
                      (first rule)))
            (set->list nulls)
            (compute-any-nullables rules)))
      (compute-any-nullables trls))

    (define (remove-nts null-elem idx-lst count word)
      (if (empty? word)
          '()
          (if (eq? (first word) null-elem)
              (if (member count idx-lst)
                  (remove-nts null-elem idx-lst (add1 count) (rest word))
                  (cons (first word) (remove-nts null-elem idx-lst (add1 count) (rest word))))
              (cons (first word) (remove-nts null-elem idx-lst count (rest word))))))
    
    (define (convert-rules rules nulls-list)
      (for/fold ([new-rules rules])
                ([null-elem (in-list nulls-list)])
        (let-values ([(null-rules rest-rules)
                      (filter-save (lambda (rule) (ormap (lambda (elem) (eq? elem null-elem)) (third rule))) new-rules)])
          (for/fold ([new-rules rest-rules])
                    ([null-rule (in-list null-rules)])
            (append
             (for/list ([removed-elems (in-combinations (range (count (lambda (elem) (eq? elem null-elem)) (third null-rule))))]
                        #:do [(define new-rhs (remove-nts null-elem removed-elems 0 (third null-rule)))]
                        #:when (not (empty? new-rhs)))
               (list (first null-rule)
                     ARROW
                     new-rhs))
             new-rules)))))
    
    (let ([new-rules (map (λ (r)
                            (list (first r) ARROW (los->symbol (third r))))
                          (remove-duplicates (filter (λ (r)
                                                       (or (eq? (first r) (cfg-get-start G))
                                                           (and (not (eq? (first r) (cfg-get-start G)))
                                                                (not (equal? (third r) (list EMP))))))
                                                     (convert-rules trls (map first (new-compute-nullables trls))))))])
      (make-cfg (remove-duplicates (map first new-rules))
                (cfg-get-alphabet G)
                new-rules
                (cfg-get-start G))))

  ;; cfg --> cfg
  ;; Purpose: Remove unit rules
  (define (unit-grammar G)
    (define nts (list->mutable-seteq (cfg-get-v G)))
    (define (rm-unit-rules rls)
      (let-values ([(urules rest-rules)
                    (filter-save (λ (r) (and (= (length (third r)) 1)
                                             (set-member? nts (first (third r)))))
                                 rls)])
        (if (empty? urules)
            rest-rules
            (rm-unit-rules (for/fold ([new-rules rest-rules])
                                     ([urule (in-list urules)])
                             (append (for/list ([rule (in-list rls)]
                                                #:when (and (eq? (first (third urule)) (first rule))
                                                            (not (equal? (list (first (third urule))) (third rule)))))
                                       (list (first urule) ARROW (third rule)))
                                     new-rules))))))
    (let [(new-rules
           (remove-duplicates
            (map (λ (r)
                   (list (first r) ARROW (los->symbol (third r))))
                 (rm-unit-rules (map (lambda (r)
                                       (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                                     (cfg-get-rules G))))))]
      (make-cfg (remove-duplicates (map first new-rules))
                (cfg-get-alphabet G)
                new-rules
                (cfg-get-start G))))
  (unit-grammar (del-grammar (bin-grammar (term-grammar (start-grammar G))))))