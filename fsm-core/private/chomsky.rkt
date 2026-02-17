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
    (let ((newS (gen-nt (cfg-get-v G))))
      (make-unchecked-cfg (cons newS (cfg-get-v G))
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
      (if (null? rules)
          '()
          (let* ([rhs (symbol->fsmlos (caddr (car rules)))]
                 [rhs-len (length rhs)])
            (if (> rhs-len 2)
                (append (convert-rule (caar rules) rhs (- rhs-len 2))
                        (convert-rules (cdr rules)))
                (cons (car rules)
                      (convert-rules (cdr rules)))))))
    (let ([new-rules (convert-rules (cfg-get-rules G))])
      (make-unchecked-cfg (set->list nts)
                          (cfg-get-alphabet G)
                          new-rules
                          (cfg-get-start G))))
  
  ;; cfg --> cfg
  ;; Purpose: Remove e-rules from given grammar
  (define (del-grammar G)
    (define trls (map (lambda (r)
                        (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                      (cfg-get-rules G)))
    
    (define nulls (list->mutable-seteq (filter (lambda (r) (equal? (caddr r) (list EMP))) trls)))
    
    (define (new-compute-nullables trls)
      (define (compute-any-nullables rules)
        (if (null? (for/list ([rule (in-list rules)]
                              #:when (and (not (set-member? nulls (car rule)))
                                          (andmap (lambda (elem) (set-member? nulls elem)) rule)))
                     (set-add! nulls (car rule))
                     (car rule)))
            (set->list nulls)
            (compute-any-nullables rules)))
      (compute-any-nullables trls))

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
        (let-values ([(null-rules rest-rules)
                      (filter-save (lambda (rule) (ormap (lambda (elem) (eq? elem null-elem)) (caddr rule))) new-rules)])
          (for/fold ([new-rules rest-rules])
                    ([null-rule (in-list null-rules)])
            (append
             (for/list ([removed-elems (in-combinations (range (count (lambda (elem) (eq? elem null-elem)) (caddr null-rule))))]
                        #:do [(define new-rhs (remove-nts null-elem removed-elems 0 (caddr null-rule)))]
                        #:when (not (null? new-rhs)))
               (list (car null-rule)
                     ARROW
                     new-rhs))
             new-rules)))))
    
    (let ([new-rules (map (λ (r)
                            (list (car r) ARROW (los->symbol (caddr r))))
                          (remove-duplicates (filter (λ (r)
                                                       (or (eq? (car r) (cfg-get-start G))
                                                           (and (not (eq? (car r) (cfg-get-start G)))
                                                                (not (equal? (caddr r) (list EMP))))))
                                                     (convert-rules trls (map car (new-compute-nullables trls))))))])
      (make-unchecked-cfg (remove-duplicates (map car new-rules))
                          (cfg-get-alphabet G)
                          new-rules
                          (cfg-get-start G))))

  ;; cfg --> cfg
  ;; Purpose: Remove unit rules
  (define (unit-grammar G)
    (define nts (list->mutable-seteq (cfg-get-v G)))
    (define (rm-unit-rules rls)
      (let-values ([(urules rest-rules)
                    (filter-save (λ (r) (and (= (length (caddr r)) 1)
                                             (set-member? nts (car (caddr r)))))
                                 rls)])
        (if (null? urules)
            rest-rules
            (rm-unit-rules (for/fold ([new-rules rest-rules])
                                     ([urule (in-list urules)])
                             (append (for/list ([rule (in-list rls)]
                                                #:when (and (eq? (car (caddr urule)) (car rule))
                                                            (not (equal? (list (car (caddr urule))) (caddr rule)))))
                                       (list (car urule) ARROW (caddr rule)))
                                     new-rules))))))
    (let [(new-rules
           (remove-duplicates
            (map (λ (r)
                   (list (car r) ARROW (los->symbol (caddr r))))
                 (rm-unit-rules (map (lambda (r)
                                       (list (car r) (cadr r) (symbol->fsmlos (caddr r))))
                                     (cfg-get-rules G))))))]
      (make-unchecked-cfg (remove-duplicates (map car new-rules))
                          (cfg-get-alphabet G)
                          new-rules
                          (cfg-get-start G))))
  (unit-grammar (del-grammar (bin-grammar (term-grammar (start-grammar G))))))