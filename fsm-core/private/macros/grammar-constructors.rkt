#lang racket/base
(require "rules/rules-flat-contracts.rkt"
         "shared/shared-flat-contracts.rkt"
         "validation/validation-flat-contracts.rkt"
         "../constants.rkt"
         "shared/shared-predicates.rkt"
         "../grammar-getters.rkt"
         racket/contract)
(provide make-cfg/c
         make-rg/c
         make-csg/c)


(define (valid-alpha? x)
  (define regex-pattern (regexp "^[a-z0-9$&!*]$"))
  (and (or (symbol? x) (and (number? x) (<= 0 x 9)))
       (not (not (regexp-match regex-pattern
                               (if (symbol? x)
                                   (symbol->string x)
                                   (number->string x)))))))



(define make-rg/c
  (->i ([states (and/c (is-a-list/c "nonterminals" "two")
                       (valid-listof/c valid-state? "nonterminal" "list of nonterminals" #:rule "two")
                       (no-duplicates/c "nonterminals" "two"))]
        [sigma (and/c (is-a-list/c "grammar alphabet" "one")
                      (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [delta (states
                sigma
                start) (and/c (is-a-list/c "grammar nonterminals" "three")
                              correct-grammar-rule-structures/c
                              (no-emp-rhs/c start)
                              (correct-rg-rules/c states (cons EMP sigma))
                              (no-duplicates/c "rules" "three")
                              )]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        )
       (#:accepts [accepts (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "accepts" "two")
                                          (words-in-sigma/c sigma 'accepts "two")
                                          (rg-input/c states
                                                      sigma
                                                      delta
                                                      start
                                                      #t))]
        #:rejects [rejects (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "rejects" "two")
                                          (words-in-sigma/c sigma 'rejects "two")
                                          (rg-input/c states
                                                      sigma
                                                      delta
                                                      start
                                                      #f))]
        )


         
       [result (lambda (x) (equal? 'rg (grammar-type x)))]))

(define make-cfg/c
  (->i ([states (and/c (is-a-list/c "nonterminals" "two")
                       (valid-listof/c valid-state? "nonterminal" "list of nonterminals" #:rule "two")
                       (no-duplicates/c "nonterminals" "two")
                       )]
        [sigma (and/c (is-a-list/c "grammar alphabet" "one")
                      (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [delta (states
                sigma) (and/c (is-a-list/c "grammar nonterminals" "three")
                              correct-grammar-rule-structures/c
                              (correct-cfg-rules/c states (cons EMP sigma))
                              (no-duplicates/c "rules" "three"))]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        )
       (#:accepts [accepts (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "accepts" "two")
                                          (words-in-sigma/c sigma 'accepts "two")
                                          (cfg-input/c states
                                                       sigma
                                                       delta
                                                       start
                                                       #t))]
        #:rejects [rejects (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "rejects" "two")
                                          (words-in-sigma/c sigma 'rejects "two")
                                          (cfg-input/c states
                                                       sigma
                                                       delta
                                                       start
                                                       #f))]
        )


         
       [result (lambda (x) (equal? 'cfg (grammar-type x)))]))

(define make-csg/c
  (->i ([states (and/c (is-a-list/c "nonterminals" "two")
                       (valid-listof/c valid-state? "nonterminal" "list of nonterminals" #:rule "two")
                       (no-duplicates/c "nonterminals" "two")
                       )]
        [sigma (and/c (is-a-list/c "grammar alphabet" "one")
                      (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one")
                      (no-duplicates/c "sigma" "one"))]
        [delta (states
                sigma) (and/c (is-a-list/c "grammar nonterminals" "three")
                              correct-grammar-rule-structures/c
                              (correct-csg-rules/c states (cons EMP sigma))
                              (no-duplicates/c "rules" "three")
                              )]
        [start (states) (and/c (valid-start/c states)
                               (start-in-states/c states))]
        )
       (#:accepts [accepts (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "accepts" "two")
                                          (words-in-sigma/c sigma 'accepts "two")
                                          (csg-input/c states
                                                       sigma
                                                       delta
                                                       start
                                                       #t))]
        #:rejects [rejects (states
                            sigma
                            delta
                            start) (and/c (listof-words/c "rejects" "two")
                                          (words-in-sigma/c sigma 'rejects "two")
                                          (csg-input/c states
                                                       sigma
                                                       delta
                                                       start
                                                       #f))]
        )


         
       [result (lambda (x) (equal? 'csg (grammar-type x)))]))