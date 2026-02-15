#lang racket/base

(module+ test
  (require rackunit
           "../../../../../fsm-core/private/macros/rules/rules-predicates.rkt"
           "../../../../../fsm-core/private/constants.rkt")
  ;add-dead-state-rules tests
  (check-equal? (add-dead-state-rules `((A a B) (A b B)) '(A B) '(a b))
                `((A a B) (A b B) (B a ,DEAD) (B b ,DEAD)))
  (check-equal? (add-dead-state-rules `((A a B) (A b B)) '(A B C) '(a b))
                `((A a B) (A b B) (B a ,DEAD) (B b ,DEAD) (C a ,DEAD) (C b ,DEAD)))
    
  ;valid-rules? tests
  (check-equal? (valid-rules? (lambda (x) (symbol? x)) '(A B C D)) #t)
  (check-equal? (valid-rules? (lambda (x) (symbol? x)) `(A (B) C D)) #f)
    
  ;invalid-rules tests
  (check-equal? (invalid-rules (lambda (x) (symbol? x)) '(A B C D)) '())
  (check-equal? (invalid-rules (lambda (x) (symbol? x)) `(A (B) C D)) `((B)))
    
  ;valid-dfa-rule-structure? tests
  (check-equal? (valid-dfa-rule-structure? '(A a B)) #t)
  (check-equal? (valid-dfa-rule-structure? '(A a a B)) #f)
  (check-equal? (valid-dfa-rule-structure? '(a a a)) #t)
  (check-equal? (valid-dfa-rule-structure? `((A) b b)) #t)
    
  ;valid-ndpda-rule-structure? tests
  (check-equal? (valid-ndpda-rule-structure? `((A a (B)) (A (b)))) #t)
  (check-equal? (valid-ndpda-rule-structure? `((a a (a)) (1 (b)))) #t)
  (check-equal? (valid-ndpda-rule-structure? `((1 1 a) (A (b)))) #f)
  (check-equal? (valid-ndpda-rule-structure? `((1 1 (a)) (A ))) #f)
  (check-equal? (valid-ndpda-rule-structure? `((A a) (A b))) #f)
    
  ;valid-tm-rule-structure? tests
  (check-equal? (valid-tm-rule-structure? `((A a) (A b))) #t)
  (check-equal? (valid-tm-rule-structure? `(((A) a) (A 1))) #t)
  (check-equal? (valid-tm-rule-structure? `((A a (A b)))) #f)
  (check-equal? (valid-tm-rule-structure? `(a a a)) #f)

  ;valid-mttm-rule-structure? tests
  (check-equal? ((valid-mttm-rule-structure? 3) `((A (a b ,BLANK)) (B (,RIGHT ,BLANK b)))) #t)
  (check-equal? ((valid-mttm-rule-structure? 2) `(a b)) #f)
  (check-equal? ((valid-mttm-rule-structure? 1) `((A a) (B b))) #f)

  ;functional? tests
  (check-equal? (functional? `((A a B) (A b B)) '(A B) '(a b) '()) #t)
  (check-equal? (functional? `((A a B) (A b B)) '(A B) '(a b) 'no-dead) #f)
  (check-equal? (functional? `((A a B) (A b B) (B a B) (B b A)) '(A B) '(a b) 'no-dead) #t)


  ;missing-functional tests
  (check-equal? (missing-functional `((A a B) (A b B)) '(A B) '(a b)) `((B a) (B b)))
  (check-equal? (missing-functional `((A a B) (A b B) (B a B) (B b A)) '(A B) '(a b)) '())
    
  ;check-duplicates-dfa tests
  (check-equal? (check-duplicates-dfa `((A a B) (A b B) (B a B) (B b A))) #f)
  (check-equal? (check-duplicates-dfa `((A a B) (A b B) (B a B) (B b A) (B b B))) `((B b)))
    
  ;correct-members-dfa? tests
  (check-equal? (correct-members-dfa? '(A B) '(a b) `((A b B) (B a A))) #t)
  (check-equal? (correct-members-dfa? '(A B) '(a b) `()) #t)
  (check-equal? (correct-members-dfa? '(A B) '(a b) `((A b B) (B a A) (C b A))) #f)
    
  ;correct-members-ndpda? tests
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((A b (c)) (B (d))))
                                        ) #t)
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((C b (c)) (B (d))))
                                        ) #f)
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((A c (c)) (B (d))))
                                        ) #f)
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((A b (b)) (B (d))))
                                        ) #f)
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((A b (c)) (C (d))))
                                        ) #f)
  (check-equal? (correct-members-ndpda? '(A B)
                                        '(a b)
                                        '(c d)
                                        `(((A b (c)) (B (a))))
                                        ) #f)

  ;correct-members-tm? tests
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A b) (B b)))) #t)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A b) (B ,RIGHT)))) #t)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A b) (B ,LEFT)))) #t)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A b) (B ,BLANK)))) #t)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A c) (B ,BLANK)))) #f)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((C b) (B ,BLANK)))) #f)
  (check-equal? (correct-members-tm? '(A B)
                                     '(a b)
                                     `(((A c) (C ,BLANK)))) #f)

  ;correct-members-mttm? tests
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       '(((A (b b)) (B (a b))))) #t)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,RIGHT))))) #t)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,LEFT))))) #t)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,BLANK))))) #t)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,BLANK)))
                                         ((A (c)) (B (,BLANK))))) #f)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,BLANK)))
                                         ((C (b)) (B (,BLANK))))) #f)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,BLANK)))
                                         ((A (c)) (C (,BLANK))))) #f)
  (check-equal? (correct-members-mttm? '(A B)
                                       '(a b)
                                       `(((A (b)) (B (,BLANK)))
                                         ((A (c)) (B (d))))) #f)
    
  ;incorrect-members-dfa tests
  (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A))) '())
  (check-equal? (incorrect-members-dfa '(A B) '(a b) `()) '())
  (check-equal? (incorrect-members-dfa '(A B) '(a b) `((A b B) (B a A) (C b A))) '((C b A)))

  ;incorrect-dfa-rules tests
  (check-equal? (incorrect-dfa-rules '(A B) '(a b) `((A b B) (B a A))) '())
  (check-equal? (incorrect-dfa-rules '(A B) '(a b) `((C d E) (A f G)))
                (list
                 (make-invalid-rule '(C d E)
                                    '("The from state, C, is not in the given list of states."
                                      "The consumed letter, d, is not in the given input alphabet."
                                      "The to state, E, is not in the given list of states."))
                 (make-invalid-rule '(A f G)
                                    '("The consumed letter, f, is not in the given input alphabet."
                                      "The to state, G, is not in the given list of states."))))
    
  ;incorrect-members-ndpda tests
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d))))
                                         ) '())
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))
                                           ((C b (c)) (B (d))))
                                         ) `(((C b (c)) (B (d)))))
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))
                                           ((A c (c)) (B (d))))
                                         ) `(((A c (c)) (B (d)))))
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))
                                           ((A b (b)) (B (d))))
                                         ) `(((A b (b)) (B (d)))))
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))
                                           ((A b (c)) (C (d))))
                                         ) `(((A b (c)) (C (d)))))
  (check-equal? (incorrect-members-ndpda '(A B)
                                         '(a b)
                                         '(c d)
                                         `(((A b (c)) (B (d)))
                                           ((A b (c)) (B (a))))
                                         ) `(((A b (c)) (B (a)))))

  ;incorrect-ndpda-rules tests
  (check-equal? (incorrect-ndpda-rules '(A B)
                                       '(a b)
                                       '(c d)
                                       `(((A b (c)) (B (d)))))
                '())
  (check-equal? (incorrect-ndpda-rules '(A B)
                                       '(a b)
                                       '(c d)
                                       `(((D e (f)) (G (,EMP)))))
                (list (make-invalid-rule `((D e (f)) (G (,EMP)))
                                         (list
                                          "The from state, D, is not in the given list of states."
                                          "e is not in the given input alphabet."
                                          "The f at index 0 of the pop list is not in the given stack alphabet."
                                          "The to state, G, is not in the given list of states."
                                          "The ε at index 0 of the push list is not in the given stack alphabet."))))

  ;incorrect-tm-rules tests
  (check-equal? (incorrect-tm-rules '(A B C)
                                    '(a b)
                                    `(((A b) (B b))
                                      ((B ,LM) (A ,LEFT))
                                      ((C ,BLANK) (B ,RIGHT))))
                '())
  (check-equal? (incorrect-tm-rules '(A B C)
                                    '(a b)
                                    `(((D ,LEFT) (E ,EMP))))
                (list (make-invalid-rule `((D ,LEFT) (E ,EMP))
                                         '("The from state, D, is not in the given list of states."
                                           "The read symbol, L, must be in the given input alphabet, BLANK, or LM."
                                           "The to state, E, is not in the given list of states."
                                           "The action ε must be in the given input alphabet, LEFT, RIGHT, or BLANK."))))

  ;incorrect-mttm-rules tests
  (check-equal? (incorrect-mttm-rules '(A B C)
                                      '(a b)
                                      `(((A (,LM ,BLANK)) (B (a b)))
                                        ((B (a b)) (C (,LEFT ,RIGHT)))))
                '())
  (check-equal? (incorrect-mttm-rules '(A B C)
                                      '(a b)
                                      `(((D (,LEFT ,EMP)) (E (f g)))))
                (list (make-invalid-rule `((D (,LEFT ,EMP)) (E (f g)))
                                         '("The from state, D, is not in the given list of states."
                                           "The read symbol, L, on tape 0 must be in the given input alphabet, BLANK, or LM."
                                           "The read symbol, ε, on tape 1 must be in the given input alphabet, BLANK, or LM."
                                           "The to state, E, is not in the given list of states."
                                           "The action f on tape 0 must be in the given input alphabet, LEFT, RIGHT, or BLANK."
                                           "The action g on tape 1 must be in the given input alphabet, LEFT, RIGHT, or BLANK."))))
    
  ;incorrect-members-tm tests
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B b)))) '())
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,RIGHT)))) '())
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,LEFT)))) '())
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,BLANK)))) '())
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,BLANK))
                                        ((A c) (B ,BLANK)))) `(((A c) (B ,BLANK))))
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,BLANK))
                                        ((C b) (B ,BLANK)))) `(((C b) (B ,BLANK))))
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,BLANK))
                                        ((A c) (C ,BLANK)))) `(((A c) (C ,BLANK))))
  (check-equal? (incorrect-members-tm '(A B)
                                      '(a b)
                                      `(((A b) (B ,BLANK))
                                        ((A c) (B d)))) `(((A c) (B d))))

  ;incorrect-members-mttm tests
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        '(((A (b b)) (B (a b))))) '())
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,RIGHT))))) '())
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,LEFT))))) '())
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,BLANK))))) '())
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,BLANK)))
                                          ((A (c)) (B (,BLANK)))))
                `(((A (c)) (B (,BLANK)))))
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,BLANK)))
                                          ((C (b)) (B (,BLANK)))))
                `(((C (b)) (B (,BLANK)))))
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,BLANK)))
                                          ((A (c)) (C (,BLANK)))))
                `(((A (c)) (C (,BLANK)))))
  (check-equal? (incorrect-members-mttm '(A B)
                                        '(a b)
                                        `(((A (b)) (B (,BLANK)))
                                          ((A (c)) (B (d)))))
                `(((A (c)) (B (d)))))
  )