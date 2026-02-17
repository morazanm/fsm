#lang racket/base

(module+ test
  (require rackunit
           "../../../../../fsm-core/private/macros/validation/validation-predicates.rkt"
           "../../../../../fsm-core/private/constants.rkt"
           )
  ;listof-words? tests
  (check-equal? (listof-words? '((a a a a))) #t)
  (check-equal? (listof-words? '(a a a a)) #f)
  (check-equal? (listof-words? '((a 1 2 e r))) #t)

  ;listof-words-tm? tests
  (check-equal? (listof-words-tm? '((a a a a) (b b b))) #t)
  (check-equal? (listof-words-tm? '((a a 2 a) (a h 2))) #f)
  (check-equal? (listof-words-tm? `(((a a a a) 0)
                                    (b b b b))) #t)
  (check-equal? (listof-words-tm? `(((a a a a) a)
                                    (b b b b))) #f)

  ;words-in-sigma? tests
  (check-equal? (words-in-sigma? `((a a a a) (b b b)) '(a b)) #t)
  (check-equal? (words-in-sigma? `((a a a a) (b b b)) '(a)) #f)

  ;invalid-words tests
  (check-equal? (invalid-words `((a a a a) (b b b)) '(a b)) '())
  (check-equal? (invalid-words `((a a a a) (b b b)) '(a)) '((b b b)))

  ;words-in-sigma-tm? tests
  (check-equal? (words-in-sigma-tm? `((a a a a)
                                      (b b b b)) '(a)) #f)
  (check-equal? (words-in-sigma-tm? `((a a a a)
                                      (b b b b)) '(a b)) #t)
  (check-equal? (words-in-sigma-tm? `(((a a a a) 0)
                                      (b b b b)) '(a b)) #t)

  ;invalid-words-tm tests
  (check-equal? (invalid-words-tm `(((a a a a) 1)
                                    (a a a)) '(a)) '())
  (check-equal? (invalid-words-tm `((a a a b)
                                    (b b b b)) '(a b)) '())
  (check-equal? (invalid-words-tm `((a a a b)
                                    (b b b b)) '(a)) `((a a a b)
                                                       (b b b b)))

  ;acceptable-position? tests
  (check-equal? (acceptable-position? `((a a a a) (a a a) (a a))) #t)
  (check-equal? (acceptable-position? `(((a a a a) 1) (a a a) (a a))) #t)
  (check-equal? (acceptable-position? `(((a a a a) -1) (a a a) (a a))) #f)
  (check-equal? (acceptable-position? `(((a a a a) 4) (a a a) (a a))) #f)
  (check-equal? (acceptable-position? `(((a a a a) 5) (a a a) (a a))) #f)

  ;unacceptable-position tests
  (check-equal? (unacceptable-position `((a a a a) (a a a) (a a))) '())
  (check-equal? (unacceptable-position `(((a a a a) 1) (a a a) (a a))) '())
  (check-equal? (unacceptable-position `(((a a a a) -1) (a a a) (a a))) `(((a a a a) -1)))
  (check-equal? (unacceptable-position `(((a a a a) 4) (a a a) (a a))) `(((a a a a) 4)))
  (check-equal? (unacceptable-position `(((a a a a) 5) (a a a) (a a))) `(((a a a a) 5)))



  ;check-input-dfa tests
  (define word-list-a `((a a a a) (a a a)))
  (define word-list-b `((b b b b) (a a a)))
  (define word-list-c `((b b b b) (b a b)))
  (define all-as (check-input-dfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f
                                  #t))
  (define all-bs (check-input-dfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f
                                  #f))
  (check-equal? (all-as word-list-a) #t)
  (check-equal? (all-as word-list-a) #t)
  (check-equal? (all-as word-list-b) #f)
  (check-equal? (all-bs word-list-c) #t)
  (check-equal? (all-bs word-list-b) #f)
    
  ;return-input-dfa tests
  (check-equal? (return-input-dfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f
                                  word-list-a
                                  #t) '())
  (check-equal? (return-input-dfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f
                                  word-list-b
                                  #t) `((b b b b)))
  (check-equal? (return-input-dfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f
                                  word-list-b
                                  #f) `((a a a)))
    
  ;check-input-ndfa tests
  (define allas (check-input-ndfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #t))
  (define allbs (check-input-ndfa '(A B)
                                  '(a b)
                                  'B
                                  '(A)
                                  `((B a A)
                                    (B b B)
                                    (A a A)
                                    (A b B))
                                  #f))
  (check-equal? (allas word-list-a) #t)
  (check-equal? (allas word-list-a) #t)
  (check-equal? (allas word-list-b) #f)
  (check-equal? (allbs word-list-c) #t)
  (check-equal? (allbs word-list-b) #f)
    
  ;return-input-ndfa tests
  (check-equal? (return-input-ndfa '(A B)
                                   '(a b)
                                   'B
                                   '(A)
                                   `((B a A)
                                     (B b B)
                                     (A a A)
                                     (A b B))
                                   word-list-a
                                   #t) '())
  (check-equal? (return-input-ndfa '(A B)
                                   '(a b)
                                   'B
                                   '(A)
                                   `((B a A)
                                     (B b B)
                                     (A a A)
                                     (A b B))
                                   word-list-b
                                   #t) `((b b b b)))
  (check-equal? (return-input-ndfa '(A B)
                                   '(a b)
                                   'B
                                   '(A)
                                   `((B a A)
                                     (B b B)
                                     (A a A)
                                     (A b B))
                                   word-list-b
                                   #f) `((a a a)))
  ;check-input-ndpda tests
  (define wcw^r (check-input-ndpda '(S P Q F)
                                   '(a b c)
                                   '(a b)
                                   'S
                                   '(F)
                                   `(((S ,EMP ,EMP) (P ,EMP))
                                     ((P a ,EMP) (P (a)))
                                     ((P b ,EMP) (P (b)))
                                     ((P c ,EMP) (Q ,EMP))
                                     ((Q a (a)) (Q ,EMP))
                                     ((Q b (b)) (Q ,EMP))
                                     ((Q ,EMP ,EMP) (F ,EMP)))
                                   #t))
  (define wcw^r-r (check-input-ndpda '(S P Q F)
                                     '(a b c)
                                     '(a b)
                                     'S
                                     '(F)
                                     `(((S ,EMP ,EMP) (P ,EMP))
                                       ((P a ,EMP) (P (a)))
                                       ((P b ,EMP) (P (b)))
                                       ((P c ,EMP) (Q ,EMP))
                                       ((Q a (a)) (Q ,EMP))
                                       ((Q b (b)) (Q ,EMP))
                                       ((Q ,EMP ,EMP) (F ,EMP)))
                                     #f))
  (check-equal? (wcw^r `((c))) #t)
  (check-equal? (wcw^r-r `((a a a))) #t)
  (check-equal? (wcw^r-r `((c))) #f)
    
  ;return-input-ndpda tests
  (check-equal? (return-input-ndpda '(S P Q F)
                                    '(a b c)
                                    '(a b)
                                    'S
                                    '(F)
                                    `(((S ,EMP ,EMP) (P ,EMP))
                                      ((P a ,EMP) (P (a)))
                                      ((P b ,EMP) (P (b)))
                                      ((P c ,EMP) (Q ,EMP))
                                      ((Q a (a)) (Q ,EMP))
                                      ((Q b (b)) (Q ,EMP))
                                      ((Q ,EMP ,EMP) (F ,EMP)))
                                    `((c))
                                    #t) '())
  (check-equal? (return-input-ndpda '(S P Q F)
                                    '(a b c)
                                    '(a b)
                                    'S
                                    '(F)
                                    `(((S ,EMP ,EMP) (P ,EMP))
                                      ((P a ,EMP) (P (a)))
                                      ((P b ,EMP) (P (b)))
                                      ((P c ,EMP) (Q ,EMP))
                                      ((Q a (a)) (Q ,EMP))
                                      ((Q b (b)) (Q ,EMP))
                                      ((Q ,EMP ,EMP) (F ,EMP)))
                                    `((c))
                                    #f) '((c)))
  ;check-input-tm tests
  (define tm-accept (check-input-tm '(S Y N)
                                    `(a b)
                                    'S
                                    '(Y N)
                                    `(((S a) (S ,RIGHT))
                                      ((S b) (N b))
                                      ((S ,BLANK) (Y ,BLANK)))
                                    'Y
                                    #t))
  (define tm-reject (check-input-tm '(S Y N)
                                    `(a b)
                                    'S
                                    '(Y N)
                                    `(((S a) (S ,RIGHT))
                                      ((S b) (N b))
                                      ((S ,BLANK) (Y ,BLANK)))
                                    'Y
                                    #f))
  (check-equal? (tm-accept `((a a a a) (a a a))) #t)
  (check-equal? (tm-accept `((a a a a) (b b b))) #f)
  (check-equal? (tm-reject `((a a a a) (a a a))) #f)
  (check-equal? (tm-reject `((b b b b) (b b b))) #t)

  ;return-input-tm tests
  (check-equal? (return-input-tm '(S Y N)
                                 `(a b)
                                 'S
                                 '(Y N)
                                 `(((S a) (S ,RIGHT))
                                   ((S b) (N b))
                                   ((S ,BLANK) (Y ,BLANK)))
                                 `((a a a a) (a a a))
                                 'Y
                                 #t) '())
  (check-equal? (return-input-tm '(S Y N)
                                 `(a b)
                                 'S
                                 '(Y N)
                                 `(((S a) (S ,RIGHT))
                                   ((S b) (N b))
                                   ((S ,BLANK) (Y ,BLANK)))
                                 `((b b b b) (a a a))
                                 'Y
                                 #t) '((b b b b)))
  (check-equal? (return-input-tm '(S Y N)
                                 `(a b)
                                 'S
                                 '(Y N)
                                 `(((S a) (S ,RIGHT))
                                   ((S b) (N b))
                                   ((S ,BLANK) (Y ,BLANK)))
                                 `((b b b b) (a a a))
                                 'Y
                                 #f) '((a a a)))

  ;check-input-mttm tests
  (define mttm-accept (check-input-mttm '(S Y N)
                                        `(a b)
                                        'S
                                        '(Y N)
                                        `(((S (a)) (S (,RIGHT)))
                                          ((S (b)) (N (b)))
                                          ((S (,BLANK)) (Y (,BLANK))))
                                        1
                                        'Y
                                        #t))
  (define mttm-reject (check-input-mttm '(S Y N)
                                        `(a b)
                                        'S
                                        '(Y N)
                                        `(((S (a)) (S (,RIGHT)))
                                          ((S (b)) (N (b)))
                                          ((S (,BLANK)) (Y (,BLANK))))
                                        1
                                        'Y
                                        #f))
  (check-equal? (mttm-accept `((a a a a) (a a a))) #t)
  (check-equal? (mttm-accept `((a a a a) (b b b))) #f)
  (check-equal? (mttm-reject `((a a a a) (a a a))) #f) 
  (check-equal? (mttm-reject `((b b b b) (b b b))) #t)

  ;return-input-mttm tests
  (check-equal? (return-input-mttm '(S Y N)
                                   `(a b)
                                   'S
                                   '(Y N)
                                   `(((S (a)) (S (,RIGHT)))
                                     ((S (b)) (N (b)))
                                     ((S (,BLANK)) (Y (,BLANK))))
                                   1
                                   `((a a a a) (a a a))
                                   'Y
                                   #t) '())
  (check-equal? (return-input-mttm '(S Y N)
                                   `(a b)
                                   'S
                                   '(Y N)
                                   `(((S (a)) (S (,RIGHT)))
                                     ((S (b)) (N (b)))
                                     ((S (,BLANK)) (Y (,BLANK))))
                                   1
                                   `((b b b b) (a a a))
                                   'Y
                                   #t) '((b b b b)))
  (check-equal? (return-input-mttm '(S Y N)
                                   `(a b)
                                   'S
                                   '(Y N)
                                   `(((S (a)) (S (,RIGHT)))
                                     ((S (b)) (N (b)))
                                     ((S (,BLANK)) (Y (,BLANK))))
                                   1
                                   `((b b b b) (a a a))
                                   'Y
                                   #f) '((a a a)))
  )