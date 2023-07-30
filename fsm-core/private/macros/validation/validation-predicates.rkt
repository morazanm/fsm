(module validation-predicates racket
  (require racket/contract
           "../../constants.rkt"
           "../../sm-getters.rkt"
           "../../fsa.rkt"
           "../../tm.rkt"
           "../../pda.rkt"
           "../../../../main.rkt")
  (provide listof-words?
           check-input-dfa
           return-input-dfa
           check-input-ndfa
           return-input-ndfa
           check-input-ndpda
           return-input-ndpda
           check-input-tm
           return-input-tm
           check-input-mttm
           return-input-mttm
           )

  ;listof-words?: (listof something) sigma --> boolean
  ;purpose: to check if the list of words is a valid list
  ; of list of symbols
  (define (listof-words? words sigma)
    (and (list? words)
         (andmap (lambda (word) (and (list? word)
                                     (andmap (lambda (letter) (symbol? letter))
                                             word))) words))
    )

  ;check-input-dfa:
  ; (listof states) (listof alphabet) symbol (listof states) (listof dfa-rules) boolean symbol
  ; --> [(listof (listof symbols)) --> boolean]
  ;purpose: to take in all the components of a machine, and then a list
  ; of words to check in that machine, then run those words through the machine
  ; and make sure that they match the "accepts?" symbol, which will be either
  ; accept or reject. This ensures that this can be used for both accept lists
  ; and rejects lists.
  (define (check-input-dfa states
                           sigma
                           start
                           finals
                           rules
                           add-dead
                           accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-dfa states
                                               sigma
                                               start
                                               finals
                                               rules
                                               add-dead))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  ;return-input-dfa:
  ; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
  ; --> (listof (listof symbols))
  ;purpose: builds the machine and finds all the words from the input
  ; list of words that don't accept/reject (based on accepts?)
  (define (return-input-dfa states
                            sigma
                            start
                            finals
                            rules
                            add-dead
                            words
                            accepts?)
    (define temp-machine (make-unchecked-dfa states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  ;check-input-ndfa
  ; (listof states) (listof alphabet) symbol (listof states) (listof ndfa-rules) boolean symbol
  ; --> [(listof (listof symbols)) --> boolean]
  ;purpose: to take in all the components of a machine, and then a list
  ; of words to check in that machine, then run those words through the machine
  ; and make sure that they match the "accepts?" symbol, which will be either
  ; accept or reject. This ensures that this can be used for both accept lists
  ; and rejects lists.
  (define (check-input-ndfa states
                            sigma
                            start
                            finals
                            rules
                            accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-ndfa states
                                                sigma
                                                start
                                                finals
                                                rules))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  ;return-input-nfa:
  ; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
  ; --> (listof (listof symbols))
  ;purpose: builds the machine and finds all the words from the input
  ; list of words that don't accept/reject (based on accepts?)
  (define (return-input-ndfa states
                             sigma
                             start
                             finals
                             rules
                             words
                             accepts?)
    (define temp-machine (make-unchecked-ndfa states
                                              sigma
                                              start
                                              finals
                                              rules))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  ;check-input-ndpda
  ; (listof states) sigma gamma symbol (listof states) (listof ndpda-rules) boolean symbol
  ; --> [(listof (listof symbols)) --> boolean]
  ;purpose: to take in all the components of a machine, and then a list
  ; of words to check in that machine, then run those words through the machine
  ; and make sure that they match the "accepts?" symbol, which will be either
  ; accept or reject. This ensures that this can be used for both accept lists
  ; and rejects lists.
  (define (check-input-ndpda states
                             sigma
                             gamma
                             start
                             finals
                             rules
                             accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-ndpda states
                                                 sigma
                                                 gamma
                                                 start
                                                 finals
                                                 rules))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  ;return-input-ndpda:
  ; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
  ; --> (listof (listof symbols))
  ;purpose: builds the machine and finds all the words from the input
  ; list of words that don't accept/reject (based on accepts?)
  (define (return-input-ndpda states
                              sigma
                              gamma
                              start
                              finals
                              rules
                              words
                              accepts?)
    (define temp-machine (make-unchecked-ndpda states
                                               sigma
                                               gamma
                                               start
                                               finals
                                               rules))
    (filter (lambda (x) (equal? (temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )


  ;check-input-tm
  ; (listof states) (listof alphabet) symbol (listof states) (listof tm-rules) boolean symbol
  ; --> [(listof (listof symbols)) --> boolean]
  ;purpose: to take in all the components of a machine, and then a list
  ; of words to check in that machine, then run those words through the machine
  ; and make sure that they match the "accepts?" symbol, which will be either
  ; accept or reject. This ensures that this can be used for both accept lists
  ; and rejects lists.
  (define (check-input-tm states
                          sigma
                          start
                          finals
                          rules
                          accept
                          accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-tm states
                                              sigma
                                              rules
                                              start
                                              finals
                                              accept))
      (andmap (lambda (x) (equal? (sm-apply temp-machine x) accepts?)) words)
      )
    )

  ;return-input-tm:
  ; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
  ; --> (listof (listof symbols))
  ;purpose: builds the machine and finds all the words from the input
  ; list of words that don't accept/reject (based on accepts?)
  (define (return-input-tm states
                           sigma
                           start
                           finals
                           rules
                           words
                           accept
                           accepts?)
    (define temp-machine (make-unchecked-tm states
                                            sigma
                                            rules
                                            start
                                            finals
                                            accept))
    (filter (lambda (x) (equal? (sm-apply temp-machine x) (if (equal? 'accept accepts?) 'reject 'accept))) words)
    )

  ;check-input-mttm:
  ; (listof states) (listof alpha) state (listof states) (listof mttm-rule) (listof word) boolean symbol
  ; --> (listof (listof symbol))
  ;purpose: to take in all the components of a multi-tape tm, and then a list
  ; of words to check in that machine, then run those words through the machine
  ; and make sure that they match the "accepts?" symbol, which will be either
  ; accept or reject. This ensures that this can be used for both accept lists
  ; and rejects lists.
  (define (check-input-mttm states sigma start finals rules num-tapes words accept accepts?)
    (define temp-machine (make-mttm states sigma start finals rules num-tapes accept))
    (andmap (lambda (word) (equal? (sm-apply temp-machine word) accepts?)) words)
    )

  ;return-input-mttm:
  ; (listof states) (listof alpha) state (listof states) (listof mttm-rule) (listof word) boolean symbol
  ; --> (listof (listof symbol))
  ;purpose: builds the multi-tape tm and finds all the words from the input
  ; list of words that don't accept/reject (based on accepts?)
  (define (return-input-mttm states
                             sigma
                             start
                             finals
                             rules
                             num-tapes
                             words
                             accept
                             accepts?)
    (define temp-machine (make-mttm states sigma start finals rules num-tapes accept))
    (filter (lambda (word) (equal? (sm-apply temp-machine word) (if (equal? 'accept accepts?) 'reject 'accept))) words))

  (module+ test

    ;listof-words? tests
    (check-equal? (listof-words? '((a a a a)) '(a a a)) #t)
    (check-equal? (listof-words? '(a a a a) '(b a c)) #f)
    (check-equal? (listof-words? '((a 1 2 e r)) '(a b c)) #f)

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
                                    'accept))
    (define all-bs (check-input-dfa '(A B)
                                    '(a b)
                                    'B
                                    '(A)
                                    `((B a A)
                                      (B b B)
                                      (A a A)
                                      (A b B))
                                    #f
                                    'reject))
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
                                    'accept) '())
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
                                    'accept) `((b b b b)))
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
                                    'reject) `((a a a)))
    
    ;check-input-ndfa tests
    (define allas (check-input-ndfa '(A B)
                                    '(a b)
                                    'B
                                    '(A)
                                    `((B a A)
                                      (B b B)
                                      (A a A)
                                      (A b B))
                                    'accept))
    (define allbs (check-input-ndfa '(A B)
                                    '(a b)
                                    'B
                                    '(A)
                                    `((B a A)
                                      (B b B)
                                      (A a A)
                                      (A b B))
                                    'reject))
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
                                     'accept) '())
    (check-equal? (return-input-ndfa '(A B)
                                     '(a b)
                                     'B
                                     '(A)
                                     `((B a A)
                                       (B b B)
                                       (A a A)
                                       (A b B))
                                     word-list-b
                                     'accept) `((b b b b)))
    (check-equal? (return-input-ndfa '(A B)
                                     '(a b)
                                     'B
                                     '(A)
                                     `((B a A)
                                       (B b B)
                                       (A a A)
                                       (A b B))
                                     word-list-b
                                     'reject) `((a a a)))
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
                                     'accept))
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
                                       'reject))
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
                                      'accept) '())
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
                                      'reject) '((c)))
    ;check-input-tm tests
    (define tm-accept (check-input-tm '(S Y N)
                                      `(a b)
                                      'S
                                      '(Y N)
                                      `(((S a) (S ,RIGHT))
                                        ((S b) (N b))
                                        ((S ,BLANK) (Y ,BLANK)))
                                      'Y
                                      'accept))
    (define tm-reject (check-input-tm '(S Y N)
                                      `(a b)
                                      'S
                                      '(Y N)
                                      `(((S a) (S ,RIGHT))
                                        ((S b) (N b))
                                        ((S ,BLANK) (Y ,BLANK)))
                                      'Y
                                      'reject))
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
                                   'accept) '())
    (check-equal? (return-input-tm '(S Y N)
                                   `(a b)
                                   'S
                                   '(Y N)
                                   `(((S a) (S ,RIGHT))
                                     ((S b) (N b))
                                     ((S ,BLANK) (Y ,BLANK)))
                                   `((b b b b) (a a a))
                                   'Y
                                   'accept) '((b b b b)))
    (check-equal? (return-input-tm '(S Y N)
                                   `(a b)
                                   'S
                                   '(Y N)
                                   `(((S a) (S ,RIGHT))
                                     ((S b) (N b))
                                     ((S ,BLANK) (Y ,BLANK)))
                                   `((b b b b) (a a a))
                                   'Y
                                   'reject) '((a a a)))
    )
  )