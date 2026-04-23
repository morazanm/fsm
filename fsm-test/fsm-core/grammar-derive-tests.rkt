#lang racket/base

(module+ test
  (require rackunit
           "../../fsm-core/private/grammar-operations.rkt"
           "../../fsm-core/private/rg-constructors.rkt"
           "../../fsm-core/private/cfg-constructors.rkt"
           "../../fsm-core/private/urg-constructors.rkt"
           "../../fsm-core/private/grammar-getters.rkt"
           "../../fsm-core/private/constants.rkt"
           )
(define rg-a* (make-rg '(S) '(a) `((S -> ,EMP) (S -> aS)) 'S))

  (check-equal? (grammar-derive? rg-a* '()) #t)
  (check-equal? (grammar-derive? rg-a* '(a)) #t)
  (check-equal? (grammar-derive? rg-a* '(a a a a a a)) #t)

  (define rg-renaments-a* ( grammar-rename-nts '(S) rg-a* ))

  (check-equal? (grammar-derive? rg-renaments-a* '()) #t)
  (check-equal? (grammar-derive? rg-renaments-a* '(a)) #t)
  (check-equal? (grammar-derive? rg-renaments-a* '(a a a a a a)) #t)

  (define rg-STARTS-WITH-aa (make-rg '(S A B) 
                                     '(a b) 
                                     '((S -> aA)
                                       (A -> a)
                                       (A -> aB)
                                       (B -> a)
                                       (B -> aB)
                                       (B -> b)
                                       (B -> bB))
                                     'S))
  (check-equal? (grammar-derive? rg-STARTS-WITH-aa '(a a b a b)) #t)
  (check-equal? (grammar-derive? rg-STARTS-WITH-aa '(b a a b a b)) #f)
  (check-equal? (grammar-derive? rg-STARTS-WITH-aa '(a b a b)) #f)
  (check-equal? (grammar-derive? rg-STARTS-WITH-aa '()) #f)

  (define rg-renaments-STARTS-WITH-aa ( grammar-rename-nts  '(A B C) rg-STARTS-WITH-aa))

  (check-equal? (grammar-derive? rg-renaments-STARTS-WITH-aa '(a a b a b)) #t)
  (check-equal? (grammar-derive? rg-renaments-STARTS-WITH-aa '(b a a b a b)) #f)
  (check-equal? (grammar-derive? rg-renaments-STARTS-WITH-aa '(a b a b)) #f)
  (check-equal? (grammar-derive? rg-renaments-STARTS-WITH-aa '()) #f)


  ;;; cfg tests

  (define cfg-moreAs-than-Bs (make-cfg '(S T)
                                       '(a b)
                                       `((S -> TaT)
                                         (T -> aTb)
                                         (T -> bTa)
                                         (T -> TT)
                                         (T -> a)
                                         (T -> ,EMP))
                                       'S))

  (check-equal? (grammar-derive? cfg-moreAs-than-Bs '(a b b a a))
                #t)
  (check-equal? (grammar-derive? cfg-moreAs-than-Bs '(a b b b a a a a))
                #t)
  (check-equal? (grammar-derive? cfg-moreAs-than-Bs '(a b b a)) 
                #f) ;!!!!Runs forever? 
  (check-equal? (grammar-derive? cfg-moreAs-than-Bs '(a b b b)) 
                #f) ;!!!!! Runs forever?

  (define numb>numa (make-cfg '(S A)
                              '(a b)
                              `((S ,ARROW b)
                                (S ,ARROW AbA)
                                (A ,ARROW AaAbA)
                                (A ,ARROW AbAaA)
                                (A ,ARROW ,EMP)
                                (A ,ARROW bA))
                              'S))

  (check-equal? (grammar-derive? numb>numa '())  
                #f)

  (check-equal? (grammar-derive? numb>numa '(a b b a b))
                #t)

  (check-equal? (grammar-derive? numb>numa '(a b b a a b b a b b b b b a a a))
                #t)

  (check-equal? (grammar-derive? numb>numa '(a b b a a b b b b b b))
                #t)

  (check-equal? (grammar-derive? numb>numa '(a b b a a b b a b b b))
                #t)
   
  (define cfg-1B-before-anA (make-cfg '(S A)
                                      '(a b)
                                      `((S -> AbaA)
                                        (A -> aA)
                                        (A -> bA)
                                        (A -> ,EMP))
                                      'S))

  (check-equal? (grammar-derive? cfg-1B-before-anA '(a b a b)) #t)
  (check-equal? (grammar-derive? cfg-1B-before-anA '(b b b)) #f)
  (check-equal? (grammar-derive? cfg-1B-before-anA '(b b b a)) #t)
  (check-equal? (grammar-derive? cfg-1B-before-anA '()) #f)

  ;{w| w contains at least three a's}
  (define cfg-test1 (make-cfg '(S X)
                              '(a b)
                              `((S -> XaXaXaX)
                                (X -> bX)
                                (X -> aX)
                                (X -> ,EMP))
                              'S))
  (check-equal? (grammar-derive? cfg-test1 '()) #f)
  (check-equal? (grammar-derive? cfg-test1 '(b b b a b a b)) #f)
  (check-equal? (grammar-derive? cfg-test1 '(a a a)) #t)
  (check-equal? (grammar-derive? cfg-test1 '(b b b a b a b a)) #t)

  ;{w| w starts and ends with the same symbol}
  (define cfg-test2 (make-cfg '(S X)
                              '(a b)
                              `((S -> aXa)
                                (S -> bXb)
                                (X -> aX)
                                (X -> bX)
                                (X -> ,EMP))
                              'S))

  (check-equal? (grammar-derive? cfg-test2 '(b b b a b a b a b)) #t)
  (check-equal? (grammar-derive? cfg-test2 '(a b b b a b a b a)) #t) ;!! dont understand this error
  (check-equal? (grammar-derive? cfg-test2 '(a b a)) #t)
  (check-equal? (grammar-derive? cfg-test2 '(b b b a b a b a)) #f)

  ;{w| the length of w is odd}
  (define cfg-test3 (make-cfg '(S P)
                              '(a b)
                              `((S -> aP)
                                (S -> bP)
                                (P -> ,EMP)
                                (P -> aS)
                                (P -> bS))
                              'S))

  (check-equal? (grammar-derive? cfg-test3 '()) #f)
  (check-equal? (grammar-derive? cfg-test3 '(a b)) #f)
  (check-equal? (grammar-derive? cfg-test3 '(a b a)) #t)
  (check-equal? (grammar-derive? cfg-test3 '(a b a b a)) #t)
  (check-equal? (grammar-derive? cfg-test3 '(a)) #t)
  (check-equal? (grammar-derive? cfg-test3 '(b)) #t)

  ;{w| the length of w is odd and its middle symbol is an "a"}
  (define cfg-test4 (make-cfg '(S)
                              '(a b)
                              '((S -> a)
                                (S -> aSb)
                                (S -> aSa)
                                (S -> bSa)
                                (S -> bSb))
                              'S))
  (check-equal? (grammar-derive? cfg-test4 '(a)) #t)
  (check-equal? (grammar-derive? cfg-test4 '(b a b)) #t)
  (check-equal? (grammar-derive? cfg-test4 '(b b a a a)) #t)
  (check-equal? (grammar-derive? cfg-test4 '(b b b a a a)) #f )
  (check-equal? (grammar-derive? cfg-test4 '()) #f)

  ;{w| w = w reverse, that is, w is a plindrome} 
  (define cfg-test5 (make-cfg '(S)
                              '(a b)
                              `((S -> ,EMP)
                                (S -> a)
                                (S -> b)
                                (S -> bSb)
                                (S -> aSa))
                              'S))

  (check-equal? (grammar-derive? cfg-test5 '())
                #t)
  (check-equal? (grammar-derive? cfg-test5 '(a))
                #t)
  (check-equal? (grammar-derive? cfg-test5 '(b))
                #t)
  (check-equal? (grammar-derive? cfg-test5 '(a b a)) #t)
  (check-equal? (grammar-derive? cfg-test5 '(b a b)) #t)
  (check-equal? (grammar-derive? cfg-test5 '(a a b b a a)) #t)
  (check-equal? (grammar-derive? cfg-test5 '( b a)) #f)

  ;{w = a^i b^j c^k, such that i=j or j=k, where i,j,k >= 0}
  ;AMIGUOUS GRAMMAR
  (define cfg-test6 (make-cfg '(S X C A Y)
                              '(a b c)
                              `((S -> XC)
                                (S -> AY)
                                (X -> aXb)
                                (X -> ,EMP)
                                (C -> cC)
                                (C -> ,EMP)
                                (A -> Aa)
                                (A -> ,EMP)
                                (Y -> bYc)
                                (Y -> ,EMP))
                              'S))

  (check-equal? (grammar-derive? cfg-test6 '(a c c)) #f) ;RUNS FOREVER???!!! 
  (check-equal? (grammar-derive? cfg-test6 '(a b)) #t)
  (check-equal? (grammar-derive? cfg-test6 '( a a b b c c)) #t)
  (check-equal? (grammar-derive? cfg-test6 '( a b c c)) #t)
  (check-equal? (grammar-derive? cfg-test6 '( a b b c c)) #t)

  ;;; csg tests

  (define CSG-an-bn (make-grammar '(S) 
                              '(a b) 
                              (list (list 'S ARROW EMP) 
                                    (list 'aSb ARROW 'aaSbb) 
                                    (list 'S ARROW 'aSb)) 
                              'S))

  (define CSG-an-bn-cn (make-grammar '(S A B C G H I) 
                                 '(a b c) 
                                 `( (S -> ABCS) (S -> G)
                                                (BA -> AB) (CA -> AC) (CB -> BC)
                                                (CG -> Gc) (G -> H) 
                                                (BH -> Hb) (H -> I)
                                                (AI -> Ia) (I -> ,EMP)) 
                                 'S))

  (check-equal? (grammar-derive? CSG-an-bn-cn '()) #t)
  (check-equal? (grammar-derive? CSG-an-bn-cn '(a b c)) #t)
  (check-equal? (grammar-derive? CSG-an-bn-cn '(a a b b c c)) #t)

  (define CSG-an-bn-an-bn-cn (grammar-concat CSG-an-bn CSG-an-bn-cn))

  (check-equal? (grammar-derive? CSG-an-bn-an-bn-cn '()) 
                #t)
  (check-equal? (grammar-derive? CSG-an-bn-an-bn-cn '(a a b b a b c))
                #t)

  ;NOTE WE DONT CHECK FOR NOT IN THE GRAMMAR BCZ IT MAY NEVER END
  (define RENAME-CSG-an-bn-cn (grammar-rename-nts (grammar-nts CSG-an-bn-cn) 
                                                  CSG-an-bn-cn))

  (check-equal? (grammar-derive? RENAME-CSG-an-bn-cn '()) #t)
  (check-equal? (grammar-derive? RENAME-CSG-an-bn-cn '(a b c)) #t)
  (check-equal? (grammar-derive? RENAME-CSG-an-bn-cn '(a a b b c c)) #t)
  )