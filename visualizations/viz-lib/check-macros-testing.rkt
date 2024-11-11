#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
          "../../fsm-core/private/sm-getters.rkt"
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/interface.rkt"
         "viz-state.rkt"
         "check-accept-reject-macro.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")




;; TESTING MACHINES

#|

2 Design and implement an ndfa for:
(ab)*b* ∪ ab*

S - ci = ε starting
A - ci = (ab)*
B - ci = (ab)*a 
C - ci = (ab)*b* final
D - ci = ε
E - ci = ab* final

|#

    
(define AB*B*UAB*
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K)
               (S a C)
               (K a B)
               (K ,EMP H)
               (B b K)
               (C ,EMP H)
               (H b H))))

#;(check-accept AB*B*UAB* '(a b a b a b b b b) ;'(a b a b a b b b b a) '(a b b b b b a b)
              '())
;(check-reject AB*B*UAB* '(a b a b a b b b b))
;(check-accept AB*B*UAB* '(a b b a b b a b b))
;(check-reject AB*B*UAB* '(a b b a b b a b b))
;(check-accept AB*B*UAB* '(b)) 
;(check-reject AB*B*UAB* '(a b b b b))
;(check-accept AB*B*UAB* '(a b b b b b a b))
;(check-reject AB*B*UAB* '(a b b b b b a b))
;(check-accept AB*B*UAB* '())
;(check-reject AB*B*UAB* '())

#|

TM that accepts words with equal number of a's b's and c's

PRE: i=1 AND t= LM ,BLANK w

S - i=1 and t[i] = ,BLANK, starting state
X - i>=2 and t[2..i-1]= x*, |x's| remainder 3 = 0
A - i>2 and tape[2..i-1] = x+ , |x's| remainder 3 = 1
B - i>2 and tape[2..i-1] = x*b
C - i>2 and tape[2..i-1] = x*c
D - i>3 and tape[3..i-1] = x*b
E - i>3 and tape[3..i-1] = x+ac OR x+bc
F - i>3 and tape[3..i-1] = x+ca OR x+ba
G - i>3 and tape[3..i-1] = x+bc OR x+ac
H - i>3 and tape[3..i-1] = x+ca OR x+ba
I - i>3 and tape[3..i-1] = x+cb OR x+ab
P - i>4 and tape[4..i-1] = x+a OR x+b OR x+c or x+
Y - w=x* AND [xs] remainder 3 = 0, final accepting state


|#




(define equal-a-b-c (make-tm '(S X A B C D E F G H I P Y)
                             '(a b c x)
                             `(((S ,BLANK) (X ,RIGHT))
                               ((X x) (X ,RIGHT))
                               ((X a) (A x))
                               ((X b) (B x))
                               ((X c) (C x))
                               ((X ,BLANK) (Y ,BLANK))
                               ((A x) (A ,RIGHT))
                               ((A b) (D x))
                               ((A c) (E x))
                               ((A a) (A ,RIGHT))
                               ((B x) (B ,RIGHT))
                               ((B a) (F x))
                               ((B c) (G x))
                               ((B b) (B ,RIGHT))
                               ((C x) (C ,RIGHT))
                               ((C a) (H x))
                               ((C b) (I x))
                               ((C c) (C ,RIGHT))
                               ((D x) (D ,RIGHT))
                               ((D c) (P x))
                               ((D a) (D ,RIGHT))
                               ((D b) (D ,RIGHT))
                               ((E a) (E ,RIGHT))
                               ((E c) (E ,RIGHT))
                               ((E x) (E ,RIGHT))
                               ((E b) (P x))
                               ((F x) (F ,RIGHT))
                               ((F c) (P x))
                               ((F a) (F ,RIGHT))
                               ((F b) (F ,RIGHT))
                               ((G x) (G ,RIGHT))
                               ((G a) (P x))
                               ((G b) (G ,RIGHT))
                               ((G c) (G ,RIGHT))
                               ((H a) (H ,RIGHT))
                               ((H c) (H ,RIGHT))
                               ((H x) (H ,RIGHT))
                               ((H b) (P x))
                               ((I x) (I ,RIGHT))
                               ((I a) (P x))
                               ((I b) (I ,RIGHT))
                               ((I c) (I ,RIGHT))
                               ((P x) (P ,LEFT))
                               ((P c) (P ,LEFT))
                               ((P b) (P ,LEFT))
                               ((P a) (P ,LEFT))
                               ((P ,BLANK) (X ,RIGHT)))
                             'S '(Y) 'Y))


(check-accept equal-a-b-c [`(,LM ,BLANK a a) 1])
;(check-accept equal-a-b-c [`(,LM ,BLANK a a) 1])
#|(check-accept equal-a-b-c [`(,LM ,BLANK a a) 1])
(check-accept equal-a-b-c `(,LM ,BLANK b b b) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK c) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK b a b c) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK a c b) 1)
(check-accept equal-a-b-c `(,LM ,BLANK a a b c) 1)
(check-accept equal-a-b-c `(,LM ,BLANK a a b b b c c) 1)
(check-accept equal-a-b-c `(,LM ,BLANK a b c c) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK a a b b c c a b c) 1)
(check-accept equal-a-b-c `(,LM ,BLANK) 1)
(check-accept equal-a-b-c `(,LM ,BLANK a b c) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK a a b b c c) 1) 
(check-accept equal-a-b-c `(,LM ,BLANK a a a b b b c c c) 1) |#

;(check-reject equal-a-b-c [`(,LM ,BLANK a a) 1]) 
;(check-reject equal-a-b-c [`(,LM ,BLANK b b b) 1]) 
;(check-reject equal-a-b-c [`(,LM ,BLANK c) 1]) 
;(check-reject equal-a-b-c [`(,LM ,BLANK a c b) 1]) 
#|(check-reject equal-a-b-c `(,LM ,BLANK a a b c) 1) 
(check-reject equal-a-b-c `(,LM ,BLANK a a b b b c c) 1) 
(check-reject equal-a-b-c `(,LM ,BLANK a b c c) 1)
(check-reject equal-a-b-c `(,LM ,BLANK a a b b c c a b c) 1)
(check-reject equal-a-b-c `(,LM ,BLANK) 1) 
(check-reject equal-a-b-c `(,LM ,BLANK a b c) 1)
(check-reject equal-a-b-c `(,LM ,BLANK a a b b c c) 1)
(check-reject equal-a-b-c `(,LM ,BLANK a a a b b b c c c) 1) 
|#




;; L(M) = at least one missing
(define AT-LEAST-ONE-MISSING (make-ndfa '(S A B C)
                                        '(a b c)
                                        'S
                                        '(A B C)
                                        `((S ,EMP A)
                                          (S ,EMP B)
                                          (S ,EMP C)
                                          (A b A)
                                          (A c A)
                                          (B a B)
                                          (B c B)
                                          (C a C)
                                          (C b C))))
#|
(check-accept AT-LEAST-ONE-MISSING '(a b c)) 
(check-accept AT-LEAST-ONE-MISSING '(b b a b c b a)) 
(check-accept AT-LEAST-ONE-MISSING '(b a c)) 
(check-accept AT-LEAST-ONE-MISSING '()) 
(check-accept AT-LEAST-ONE-MISSING '(a))
(check-accept AT-LEAST-ONE-MISSING '(b)) 
(check-accept AT-LEAST-ONE-MISSING '(c))
(check-accept AT-LEAST-ONE-MISSING '(c c a a)) 
(check-accept AT-LEAST-ONE-MISSING '(b b c b b b)) 
(check-accept AT-LEAST-ONE-MISSING '(a a a b b b))

(check-reject AT-LEAST-ONE-MISSING '(a b c)) 
(check-reject AT-LEAST-ONE-MISSING '(b b a b c b a))
(check-reject AT-LEAST-ONE-MISSING '(b a c))
(check-reject AT-LEAST-ONE-MISSING '())
(check-reject AT-LEAST-ONE-MISSING '(a))
(check-reject AT-LEAST-ONE-MISSING '(b))
(check-reject AT-LEAST-ONE-MISSING '(c))
(check-reject AT-LEAST-ONE-MISSING '(c c a a))
(check-reject AT-LEAST-ONE-MISSING '(b b c b b b))
(check-reject AT-LEAST-ONE-MISSING '(a a a b b b))

|#




;; States (i = head’s position)
;; S: tape[1..i-1], contains even a's or b's
;; A: tape[i] = BLANK, tape[1..i-1] contains even number of a's and b's, final state
;; B: tape[1...i-1] contains odd number of a's and even number of b's
;; C: tape[1..i-1] contains even number of a's and odd number of b's
;; D: tape[i] = BLANK and tape[1..i-1] contains an odd number of a's and even number of b's final
;; E: tape[i] = BLANK and tape[1..i-1] contains even number of a's and odd number of b's final
;; F: tape[1..i-1] contains an odd number of a's and odd number of b's
;; G: tape[i] = BLANK and tape[1..i-1] contains an odd number of a's and odd number of b's final

;; PRE: tape = LMw_ AND i = 0
(define even-a-even-b (make-tm '(S A B C D E F G)
                               '(a b)
                               `(((S a) (B ,RIGHT))
                                 ((S b) (C ,RIGHT))
                                 ((S ,BLANK) (A ,BLANK))
                                 ((B a) (S ,RIGHT))
                                 ((B b) (F ,RIGHT))
                                 ((B ,BLANK) (D ,BLANK))
                                 ((C a) (F ,RIGHT))
                                 ((C b) (S ,RIGHT))
                                 ((C ,BLANK) (E ,BLANK))
                                 ((F a) (C ,RIGHT))
                                 ((F b) (B ,RIGHT))
                                 ((F ,BLANK) (G ,BLANK)))
                               'S
                               '(A D E G)
                               'A))

;; Tests for even-a-even-b

#|
(check-accept even-a-even-b `(,LM a a a a b b))
(check-accept even-a-even-b `(,LM a a a b b))
(check-accept even-a-even-b `(,LM))
(check-accept even-a-even-b `(,LM a a a a b b b))
(check-accept even-a-even-b `(,LM a b))


(check-reject even-a-even-b `(,LM a a a a b b)) 
(check-reject even-a-even-b `(,LM a a a b b))
(check-reject even-a-even-b `(,LM))
(check-reject even-a-even-b `(,LM a a a a b b b))
(check-reject even-a-even-b `(,LM a b)) 

|#




;; TESTING GRAMMARS

(define even-bs-odd-as
  (make-unchecked-cfg '(S A B C)
                      '(a b)
                      `((S ,ARROW aA)
                        (S ,ARROW bB)
                        (S ,ARROW a)
                        (A ,ARROW aS)
                        (A ,ARROW bC)
                        (B ,ARROW aC)
                        (B ,ARROW bS)
                        (C ,ARROW aB)
                        (C ,ARROW bA)
                        (C ,ARROW b))
                      'S))


(check-accept even-bs-odd-as '(b b b b a a a) '(b))

;(check-accept even-bs-odd-as '(b b b a a a))
#|(check-accept even-bs-odd-as '(b a a a)) 
(check-accept even-bs-odd-as '(a a a))
(check-accept even-bs-odd-as '(b b b b))
(check-accept even-bs-odd-as '(b b b b a a a a))

|#

#|


(check-reject even-bs-odd-as '(b b b b a a a))
(check-reject even-bs-odd-as '(b b b a a a))
(check-reject even-bs-odd-as '(b a a a))
(check-reject even-bs-odd-as '(a a a))
(check-reject even-bs-odd-as '(b b b b))
(check-reject even-bs-odd-as '(b b b b a a a a))





;; L = {wwR | w ∈ (a b)* AND wR = w reversed}
(define reverse-w (make-cfg '(S)
                            '(a b)
                            `((S -> aSa)
                              (S -> bSb)
                              (S -> ,EMP))
                            'S))


(check-accept reverse-w '(b b b b a a a)) 
(check-accept reverse-w '(a a a a a a)) 
(check-accept reverse-w '(a a a b a a a))
(check-accept reverse-w '(a a b b a a)) 
(check-accept reverse-w '(b b b b))
(check-accept reverse-w '(b b b b a a a a))

(check-reject reverse-w '(b b b b a a a)) 
(check-reject reverse-w '(a a a a a a))
(check-reject reverse-w '(a a a b a a a)) 
(check-reject reverse-w '(a a b b a a))
(check-reject reverse-w '(b b b b))
(check-reject reverse-w '(b b b b a a a a)) 

;; L = palindromes
(define palindrome (make-cfg '(S A)
                             '(a b)
                             `((S -> ,EMP)
                               (S -> aSa)
                               (S -> bSb)
                               (S -> aAa)
                               (S -> bAb)
                               (A -> a)
                               (A -> b))
                             'S))



(check-accept palindrome '(b b b b a a a))
(check-accept palindrome '(a a a a a a))
(check-accept palindrome '(a a a b a a a)) 
(check-accept palindrome '(a a b b a a))
(check-accept palindrome '(b b a b b))
(check-accept palindrome '(b b b b a a a a))

(check-reject palindrome '(b b b b a a a))
(check-reject palindrome '(a a a a a a))
(check-reject palindrome '(a a a b a a a))
(check-reject palindrome '(a a b b a a))
(check-reject palindrome '(b b a b b))
(check-reject palindrome '(b b b b a a a a))



;; L = a^ib^j i<=j
(define aibj (make-cfg '(S A)
                       '(a b)
                       `((S -> b)
                         (S -> bS)
                         (S -> aAb)
                         (A -> ,EMP)
                         (A -> aAb)
                         (A -> bS)) 
                       'S))

(check-accept aibj '(a a a a b b b b))
(check-accept aibj '(a a a a))
(check-accept aibj '(a a a b a a a)) 
(check-accept aibj '(a a b b a a))
(check-accept aibj '(b b a b b))
(check-accept aibj '(a a a a b b b b b b b b))

(check-reject aibj '(a a a a b b b b)) 
(check-reject aibj '(a a a a))
(check-reject aibj '(a a a b a a a))
(check-reject aibj '(a a b b a a))
(check-reject aibj '(b b a b b))
(check-reject aibj '(a a a a b b b b b b b b)) 

|#