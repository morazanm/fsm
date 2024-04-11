#lang fsm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-free grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S = words that start with n a and end with n b

;; L = a^nb^n
(define a2nb2n (make-cfg '(S)
                         '(a b)
                         `((S -> ,EMP)
                           (S -> aSb))
                         'S))

;; Tests for a^nb^n
(check-equal? (grammar-derive a2nb2n '(b b b))
              "(b b b) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b a))
              "(a b a) is not in L(G).")
(check-equal? (grammar-derive a2nb2n '(a b))
              '(S -> aSb -> ab))
(check-equal? (grammar-derive a2nb2n '(a a a b b b))
              '(S -> aSb -> aaSbb -> aaaSbbb -> aaabbb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Syntactic Categories
;;  S = words such that number of b > number of a
;;  A = words such that number of b >= number of a

;; L = w | w in (ab)* AND w has more b than a
(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; Tests for numb>numa
(check-equal? (grammar-derive numb>numa '(a b))
              "(a b) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(a b a))
              "(a b a) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(a a a a a))
              "(a a a a a) is not in L(G).")
(check-equal? (grammar-derive numb>numa '(b b b))
              '(S -> AbA -> bA -> bbA -> bbbA -> bbb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Syntactic Categories
;;  S = words such that number of a = 0 v number of a = 3
;;  B = words such that number of a = 1
;;  C = words such that number of a = 2

;; cfg for L = {w | the number of as in w is a multiple of 3}
(define MULT3-as (make-cfg '(S B C)
                           '(a b)
                           `((S ,ARROW ,EMP)
                             (S ,ARROW aB)
                             (S ,ARROW bS)
                             (B ,ARROW aC)
                             (B ,ARROW bB)
                             (C ,ARROW aS)
                             (C ,ARROW bC))
                           'S))
;; Tests for MULT3-as
(check-equal? (grammar-derive MULT3-as '(b b a b b))
              "(b b a b b) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '(b b a b b a))
              "(b b a b b a) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '(b b a b a b a a b))
              "(b b a b a b a a b) is not in L(G).")
(check-equal? (grammar-derive MULT3-as '())
              "The word () is too short to test.")
(check-equal? (grammar-derive MULT3-as '(a a a))
              '(S -> aB -> aaC -> aaaS -> aaa))
(check-equal? (grammar-derive MULT3-as '(b b a a b a b b))
              '(S -> bS -> bbS -> bbaB -> bbaaC -> bbaabC ->
                  bbaabaS -> bbaababS -> bbaababbS ->  bbaababb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Syntactic Categories
;;  S: w = ε v w = a {ab}* a v w = b {ab}* b, reads a and b pairs at start and end of w
;;  A: w = a {ab}* a v w = b {ab}* b, reads singular as and bs within the palindrome

;; L = w | w is a palindrome
#;(define palindrome (make-cfg '(S A)
                               '(a b)
                               '((S -> ε)
                                 (S -> aSa)
                                 (S -> bSb)
                                 (S -> aAa)
                                 (S -> bAb)
                                 (A -> aS)
                                 (A -> bS)) 
                               'S))
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

;; Tests for palindrome
(check-equal? (grammar-derive palindrome '(a a))
              '(S -> aSa -> aa))
(check-equal? (grammar-derive palindrome '(b b))
              '(S -> bSb -> bb))
(check-equal? (grammar-derive palindrome '(a b a))
              '(S -> aAa -> aba))
(check-equal? (grammar-derive palindrome '(a b b a))
              '(S -> aSa -> abSba -> abba))
(check-equal? (grammar-derive palindrome '(a b))
              "(a b) is not in L(G).")
(check-equal? (grammar-derive palindrome '(a b a b a))
              '(S -> aSa -> abAba -> ababa))
(check-equal? (grammar-derive palindrome '(a b a b b a))
              "(a b a b b a) is not in L(G).")
(check-equal? (grammar-derive palindrome '(a b a a a b a))
              '(S -> aSa -> abSba -> abaAaba -> abaaaba))
(check-equal? (grammar-derive palindrome '(a b a b))
              "(a b a b) is not in L(G).")
(check-equal? (grammar-derive palindrome '(b a b a b))
              '(S -> bSb -> baAab -> babab))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: w = a^i b^j c^k d^l

;; L = {a^i b^j c^k d^l | i,j,k,l ≥ 0 ∧ i+j = k+l}
(define aibjckdl (make-cfg '(S A B C D E F)
                           '(a b c d)
                           '((S -> ε)
                             (A -> ε)
                             (B -> ε)
                             (C -> ε)
                             (D -> ε)
                             (E -> ε)
                             (F -> ε)

                             (S -> A)
                             (S -> D)

                             (A -> aBc)
                             (B -> aBc)
                             (B -> bBc)

                             (A -> aCd)
                             (C -> aCd)
                             (C -> aBc)
                             (C -> B)

                             (D -> bEc)
                             (E -> bEc)
                             
                             (D -> bDd)
                             (D -> bFd)

                             (F -> bFc)
                             )
                           'S))

;; Tests for aibjckdl
#|(grammar-derive aibjckdl '(a b c d))
(grammar-derive aibjckdl '(a c))
(grammar-derive aibjckdl '(a c d))
(grammar-derive aibjckdl '(a c c))
(grammar-derive aibjckdl '(a b c c))
(grammar-derive aibjckdl '(b b b c d d))
(grammar-derive aibjckdl '(b a b c d d))
(grammar-derive aibjckdl '(a b d c))
(grammar-derive aibjckdl '(a a b c c d))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: w = ε v w = ww^R

;; L = {ww^R | w∈(a b)* ∧ w^R=w reversed}
(define wwR (make-cfg '(S)
                      '(a b)
                      '((S -> ε)
                        (S -> aSa)
                        (S -> bSb)) 
                      'S))

;; Tests for wwR
(check-equal? (grammar-derive wwR '())
              "The word () is too short to test.")
(check-equal? (grammar-derive wwR '(a a a a))
              '(S -> aSa -> aaSaa -> aaaa))
(check-equal? (grammar-derive wwR '(a b b a))
              '(S -> aSa -> abSba -> abba))
(check-equal? (grammar-derive wwR '(b a a b))
              '(S -> bSb -> baSab -> baab))
(check-equal? (grammar-derive wwR '(a a))
              '(S -> aSa -> aa))
(check-equal? (grammar-derive wwR '(b b))
              '(S -> bSb -> bb))
(check-equal? (grammar-derive wwR '(b a b))
              "(b a b) is not in L(G).")
(check-equal? (grammar-derive wwR '(b b a a))
              "(b b a a) is not in L(G).")
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: 

;; L = {a^ib^j | i≤j}
(define aibj (make-cfg '(S A B)
                       '(a b)
                       '((S -> ε)
                         (S -> AB)
                         (A -> ε)
                         (A -> AAB)
                         (A -> a)
                         (B -> b))
                       'S))

;; Tests for aibj
(check-equal? (grammar-derive aibj '())
              "The word () is too short to test.")
(check-equal? (grammar-derive aibj '(a a b b))
              '(S -> AB -> AABB -> aABB -> aaBB -> aabB -> aabb))
(check-equal? (grammar-derive aibj '(a b b b))
              '(S -> AB -> AABB -> ABB -> AABBB -> ABBB -> aBBB -> abBB -> abbB -> abbb))
(check-equal? (grammar-derive aibj '(b b))
              '(S -> AB -> AABB -> ABB -> BB -> bB -> bb))
(check-equal? (grammar-derive aibj '(b b a))
              "(b b a) is not in L(G).")
(check-equal? (grammar-derive aibj '(a a a b))
              "(a a a b) is not in L(G).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S:

;; L = {wcwR | w∈(a b)∗ ∧ wR=w reversed}
(define wcwR (make-cfg '(S A B)
                       '(a b c)
                       '((B -> ε)
                         (S -> aSa)
                         (S -> bSb)
                         (S -> aAa)
                         (S -> bAb)
                         (A -> cB)) 
                       'S))

;; Tests for wcwR
(check-equal? (grammar-derive wcwR '(a c a))
              '(S -> aAa -> acBa -> aca))
(check-equal? (grammar-derive wcwR '(b c b))
              '(S -> bAb -> bcBb -> bcb))
(check-equal? (grammar-derive wcwR '(a b a c a b a))
              '(S -> aSa -> abSba -> abaAaba -> abacBaba -> abacaba))
(check-equal? (grammar-derive wcwR '(a b c b a))
              '(S -> aSa -> abAba -> abcBba -> abcba))
(check-equal? (grammar-derive wcwR '(a b))
              "(a b) is not in L(G).")
(check-equal? (grammar-derive wcwR '(a b a b b a))
              "(a b a b b a) is not in L(G).")
(check-equal? (grammar-derive wcwR '(a b a b))
              "(a b a b) is not in L(G).")
(check-equal? (grammar-derive wcwR '(b a b a b))
              "(b a b a b) is not in L(G).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: starting nt, empty word
;;  A: not same number of as and bs
;;  B: not same number of bs and cs
;;  C: no bs 

;; L = {a^i b^j c^k | i,j,k≥0 ∧ (i̸=j or j̸=k)}
(define aibjck (make-cfg '(S A B C)
                         '(a b c)
                         '((S -> AbA)
          
                           (A -> aAb)
                           (A -> Ac)
                           (A -> ε)

                           (S -> BbB)
          
                           (B -> aB)
                           (B -> bBc)
                           (B -> ε)

                           (S -> aC)
                           (S -> Cc)
                           (C -> aC)
                           (C -> Cc)
                           (C -> ε))
                           
                         'S))

;; Tests for aibjck
(check-equal? (grammar-derive aibjck '(a b))
              '(S -> BbB -> aBbB -> abB -> ab))
(check-equal? (grammar-derive aibjck '(a c))
              '(S -> aC -> aCc -> ac))
(check-equal? (grammar-derive aibjck '(b c))
              '(S -> AbA -> bA -> bAc -> bc))
(check-equal? (grammar-derive aibjck '(a a b b c))
              '(S -> BbB -> aBbB -> aaBbB -> aabB -> aabbBc -> aabbc))
(check-equal? (grammar-derive aibjck '(a b b c c))
              '(S -> AbA -> aAbbA -> abbA -> abbAc -> abbAcc -> abbcc))
(check-equal? (grammar-derive aibjck '(a b c))
              "(a b c) is not in L(G).")
(check-equal? (grammar-derive aibjck '(b c a))
              "(b c a) is not in L(G).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: starting nt, derives a word in L 
;;  A: derives word where as = ds
;;  B: derives word where as = cs + ds
;;  C: derives word where as + bs = cs + ds 
;;  D: derives word where bs = ds
;;  E: derives word where bs = cs + ds

;; L = {a^i b^j c^k d^l | i,j,k,l≥0 ∧ i+j=k+l}
#;(define abcd (make-cfg '(S A B C D E)
                       '(a b c d)
                       `((S -> aAd)
                         (A -> aBAd)
                         (B -> aCBc)
                         (C -> aCb)
                         (A -> ,EMP)
                         (B -> ,EMP)
                         (C -> ,EMP)
                         (D -> ,EMP)
                         (E -> ,EMP)
                         (S -> bDd)
                         (D -> bEDd)
                         (E -> bEc))
                       'S))
(define abcd (make-cfg '(S A B C D E)
                       '(a b c d)
                       `((S -> A)
                         (S -> B)
                         (A -> aAd)
                         (A -> B)
                         (B -> aBc)
                         (B -> C)
                         (B -> E)
                         (C -> aCc)
                         (A -> ,EMP)
                         (B -> ,EMP)
                         (C -> ,EMP)
                         (D -> ,EMP)
                         (E -> ,EMP)
                         (S -> D)
                         (S -> E)
                         (D -> bDd)
                         (D -> E)
                         (E -> bEc))
                       'S))

;; Tests for abcd
(check-equal? (grammar-derive abcd '(a b c d))
              '(S -> A -> aAd -> aBd -> aEd -> abEcd -> abcd))
(check-equal? (grammar-derive abcd '(a c))
              '(S -> B -> aBc -> ac))
(check-equal? (grammar-derive abcd '(a c d))
              "(a c d) is not in L(G).")
(check-equal? (grammar-derive abcd '(a c c))
              "(a c c) is not in L(G).")
(check-equal? (grammar-derive abcd '(a b c c))
              '(S -> B -> aBc -> aEc -> abEcc -> abcc))
(check-equal? (grammar-derive abcd '(b b b c d d))
              '(S -> D -> bDd -> bbDdd -> bbEdd -> bbbEcdd -> bbbcdd))
(check-equal? (grammar-derive abcd '(b a b c d d))
              "(b a b c d d) is not in L(G).")
(check-equal? (grammar-derive abcd '(a b d c))
              "(a b d c) is not in L(G).")
(check-equal? (grammar-derive abcd '(a a b c c d))
              '(S -> A -> aAd -> aBd -> aaBcd -> aaEcd -> aabEccd -> aabccd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: starting nt, empty word or word with matching parents
;;  A: word with any number and combination of as and bs in between matching parents

;; L {w | w is word with properly balance parenthesis}
(define parents (make-cfg '(S A)
                          '(a b o c)
                          `((S -> oAc)
                            (S -> ,EMP)
                            (A -> aA)
                            (A -> bA)
                            (A -> S))
                          'S))

;; Tests for parents
(check-equal? (grammar-derive parents '(o a b c))
              '(S -> oAc -> oaAc -> oabAc -> oabSc -> oabc))
(check-equal? (grammar-derive parents '(o c))
              '(S -> oAc -> oSc -> oc))
(check-equal? (grammar-derive parents '(o a))
              "(o a) is not in L(G).")
(check-equal? (grammar-derive parents '(o c a))
              "(o c a) is not in L(G).")
(check-equal? (grammar-derive parents '(o a a c))
              '(S -> oAc -> oaAc -> oaaAc -> oaaSc -> oaac))
(check-equal? (grammar-derive parents '(o o a b a c c))
              '(S
                ->
                oAc
                ->
                oSc
                ->
                ooAcc
                ->
                ooaAcc
                ->
                ooabAcc
                ->
                ooabaAcc
                ->
                ooabaScc
                ->
                ooabacc))
(check-equal? (grammar-derive parents '(a b a c))
              "(a b a c) is not in L(G).")
(check-equal? (grammar-derive parents '(o b c o a c))
              "(o b c o a c) is not in L(G).")
(check-equal? (grammar-derive parents '(o a b a o a b c c))
              '(S
                ->
                oAc
                ->
                oaAc
                ->
                oabAc
                ->
                oabaAc
                ->
                oabaSc
                ->
                oabaoAcc
                ->
                oabaoaAcc
                ->
                oabaoabAcc
                ->
                oabaoabScc
                ->
                oabaoabcc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Categories
;;  S: starting nt
;;  A: words that read A
;;  B: words reading bs

;; L = {aibj | i≤2j}
(define aibj2 (make-cfg '(S A B)
                        '(a b)
                        `((S -> aABb)
                          (S -> B)
                          (A -> aAb)
                          (A -> ,EMP)
                          (B -> Bb)
                          (B -> ,EMP))
                        'S))

;; Tests for aibj2
(check-equal? (grammar-derive aibj2 '(a a b b))
              '(S -> aABb -> aaAbBb -> aabBb -> aabb))
(check-equal? (grammar-derive aibj2 '(a b b))
              '(S -> aABb -> aBb -> aBbb -> abb))
(check-equal? (grammar-derive aibj2 '(a a b))
              "(a a b) is not in L(G).")
(check-equal? (grammar-derive aibj2 '(b a))
              "(b a) is not in L(G).")
(check-equal? (grammar-derive aibj2 '(b b b))
              '(S -> B -> Bb -> Bbb -> Bbbb -> bbb))
(check-equal? (grammar-derive aibj2 '(a a b))
              "(a a b) is not in L(G).")
(check-equal? (grammar-derive aibj2 '(a a))
              "(a a) is not in L(G).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;













