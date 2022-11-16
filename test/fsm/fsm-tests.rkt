#lang racket

(require "../../main.rkt")



(module+ test
  (require rackunit)

;; ---- DFA TESTS ---

; L = a*
(define dfa-a* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q a Q))))

(check-equal? (sm-apply dfa-a* ' (a b a a b)) 'reject)
(check-equal? (sm-apply dfa-a* '(b b a b )) 'reject)
(check-equal? (sm-apply dfa-a* '(b b b b b )) 'reject)
(check-equal? (sm-apply dfa-a* '(b a a a a a)) 'reject)
(check-equal? (sm-apply dfa-a* '(a a a a a a)) 'accept)
(check-equal? (sm-apply dfa-a* '( a )) 'accept)
(check-equal? (sm-apply dfa-a* '()) 'accept)

; L = b*
(define dfa-b* (make-dfa '(Q) '(a b) 'Q '(Q) '((Q b Q))))

(check-equal? (sm-apply dfa-b* '(a a a a a a)) 'reject)
(check-equal? (sm-apply dfa-b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply dfa-b* '(b b a b )) 'reject)
(check-equal? (sm-apply dfa-b* '(b b b b b)) 'accept)
(check-equal? (sm-apply dfa-b* '(b)) 'accept)
(check-equal? (sm-apply dfa-b* '()) 'accept)

; L = {a*b+} 
(define dfa-a*b+ (make-dfa '(Q R S) '(a b) 'Q '(R) '((Q a Q)
                                                     (Q b R)
                                                     (R b R))))
(check-equal? (sm-apply dfa-a*b+ '(b b a a a)) 'reject)
(check-equal? (sm-apply dfa-a*b+ '(a a a)) 'reject)
(check-equal? (sm-apply dfa-a*b+ '()) 'reject)
(check-equal? (sm-apply dfa-a*b+ '(a a a a a b b)) 'accept)
(check-equal? (sm-apply dfa-a*b+ '(b)) 'accept)
(check-equal? (sm-apply dfa-a*b+ '(b b b b b)) 'accept)

; L = a* U b*
(define dfa-a*Unionb* (sm-union dfa-a* dfa-b*))

(check-equal? (sm-apply dfa-a*Unionb* '(b a)) 'reject)
(check-equal? (sm-apply dfa-a*Unionb* '(a a b a)) 'reject)
(check-equal? (sm-apply dfa-a*Unionb* '(b b a b)) 'reject)
(check-equal? (sm-apply dfa-a*Unionb* '()) 'accept)
(check-equal? (sm-apply dfa-a*Unionb* '( a )) 'accept)
(check-equal? (sm-apply dfa-a*Unionb* '( b )) 'accept)
(check-equal? (sm-apply dfa-a*Unionb* '(a a a a )) 'accept)
(check-equal? (sm-apply dfa-a*Unionb* '(b b b b b)) 'accept)
(check-equal? (sm-apply dfa-a*Unionb* '(a a a b b b b)) 'reject)

(define dfa-showtransitions-a*Unionb* (sm-showtransitions dfa-a*Unionb* '(a a a a))) 

;L = {w/ w starts and ends with an "a"}
(define dfa-test1 (make-dfa '(Q R S ds)
                            '(a b) 
                            'Q 
                            '(R)
                            '((Q a R)
                              (Q b ds)
                              (R a R)
                              (R b S)
                              (S a R)
                              (S b S)
                              (ds a ds)
                              (ds b ds))))

(check-equal? (sm-apply dfa-test1 '()) 'reject)
(check-equal? (sm-apply dfa-test1 '(a a a b b b b)) 'reject)
(check-equal? (sm-apply dfa-test1 '(a b a b a b a b)) 'reject)
(check-equal? (sm-apply dfa-test1 '(b)) 'reject)
(check-equal? (sm-apply dfa-test1 '(a)) 'accept)
(check-equal? (sm-apply dfa-test1 '(a b a b b b b a)) 'accept)
(check-equal? (sm-apply dfa-test1 '(a a a a)) 'accept)


(define dfa-renamests-a* (sm-rename-states (sm-states dfa-a*) dfa-a*)) 

(check-equal? (sm-apply dfa-renamests-a* ' (a b a a b)) 'reject)
(check-equal? (sm-apply dfa-renamests-a* '(b b a b )) 'reject)
(check-equal? (sm-apply dfa-renamests-a* '(b b b b b )) 'reject)
(check-equal? (sm-apply dfa-renamests-a* '(b a a a a a)) 'reject)
(check-equal? (sm-apply dfa-renamests-a* '(a a a a a a)) 'accept)
(check-equal? (sm-apply dfa-renamests-a* '( a )) 'accept)
(check-equal? (sm-apply dfa-renamests-a* '()) 'accept)

(define dfa-renamests-b* (sm-rename-states (sm-states dfa-b*) dfa-b*))  

(check-equal? (sm-apply dfa-renamests-b* '(a a a a a a)) 'reject)
(check-equal? (sm-apply dfa-renamests-b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply dfa-renamests-b* '(b b a b )) 'reject)
(check-equal? (sm-apply dfa-renamests-b* '(b b b b b)) 'accept)
(check-equal? (sm-apply dfa-renamests-b* '(b)) 'accept)
(check-equal? (sm-apply dfa-renamests-b* '()) 'accept)

(define dfa-renamests-a*Unionb* (sm-rename-states (sm-states dfa-a*Unionb*)dfa-a*Unionb*))

(check-equal? (sm-apply dfa-renamests-a*Unionb* '(b a)) 'reject)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a b a)) 'reject)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '(b b a b)) 'reject)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '()) 'accept)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '( a )) 'accept)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '( b )) 'accept)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a a a )) 'accept)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '(b b b b b)) 'accept)
(check-equal? (sm-apply dfa-renamests-a*Unionb* '(a a a b b b b)) 'reject)


;; ---- NDFA TESTS ---

; L = a*/b* 
(define ndfa-inter-a*-b* (sm-intersection dfa-a* dfa-b*))

(check-equal? (sm-apply ndfa-inter-a*-b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply ndfa-inter-a*-b* ' (b b b)) 'reject)
(check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a)) 'reject)
(check-equal? (sm-apply ndfa-inter-a*-b* ' (a b)) 'reject)
(check-equal? (sm-apply ndfa-inter-a*-b* ' (a a a b ))'reject)
(check-equal? (sm-apply ndfa-inter-a*-b* ' ()) 'accept)

(define ndfa-renamests-inter-a*-b* (sm-rename-states (sm-states ndfa-inter-a*-b*)ndfa-inter-a*-b*))

(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' (b b b)) 'reject)
(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' (a a a)) 'reject)
(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' (a b)) 'reject)
(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' (a a a b ))'reject)
(check-equal? (sm-apply ndfa-renamests-inter-a*-b* ' ()) 'accept)

; L = a*b*
(define ndfa-concat-a*b* (sm-concat dfa-a* dfa-b*))

(check-equal? (sm-apply ndfa-concat-a*b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply ndfa-concat-a*b* ' (b b a a b)) 'reject)
(check-equal? (sm-apply ndfa-concat-a*b* '(a a a a a a)) 'accept)
(check-equal? (sm-apply ndfa-concat-a*b* '(b b b )) 'accept)
(check-equal? (sm-apply ndfa-concat-a*b* '(b b b b b)) 'accept)
(check-equal? (sm-apply ndfa-concat-a*b* ' ()) 'accept)

; L = not a*
(define ndfa-not-a* (sm-complement dfa-a*))

(check-equal? (sm-apply ndfa-not-a* ' ()) 'reject)
(check-equal? (sm-apply ndfa-not-a* ' ( a )) 'reject)
(check-equal? (sm-apply ndfa-not-a* ' (a a a )) 'reject)
(check-equal? (sm-apply ndfa-not-a* ' (b b b)) 'accept)
(check-equal? (sm-apply ndfa-not-a* ' (b b b a b a)) 'accept)

; L = not b*
(define ndfa-not-b* (sm-complement dfa-b*))

(check-equal? (sm-apply ndfa-not-b* ' ()) 'reject)
(check-equal? (sm-apply ndfa-not-b* ' ( b )) 'reject)
(check-equal? (sm-apply ndfa-not-b* ' (b b b )) 'reject)
(check-equal? (sm-apply ndfa-not-b* ' (a a a)) 'accept)
(check-equal? (sm-apply ndfa-not-b* ' (b b b a b a)) 'accept)

;L = not M1 union not M2
(define nota*Unotb* (sm-union  ndfa-not-a* ndfa-not-b* ))

(check-equal? (sm-apply nota*Unotb* ' ()) 'reject)
(check-equal? (sm-apply nota*Unotb* ' ( b )) 'accept)
(check-equal? (sm-apply nota*Unotb* ' ( a )) 'accept)
(check-equal? (sm-apply nota*Unotb* ' (b b b)) 'accept)
(check-equal? (sm-apply nota*Unotb* ' (a a a)) 'accept)
(check-equal? (sm-apply nota*Unotb* ' (a b b a a )) 'accept)

; L = (a*b*)*
(define ndfa-kleenestar-concat-a*b* (sm-kleenestar ndfa-concat-a*b*))

(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' ()) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (a)) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (b)) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (a a a a )) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (b b b b )) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (a a a b b a a)) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (b b b a b a)) 'accept)
(check-equal? (sm-apply ndfa-kleenestar-concat-a*b* ' (b b a b b b )) 'accept)

; L = ndfa to dfa 
(define ndfa-dfa-concat-a*b* (ndfa->dfa ndfa-concat-a*b*))

(check-equal? (sm-apply ndfa-dfa-concat-a*b* ' (a b a a b)) 'reject)
(check-equal? (sm-apply ndfa-dfa-concat-a*b* ' (b b a a b)) 'reject)
(check-equal? (sm-apply ndfa-dfa-concat-a*b* '(a a a a a a)) 'accept)
(check-equal? (sm-apply ndfa-dfa-concat-a*b* '(b b b )) 'accept)
(check-equal? (sm-apply ndfa-dfa-concat-a*b* '(b b b b b)) 'accept)
(check-equal? (sm-apply ndfa-dfa-concat-a*b* ' ()) 'accept)

; L = { ((ab U aab)* a*)*}
(define ndfa-test1 (make-ndfa '(Q R S T U V W X Y Z F) 
                              '(a b) 
                              'Q 
                              '(F Q)
                              `((Q ,EMP R)
                                (R ,EMP S)
                                (R ,EMP F)
                                (S ,EMP T)
                                (S ,EMP W)
                                (T a U)
                                (U b V)
                                (V ,EMP S)
                                (V ,EMP F)
                                (W a X)
                                (X a Y)
                                (Y b Z)
                                (Z ,EMP F)
                                (F a F)
                                (F ,EMP R))))

(check-equal? (sm-apply ndfa-test1 ' ( b )) 'reject)
(check-equal? (sm-apply ndfa-test1 ' ( b a )) 'reject)
(check-equal? (sm-apply ndfa-test1 ' (b b b b a a b a )) 'reject)
(check-equal? (sm-apply ndfa-test1 ' ()) 'accept)
(check-equal? (sm-apply ndfa-test1 ' ( a )) 'accept)
(check-equal? (sm-apply ndfa-test1 ' ( a a a a )) 'accept)
(check-equal? (sm-apply ndfa-test1 ' (a b)) 'accept)
(check-equal? (sm-apply ndfa-test1 ' (a a b)) 'accept)
(check-equal? (sm-apply ndfa-test1 ' (a b a a a a a a b)) 'accept)
(check-equal? (sm-apply ndfa-test1 ' (a a b a a b a b a a a )) 'accept)

; L = a*bab*
(define ndfa-a*bab*(make-dfa '(Q R S ds)
                             '(a b) 
                             'Q 
                             '(S)
                             '((Q a Q)
                               (Q b R)
                               (R a S)
                               (R b ds)
                               (S a ds)
                               (S b S)
                               (ds a ds)
                               (ds b ds))))

(check-equal? (sm-apply  ndfa-a*bab* ' ()) 'reject)
(check-equal? (sm-apply  ndfa-a*bab* ' ( b )) 'reject)
(check-equal? (sm-apply  ndfa-a*bab* ' ( a b b )) 'reject)
(check-equal? (sm-apply  ndfa-a*bab* ' ( b a b a )) 'reject)
(check-equal? (sm-apply  ndfa-a*bab* ' ( b a )) 'accept)
(check-equal? (sm-apply  ndfa-a*bab* ' ( a b a )) 'accept)
(check-equal? (sm-apply  ndfa-a*bab* ' ( a b a b )) 'accept)
(check-equal? (sm-apply  ndfa-a*bab* ' ( a a a b a b b b b )) 'accept)

(define aUb (regexp->fsa (union-regexp (singleton-regexp "a") (singleton-regexp "b"))))

(check-equal? (sm-apply aUb '()) 'reject)
(check-equal? (sm-apply aUb '(a a b b)) 'reject)
(check-equal? (sm-apply aUb '(b a b a)) 'reject)
(check-equal? (sm-apply aUb '(a a a)) 'reject)
(check-equal? (sm-apply aUb '(b b b b)) 'reject)
(check-equal? (sm-apply aUb '(a)) 'accept)
(check-equal? (sm-apply aUb '(b)) 'accept)

;; pda tests

; K sigma gamma start finals pdarules
; L = equal number of a & b
(define pda-eq#asndbs (make-ndpda '(S M F)
                                  '(a b)
                                  '(a b)
                                  'S
                                  '(F)
                                  `(((S ,EMP ,EMP) (M ,EMP))
                                    ((M a ,EMP) (M ,(list 'a)))
                                    ((M b ,(list 'a)) (M ,EMP))
                                    ((M a ,(list 'b)) (M ,EMP))
                                    ((M b ,EMP) (M ,(list 'b)))
                                    ((M ,EMP ,EMP) (F ,EMP)))))

(check-equal? (sm-apply pda-eq#asndbs '( a )) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '( b )) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '(a a)) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '(b b)) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '(a a b b a )) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '(b a b a a a)) 'reject)
(check-equal? (sm-apply pda-eq#asndbs '()) 'accept)
(check-equal? (sm-apply pda-eq#asndbs '(b b b a a a b a b a)) 'accept)
(check-equal? (sm-apply pda-eq#asndbs '(a a a a b a b b b b)) 'accept)

(define pda-renamests-eq#asndbs (sm-rename-states (sm-states pda-eq#asndbs) pda-eq#asndbs))

(check-equal? (sm-apply pda-renamests-eq#asndbs '( a )) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '( b )) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(a a)) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(b b)) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(a a b b a )) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(b a b a a a)) 'reject)
(check-equal? (sm-apply pda-renamests-eq#asndbs '()) 'accept)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(b b b a a a b a b a)) 'accept)
(check-equal? (sm-apply pda-renamests-eq#asndbs '(a a a a b a b b b b)) 'accept)

(define pda-kleenestar-eq#asndbs (sm-kleenestar pda-eq#asndbs)) 

(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( a )) 'reject)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( b )) 'reject)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( a a)) 'reject)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( b b )) 'reject)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '()) 'accept)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( a b ))'accept)
(check-equal? (sm-apply pda-kleenestar-eq#asndbs '( b a b a a a b b )) 'accept)

; L = wcw^R
(define pda-wCwreverse (make-ndpda '(S M M2 F)
                                   '(a b c)
                                   '(a b)
                                   'S
                                   '(F)
                                   `(((S ,EMP ,EMP) (M ,EMP))
                                     ((M a ,EMP) (M (a)))
                                     ((M b ,EMP) (M (b)))
                                     ((M c ,EMP) (M2 ,EMP))
                                     ((M2 b (b)) (M2 ,EMP))
                                     ((M2 a (a)) (M2 ,EMP))
                                     ((M2 ,EMP ,EMP) (F ,EMP)))))

(check-equal? (sm-apply pda-wCwreverse '()) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(b)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(a)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(a b c)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(b a c)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(c a b)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(c a)) 'reject)
(check-equal? (sm-apply pda-wCwreverse '(c)) 'accept)
(check-equal? (sm-apply pda-wCwreverse '(a b c b a)) 'accept)
(check-equal? (sm-apply pda-wCwreverse '(b b a c a b b)) 'accept)
(check-equal? (sm-apply pda-wCwreverse '(a a b a c a b a a)) 'accept)

(define pda-union-wCwreverse-eq#asndbs (sm-union pda-wCwreverse pda-eq#asndbs))

(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (a b a)) 'reject)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (a)) 'reject)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (b)) 'reject)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (b b a)) 'reject)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' ()) 'accept)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (c)) 'accept)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (a a b c b a a)) 'accept)
(check-equal? (sm-apply pda-union-wCwreverse-eq#asndbs ' (b a a b c b a a b)) 'accept)

; L = a^nb^2n
(define pda-a^nb^2n (make-ndpda '(S M F)
                                '(a b)
                                '(a)
                                'S
                                '(F)
                                (list (list (list 'S EMP EMP) (list 'M EMP))
                                      (list (list 'M 'a EMP) (list 'M '(a a)))
                                      (list (list 'M 'b '(a)) (list 'M EMP))
                                      (list (list 'M EMP EMP) (list 'F EMP)))))

(check-equal? (sm-apply  pda-a^nb^2n '(b a)) 'reject)
(check-equal? (sm-apply  pda-a^nb^2n '(a a b b b b a b)) 'reject)
(check-equal? (sm-apply  pda-a^nb^2n '(a b)) 'reject)
(check-equal? (sm-apply  pda-a^nb^2n '(b)) 'reject) 
(check-equal? (sm-apply  pda-a^nb^2n '()) 'accept)
(check-equal? (sm-apply  pda-a^nb^2n '(a b b)) 'accept)
(check-equal? (sm-apply  pda-a^nb^2n '(a a b b b b)) 'accept)

; L = a^2nb^n
(define pda-a^2nb^n (make-ndpda '(S M F)
                                '(a b)
                                '(a)
                                'S
                                '(F)
                                (list (list (list 'S EMP EMP) (list 'M EMP))
                                      (list (list 'M 'a EMP) (list 'M '(a)))
                                      (list (list 'M 'b '(a a)) (list 'M EMP))
                                      (list (list 'M EMP EMP) (list 'F EMP)))))

(check-equal? (sm-apply pda-a^2nb^n ' (a a a b b)) 'reject)
(check-equal? (sm-apply pda-a^2nb^n ' (b a)) 'reject)
(check-equal? (sm-apply pda-a^2nb^n ' (a b)) 'reject)
(check-equal? (sm-apply  pda-a^2nb^n '(b)) 'reject)
(check-equal? (sm-apply pda-a^2nb^n ' (a a a b b b)) 'reject)
(check-equal? (sm-apply pda-a^2nb^n ' (a a a a b b)) 'accept)
(check-equal? (sm-apply pda-a^2nb^n ' (a a b)) 'accept)
(check-equal? (sm-apply  pda-a^2nb^n '()) 'accept)


(define rename-states-pda-a^2nb^n (sm-rename-states (sm-states pda-a^2nb^n) pda-a^2nb^n))

(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (a a a b b)) 'reject)
(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (b a)) 'reject)
(check-equal? (sm-apply  rename-states-pda-a^2nb^n '(b)) 'reject)
(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (a b)) 'reject)
(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (a a a b b b)) 'reject)
(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (a a a a b b)) 'accept)
(check-equal? (sm-apply rename-states-pda-a^2nb^n ' (a a b)) 'accept)
(check-equal? (sm-apply  rename-states-pda-a^2nb^n '()) 'accept)


(define pda-transitions-pda-a^2nb^n-1 (sm-showtransitions pda-a^2nb^n '(a a b)))
(check-equal? pda-transitions-pda-a^2nb^n-1 '((S (a a b) ()) 
                                              (M (a a b) ()) 
                                              (M (a b) (a)) (M (b) (a a)) 
                                              (M () ()) (F () ()) accept))

(define pda-transitions-pda-a^2nb^n-2 (sm-showtransitions pda-a^2nb^n '(b a)))
(check-equal? pda-transitions-pda-a^2nb^n-2 'reject)

(define pda-transitions-pda-a^nb^2n-1 (sm-showtransitions pda-a^nb^2n '(b a)))
(check-equal? pda-transitions-pda-a^nb^2n-1 'reject)

(define pda-transitions-pda-a^nb^2n-2 (sm-showtransitions pda-a^nb^2n '(a a b b b b)))
(check-equal? pda-transitions-pda-a^nb^2n-2 '((S (a a b b b b) ())
                                              (M (a a b b b b) ())
                                              (M (a b b b b) (a a))
                                              (M (b b b b) (a a a a))
                                              (M (b b b) (a a a))
                                              (M (b b) (a a))
                                              (M (b) (a))
                                              (M () ())
                                              (F () ())
                                              accept))


;;; tm tests

;L = {w/ w starts with an "a"}
(define tm-STARTS-WITH-A (make-tm '(Q R S)
                                  '(a b)
                                  (list (list '(Q a) '(S a))
                                        (list '(Q b) '(R b))
                                        (list (list 'Q BLANK) (list 'R BLANK)))
                                  'Q
                                  '(R S)
                                  'S))

(check-equal? (sm-apply tm-STARTS-WITH-A ' (b a b)) 'reject)

(check-equal? (sm-apply tm-STARTS-WITH-A '(_)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A '(b a b)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A '(b)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A '(a)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-A '(a b a)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-A '(a a b a a b)) 'accept)

;L = {w/ w starts with a "b"}    
(define tm-STARTS-WITH-B (make-tm '(S Y N)
                                  '(a b)
                                  (list (list '(S b) '(Y b))
                                        (list '(S a) '(N a))
                                        (list (list 'S BLANK) (list 'N BLANK)))
                                  'S
                                  '(Y N)
                                  'Y))

(check-equal? (sm-apply tm-STARTS-WITH-B (list BLANK)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-B '(b a b)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-B '(a)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-B '(a b a)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-B '(a a b a a b)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-B '(b)) 'accept)

#|
(define tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B (sm-intersection tm-STARTS-WITH-A tm-STARTS-WITH-B))

(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B '()) 'accept)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B '(b a b)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B '(a a b)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B '(b)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-B '(a)) 'reject)
|#

;L = {w/ w starts with a "c"}  
(define tm-STARTS-WITH-C (make-tm '(S Y N)
                                  '(c d)
                                  (list (list '(S c) '(Y c))
                                        (list '(S d) '(N d))
                                        (list (list 'S BLANK) (list 'N BLANK)))
                                  'S
                                  '(N Y)
                                  'Y))

(check-equal? (sm-apply tm-STARTS-WITH-C (list BLANK)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-C ' (d)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-C ' ( d c c d d )) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-C ' (c)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-C ' ( c d c d c c)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-C ' ( c d d d d )) 'accept)

#|
(define tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C (sm-intersection tm-STARTS-WITH-A tm-STARTS-WITH-C))

(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '()) 'accept)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(c d a b a b)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(a a b c d )) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(c)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(a)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(b)) 'reject)
(check-equal? (sm-apply tm-INTERSECTION-STARTS-WITH-A-STARTS-WITH-C '(d)) 'reject)
|#

(define tm-STARTS-WITH-A-OR-C (sm-union tm-STARTS-WITH-A tm-STARTS-WITH-C))

(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C (list BLANK)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(b)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(d)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(b d c a)) 'reject)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(a)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(c)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '( a b b b a)) 'accept)
(check-equal? (sm-apply tm-STARTS-WITH-A-OR-C '(c d d d c)) 'accept)

;(define tm-CONCAT-STARTS-WITH-A-STARTS-WITH-C (sm-concat tm-STARTS-WITH-A tm-STARTS-WITH-C))

;(check-equal? (sm-apply tm-CONCAT-STARTS-WITH-A-STARTS-WITH-C (list BLANK)) 'reject)
;(check-equal? (sm-apply tm-CONCAT-STARTS-WITH-A-STARTS-WITH-C '(a b b c d d)) 'accept)

;(define tm-STARTS-WITH-A* (sm-kleenestar tm-STARTS-WITH-A))

;(check-equal? (sm-apply tm-STARTS-WITH-A* (list BLANK)) 'accept)
;(check-equal? (sm-apply tm-STARTS-WITH-A* '(b a b a b)) 'reject)
;(check-equal? (sm-apply tm-STARTS-WITH-A* '(a b a a b a b)) 'accept)
;(check-equal? (sm-apply tm-STARTS-WITH-A* '(a)) 'accept)

;(define tm-NOT-STARTS-WITH-A (sm-complement tm-STARTS-WITH-A))

;(check-equal? (sm-apply tm-NOT-STARTS-WITH-A '(a)) 'reject)
;(check-equal? (sm-apply tm-NOT-STARTS-WITH-A '(a b a b b b )) 'reject)
;(check-equal? (sm-apply tm-NOT-STARTS-WITH-A (list BLANK)) 'accept)
;(check-equal? (sm-apply tm-NOT-STARTS-WITH-A '(b a a)) 'accept)
;(check-equal? (sm-apply tm-NOT-STARTS-WITH-A '(b)) 'accept)

(define tm-WriteBlank (make-tm '(S H) 
                               '(i j k) 
                               (list
                                (list (list 'S 'i) (list 'H BLANK)) 
                                (list (list 'S BLANK) (list 'H BLANK))
                                (list (list 'S 'j) (list 'H BLANK)) 
                                (list (list 'S 'k) (list 'H BLANK)))
                               'S
                               '(H)))

(check-equal? (last (sm-showtransitions tm-WriteBlank '(i i i) 1)) '(H 1 (i _ i)))
(check-equal? (last (sm-showtransitions tm-WriteBlank '(i i i) 0)) '(H 0 (_ i i)))
(check-equal? (last (sm-showtransitions tm-WriteBlank `(i ,BLANK i ,BLANK i i ,BLANK) 4))
              '(H 4 (i _ i _ _ i _)))

(define tm-WriteI (make-tm '(S H) 
                           '(i j k) 
                           (list 
                            (list (list 'S 'i) (list 'H 'i)) 
                            (list (list 'S BLANK) (list 'H 'i))
                            (list (list 'S 'j) (list 'H 'i)) 
                            (list (list 'S 'k) (list 'H 'i)))
                           'S
                           '(H)))

(check-equal? (last (sm-showtransitions tm-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 1))
              '(H 1 (i i i _ i i _)))
(check-equal? (last (sm-showtransitions tm-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 0))
              '(H 0 (i _ i _ i i _)))                                        

(define tm-rename-sts-WriteI (sm-rename-states (sm-states tm-WriteI) tm-WriteI))

(check-equal? (cdr (last (sm-showtransitions  tm-rename-sts-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 1)))
              '(1 (i i i _ i i _)))
(check-equal? (cdr (last (sm-showtransitions tm-rename-sts-WriteI `(i ,BLANK i ,BLANK i i ,BLANK) 0)))
              '(0 (i _ i _ i i _)))                                        

(define tm-RI (make-tm '(S H) 
                       '(i j k) 
                       (list
                        (list (list 'S 'i) (list 'H RIGHT))
                        (list (list 'S 'j) (list 'H RIGHT))
                        (list (list 'S 'k) (list 'H RIGHT))
                        (list (list 'S BLANK) (list 'H RIGHT)))
                       'S
                       '(H)))

(define tm-LI (make-tm '(S H) 
                       '(i j k) 
                       (list 
                        (list (list 'S 'i) (list 'H LEFT))
                        (list (list 'S 'j) (list 'H LEFT))
                        (list (list 'S 'k) (list 'H LEFT))
                        (list (list 'S BLANK) (list 'H LEFT)))
                       'S
                       '(H)))

(define tm-LB (combine-tms (list 0 tm-LI (list BRANCH 
                                               (list 'I (list GOTO 0))
                                               (list BLANK tm-RI tm-LI)
                                               (list 'add1 (list GOTO 0))
                                               (list 'sub1 (list GOTO 0)))) 
                           '(I add1 sub1)))

#|
(check-equal? (ctm-apply tm-LB `(I ,BLANK I I add1 I ,BLANK) 5) (tmconfig 'h 1 '(I _ I I add1 I _)))

;NOTE: WE NEED AN STARTEGY TO TEST TM CONFIGS 
|#


;;; rg tests

(define rg-a* (make-rg '(S) '(a) `((S -> ,EMP) (S -> aS)) 'S))

(check-equal? (last (grammar-derive rg-a* '())) EMP)
(check-equal? (last (grammar-derive rg-a* '(a))) (los->symbol '(a)))
(check-equal? (last (grammar-derive rg-a* '(a a a a a a))) (los->symbol '(a a a a a a)))

(define rg-renaments-a* ( grammar-rename-nts '(S) rg-a* ))

(check-equal? (last (grammar-derive rg-renaments-a* '())) EMP)
(check-equal? (last (grammar-derive rg-renaments-a* '(a))) (los->symbol '(a)))
(check-equal? (last (grammar-derive rg-renaments-a* '(a a a a a a))) (los->symbol '(a a a a a a)))

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
(check-equal? (last (grammar-derive rg-STARTS-WITH-aa '(a a b a b))) (los->symbol '(a a b a b)))
(check-equal? (grammar-derive rg-STARTS-WITH-aa '(b a a b a b)) "(b a a b a b) is not in L(G).")
(check-equal? (grammar-derive rg-STARTS-WITH-aa '(a b a b)) "(a b a b) is not in L(G).")
(check-equal? (grammar-derive rg-STARTS-WITH-aa '()) "() is not in L(G).")

(define rg-renaments-STARTS-WITH-aa ( grammar-rename-nts  '(A B C) rg-STARTS-WITH-aa))

(check-equal? (last (grammar-derive rg-renaments-STARTS-WITH-aa '(a a b a b))) (los->symbol '(a a b a b)))
(check-equal? (grammar-derive rg-renaments-STARTS-WITH-aa '(b a a b a b)) "(b a a b a b) is not in L(G).")
(check-equal? (grammar-derive rg-renaments-STARTS-WITH-aa '(a b a b)) "(a b a b) is not in L(G).")
(check-equal? (grammar-derive rg-renaments-STARTS-WITH-aa '()) "() is not in L(G).")


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

;(check-equal? (last (grammar-derive cfg-moreAs-than-Bs '(a b b a a)))
;              (los->symbol '(a b b a a)))
;(check-equal? (last (grammar-derive cfg-moreAs-than-Bs '(a b b b a a a a)))
;              (los->symbol '(a b b b a a a a)))
;(check-equal? (grammar-derive cfg-moreAs-than-Bs '(a b b a)) 
;              "(a b b a) is not in L(G)") ;!!!!Runs forever? 
;(check-equal? (grammar-derive cfg-moreAs-than-Bs '(a b b b)) 
;              "(a b b b) is not in L(G)") ;!!!!! Runs forever? 

(define cfg-moreBs-than-As (make-cfg '(S P)
                                     '(a b)
                                     `((S -> PbP)
                                       (P -> aPb)
                                       (P -> bPa)
                                       (P -> PP)
                                       (P -> b)
                                       (P -> ,EMP))
                                     'S))
;(check-equal? (last (grammar-derive cfg-moreBs-than-As '(a b b a b))) (los->symbol '(a b b a b)))
;(check-equal? (grammar-derive cfg-moreBs-than-As '(a b b a b a)) 
;              "(a b b a b a) is not in L(G)") ;runs forever? ! !!!!
;(check-equal? (grammar-derive cfg-moreBs-than-As '())  
;              "The word () is too short to test.") ;runs forever? ! !!!!

(define cfg-1B-before-anA (make-cfg '(S A)
                                    '(a b)
                                    `((S -> AbaA)
                                      (A -> aA)
                                      (A -> bA)
                                      (A -> ,EMP))
                                    'S))

(check-equal? (last (grammar-derive cfg-1B-before-anA '(a b a b))) (los->symbol '(a b a b)))
(check-equal? (grammar-derive cfg-1B-before-anA '(b b b)) "(b b b) is not in L(G).")
(check-equal? (last (grammar-derive cfg-1B-before-anA '(b b b a))) (los->symbol '(b b b a)))
(check-equal? (grammar-derive cfg-1B-before-anA '()) "The word () is too short to test.")

;{w| w contains at least three a's}
(define cfg-test1 (make-cfg '(S X)
                            '(a b)
                            `((S -> XaXaXaX)
                              (X -> bX)
                              (X -> aX)
                              (X -> ,EMP))
                            'S))
(check-equal? (grammar-derive cfg-test1 '()) "The word () is too short to test.")
(check-equal? (grammar-derive cfg-test1 '(b b b a b a b)) "(b b b a b a b) is not in L(G).")
(check-equal? (last (grammar-derive cfg-test1 '(a a a))) (los->symbol '(a a a)))
(check-equal? (last (grammar-derive cfg-test1 '(b b b a b a b a))) (los->symbol '(b b b a b a b a)))

;{w| w starts and ends with the same symbol}
(define cfg-test2 (make-cfg '(S X)
                            '(a b)
                            `((S -> aXa)
                              (S -> bXb)
                              (X -> aX)
                              (X -> bX)
                              (X -> ,EMP))
                            'S))

(check-equal? (last (grammar-derive cfg-test2 '(b b b a b a b a b))) (los->symbol '(b b b a b a b a b)))
(check-equal? (last (grammar-derive cfg-test2 '(a b b b a b a b a))) (los->symbol '(a b b b a b a b a))) ;!! dont understand this error
(check-equal? (last (grammar-derive cfg-test2 '(a b a))) (los->symbol '(a b a)))
(check-equal? (grammar-derive cfg-test2 '(b b b a b a b a)) "(b b b a b a b a) is not in L(G).")

;{w| the length of w is odd}
(define cfg-test3 (make-cfg '(S P)
                            '(a b)
                            `((S -> aP)
                              (S -> bP)
                              (P -> ,EMP)
                              (P -> aS)
                              (P -> bS))
                            'S))

(check-equal? (grammar-derive cfg-test3 '()) "The word () is too short to test.")
(check-equal? (grammar-derive cfg-test3 '(a b)) "(a b) is not in L(G).")
(check-equal? (last (grammar-derive cfg-test3 '(a b a))) (los->symbol '(a b a)))
(check-equal? (last (grammar-derive cfg-test3 '(a b a b a))) (los->symbol '(a b a b a)))
(check-equal? (grammar-derive cfg-test3 '(a)) "The word (a) is too short to test.")
(check-equal? (grammar-derive cfg-test3 '(b)) "The word (b) is too short to test.")

;{w| the length of w is odd and its middle symbol is an "a"}
(define cfg-test4 (make-cfg '(S)
                            '(a b)
                            '((S -> a)
                              (S -> aSb)
                              (S -> aSa)
                              (S -> bSa)
                              (S -> bSb))
                            'S))
(check-equal? (grammar-derive cfg-test4 '(a)) "The word (a) is too short to test.")
(check-equal? (last (grammar-derive cfg-test4 '(b a b))) (los->symbol '(b a b)))
(check-equal? (last (grammar-derive cfg-test4 '(b b a a a))) (los->symbol '(b b a a a)))
(check-equal? (grammar-derive cfg-test4 '(b b b a a a)) "(b b b a a a) is not in L(G)." )
(check-equal? (grammar-derive cfg-test4 '()) "The word () is too short to test.")
;{w| w = w reverse, that is, w is a plindrome} 
(define cfg-test5 (make-cfg '(S)
                            '(a b)
                            `((S -> ,EMP)
                              (S -> a)
                              (S -> b)
                              (S -> bSb)
                              (S -> aSa))
                            'S))

(check-equal? (grammar-derive cfg-test5 '()) "The word () is too short to test.")
(check-equal? (grammar-derive cfg-test5 '(a)) "The word (a) is too short to test.")
(check-equal? (grammar-derive cfg-test5 '(b)) "The word (b) is too short to test.")
(check-equal? (last (grammar-derive cfg-test5 '(a b a))) (los->symbol '(a b a)))
(check-equal? (last (grammar-derive cfg-test5 '(b a b))) (los->symbol '(b a b)))
(check-equal? (last (grammar-derive cfg-test5 '(a a b b a a))) (los->symbol '(a a b b a a)))
(check-equal? (grammar-derive cfg-test5 '( b a))"(b a) is not in L(G).")

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

(check-equal? (grammar-derive cfg-test6 '(a c c)) "(a c c) is not in L(G).") ;RUNS FOREVER???!!! 
(check-equal? (last (grammar-derive cfg-test6 '(a b))) (los->symbol '(a b)))
(check-equal? (last (grammar-derive cfg-test6 '( a a b b c c))) (los->symbol '( a a b b c c)))
(check-equal? (last (grammar-derive cfg-test6 '( a b c c))) (los->symbol '( a b c c)))
(check-equal? (last (grammar-derive cfg-test6 '( a b b c c))) (los->symbol '( a b b c c)))


;;; csg tests

(define CSG-an-bn (make-csg '(S) 
                            '(a b) 
                            (list (list 'S ARROW EMP) 
                                  (list 'aSb ARROW 'aaSbb) 
                                  (list 'S ARROW 'aSb)) 
                            'S))

(define CSG-an-bn-cn (make-csg '(S A B C G H I) 
                               '(a b c) 
                               `( (S -> ABCS) (S -> G)
                                              (BA -> AB) (CA -> AC) (CB -> BC)
                                              (CG -> Gc) (G -> H) 
                                              (BH -> Hb) (H -> I)
                                              (AI -> Ia) (I -> ,EMP)) 
                               'S))

(check-equal? (last (grammar-derive CSG-an-bn-cn '())) (los->symbol (list EMP)))
(check-equal? (last (grammar-derive CSG-an-bn-cn '(a b c))) (los->symbol '(a b c)))
(check-equal? (last (grammar-derive CSG-an-bn-cn '(a a b b c c))) (los->symbol '(a a b b c c)))

(define CSG-an-bn-an-bn-cn (grammar-concat CSG-an-bn CSG-an-bn-cn))

(check-equal? (last (grammar-derive CSG-an-bn-an-bn-cn '())) 
              EMP)
(check-equal? (last (grammar-derive CSG-an-bn-an-bn-cn '(a a b b a b c)))
              'aabbabc)

;NOTE WE DONT CHECK FOR NOT IN THE GRAMMAR BCZ IT MAY NEVER END
(define RENAME-CSG-an-bn-cn (grammar-rename-nts (grammar-nts CSG-an-bn-cn) 
                                                CSG-an-bn-cn))

(check-equal? (last (grammar-derive RENAME-CSG-an-bn-cn '())) (los->symbol (list EMP)))
(check-equal? (last (grammar-derive RENAME-CSG-an-bn-cn '(a b c))) (los->symbol '(a b c)))
(check-equal? (last (grammar-derive RENAME-CSG-an-bn-cn '(a a b b c c))) (los->symbol '(a a b b c c)))

;; Tests for fsa->regexp

; L(KLEENESTAR-abUaba) = (abUaba)*
;; does not require new start and final states
(define KLEENESTAR-abUaba (make-ndfa '(S A B C D E U V)
                                     '(a b)
                                     'V
                                     '(U)
                                     `((V ,EMP S)
                                       (S ,EMP U)
                                       (S a A)
                                       (A b B)
                                       (B a C)
                                       (C ,EMP S)
                                       (S a D)
                                       (D b E)
                                       (E ,EMP S))))

; L(KLEENESTAR-abUaba) = (abUaba)*
;; requires new start and final states
(define KLEENESTAR-abUaba2 (make-ndfa '(S A B C D E)
                                      '(a b)
                                      'S
                                      '(S)
                                      `((S a A)
                                        (A b B)
                                        (B a C)
                                        (C ,EMP S)
                                        (S a D)
                                        (D b E)
                                        (E ,EMP S))))

; L = a*
(define a* (make-ndfa '(S)
                      '(a b)
                      'S
                      '(S)
                      `((S a S))))

; L = a*b*
(define a*b* (make-ndfa '(S F)
                        '(a b)
                        'S
                        '(F)
                        `((S a S)
                          (S ,EMP F)
                          (F b F))))

; L = (aab)*
(define aab* (make-ndfa '(S A B)
                        '(a b)
                        'S
                        '(S)
                        `((S a A)
                          (A a B)
                          (B b S))))

(check-equal? (fsa->regexp KLEENESTAR-abUaba)
              "(aba(aba)* U (ε U (a U aba(aba)*a)b((a U aba(aba)*a)b)*(ε U aba(aba)*)))")

(check-equal? (fsa->regexp KLEENESTAR-abUaba2)
              "(aba(aba)* U (ε U (a U aba(aba)*a)b((a U aba(aba)*a)b)*(ε U aba(aba)*)))")

(check-equal? (fsa->regexp a*) "a*")

(check-equal? (fsa->regexp a*b*) "a*b*")

(check-equal? (fsa->regexp aab*) "(ε U aa(baa)*b)")


(define regexp-inter-a*-b* (fsa->regexp ndfa-inter-a*-b*)) 
(check-equal? regexp-inter-a*-b* (symbol->string EMP))

(define regexp-a*bab* (fsa->regexp ndfa-a*bab*))
(check-equal? regexp-a*bab* "a*bab*")

(define regexp-nota*Unotb* (fsa->regexp nota*Unotb*)) 
(check-equal? regexp-nota*Unotb* "(a*b(b U a)* U b*a(b U a)*)")




  ) ;; end module+ test 
