#lang racket

(require "../../../main.rkt")

(define empty-tran (make-ndfa '(A)
                              '(a b)
                              'A
                              '(A)
                              `((A ,EMP A))))

;; ndfa2dfa works

(define single-tran (make-ndfa '(A)
                               '(a b)
                               'A
                               '(A)
                               `((A a A))))

;; ndfa2dfa works

(define double-loop (make-ndfa '(A)
                               '(a b)
                               'A
                               '(A)
                               `((A a A) (A b A))))
;; ndfa2dfa works

(define empties-weird (make-ndfa '(A B)
                                 '(a)
                                 'A
                                 '(A B)
                                 `((A ,EMP B) (B ,EMP A))))

;; ndfa2dfa works

(define AA (make-ndfa '(A B)
                      '(a)
                      'A
                      '(B)
                      `((A a B)
                        (A a A)
                        (B a A))))

;; ndfa2dfa works

(define WWW (make-ndfa '(W E O)
                       '(a b c f)
                       'W
                       '(O)
                       `((W a E)
                         (W b O)
                         (E a E)
                         (E f O)
                         (O c O)
                         (O a W))))

;; ndfa2dfa works

(define ABBCCD (make-ndfa '(S L P K)
                          '(a b c d)
                          'S
                          '(L P K)
                          `((S a L)
                            (S b P)
                            (S c K)
                            (L b L)
                            (P c P)
                            (K d K))))

;; ndfa2dfa works

(define another-ndfa (make-ndfa '(M X C Z)
                                '(l k j)
                                'M
                                '(X C Z)
                                `((M l X)
                                  (M l C)
                                  (M l Z)
                                  (X k X)
                                  (X ,EMP Z)
                                  (C j C))))

;; L(M) = ab*
(define M (make-dfa `(S F ,DEAD)
                    '(a b)
                    'S
                    '(F)
                    `((S a F)
                      (S b ,DEAD)
                      (F a ,DEAD)
                      (F b F)
                      (,DEAD a ,DEAD)
                      (,DEAD b ,DEAD))
                    'no-dead))

;; L(M2) = abb*
(define M2 (make-dfa `(S A F ,DEAD)
                     '(a b)
                     'S
                     '(F)
                     `((S a A)
                       (S b ,DEAD)
                       (A a ,DEAD)
                       (A b F)
                       (F a ,DEAD)
                       (F b F))))

;; L = {e} ∪ aa* ∪ ab*
(define LNDFA (make-ndfa
               '(S A B F)
               '(a b)
               'S
               '(A B F)
               `((S a A)
                 (S a B)
                 (S ,EMP F)
                 (A b A)
                 (B a B))))

(define NO-ABAA
  (make-dfa
   '(S A B C R)
   '(a b)
   'S
   '(S A B C)
   '((S a A) (S b S)
             (A a A) (A b B)
             (B a C) (B b S)
             (C a R) (C b B)
             (R a R) (R b R))
   'no-dead))

(define EVEN-A-ODD-B (make-dfa '(S M N P)
                               '(a b)
                               'S
                               '(N)
                               '((S a P)
                                 (S b N)
                                 (M a N)
                                 (M b P)
                                 (N a M)
                                 (N b S)
                                 (P a S)
                                 (P b M))
                               'no-dead))

;; L = e U aa* U ab*
(define LNDFA1 (make-ndfa
                `(S A B F ,DEAD)
                '(a b)
                'S
                '(A B F)
                `((S a A)
                  (S a B)
                  (S ,EMP F)
                  (A b A)
                  (B a B)
                  (S b ,DEAD)
                  (A a ,DEAD)
                  (B b ,DEAD)
                  (F a ,DEAD)
                  (F b ,DEAD))))

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

;; ndfa2dfa works

(define ND
  (make-ndfa
   '(S A B C D E)
   '(a b)
   'S
   '(S)
   `((S a A)
     (S a B)
     (A b C)
     (B b D)
     (C a E)
     (D ,EMP S)
     (E ,EMP S))))