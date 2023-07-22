#lang fsm

(require "../../fsm-core/interface.rkt" "lib.rkt")
(require 2htdp/universe rackunit)
(require (rename-in racket/gui/base
                    [make-color loc-make-color]
                    [make-pen loc-make-pen]))
(require 2htdp/image)

(define FNAME "fsm")

;; L = ab*
(define ab* (make-ndfa '(S A)
                       '(a b)
                       'S
                       '(A)
                       '((S a A)
                         (A b A))))
;; L = a(a U ab)b*
(define a-aUb-b* (make-ndfa '(Z H B C D F)
                            '(a b)
                            'Z
                            '(F)
                            `((Z a H)
                              (Z a B)
                              (H a D)
                              (D ,EMP F)
                              (B a C)
                              (C b F)
                              (F b F))))
;; L = aab*
(define aab* (make-ndfa '(W X Y)
                        '(a b)
                        'W
                        '(Y)
                        '((W a X)
                          (X a Y)
                          (Y b Y))))
;; L = a*
(define a* (make-dfa '(S D)
                     '(a b)
                     'S
                     '(S)
                     '((S a S)
                       (S b D)
                       (D a D)
                       (D b D))
                     'no-dead))


;; ndfa ndfa → ndfa
;; Purpose: Construct ndfa for the union of the languages of the
;;          given ndfas
;; Assume: The intersection of the states of the given machines is empty
(define (union-fsa M N)
  (let* [(new-start (generate-symbol
                     'S (append (sm-states M) (sm-states N))))
         (new-sigma (remove-duplicates
                     (append (sm-sigma M) (sm-sigma N))))
         (new-states (cons new-start
                           (append (sm-states M) (sm-states N))))
         (new-finals (append (sm-finals M) (sm-finals N)))
         (new-rules (append (list (list new-start EMP (sm-start M))
                                  (list new-start EMP (sm-start N)))
                            (sm-rules M)
                            (sm-rules N)))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))


;; Tests for union-fsa
(define ab*Ua-aUb-b* (union-fsa ab* a-aUb-b*))
(define ab*Uaab* (union-fsa ab* aab*))


(check-equal? (sm-apply ab*Ua-aUb-b* '()) 'reject)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a a a a)) 'reject)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a b)) 'accept)
(check-equal? (sm-apply ab*Ua-aUb-b* '(a a b b)) 'accept)
(check-equal? (sm-testequiv? ab*Ua-aUb-b* (sm-union ab* ab*Ua-aUb-b*))
              #t)
(check-equal? (sm-apply ab*Uaab* '(a a a)) 'reject)
(check-equal? (sm-apply ab*Uaab* '(b a b a)) 'reject)
(check-equal? (sm-apply ab*Uaab* '(a b b)) 'accept)
(check-equal? (sm-apply ab*Uaab* '(a a b)) 'accept)
(check-equal? (sm-apply ab*Uaab* '(a b b b b)) 'accept)
(check-equal? (sm-testequiv? ab*Uaab* (sm-union ab* aab*)) #t)


;; ndfa ndfa → ndfa
;; Purpose: Construct ndfa for the concatenation of the languages of the
;;          given ndfas
;; Assume: The intersection of the states of the given machines is empty
(define (concat-fsa M N)
  (let* [(new-start (sm-start M))
         (new-sigma (remove-duplicates (append (sm-sigma M) (sm-sigma N))))
         (new-states (append (sm-states M) (sm-states N)))
         (new-finals (sm-finals N))
         (new-rules (append (sm-rules M)
                            (sm-rules N)
                            (map (λ (f) (list f EMP (sm-start N)))
                                 (sm-finals M))))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))


;; Tests for concat-fsa
(define ab*-o-a-aUb-b* (concat-fsa ab* a-aUb-b*))
(define ab*-o-aab* (concat-fsa ab* aab*))


(check-equal? (sm-apply ab*-o-a-aUb-b* '()) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(b b b)) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a a b a b)) 'reject)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a b a a b)) 'accept)
(check-equal? (sm-apply ab*-o-a-aUb-b* '(a b b b a a)) 'accept)
(check-equal? (sm-testequiv? ab*-o-a-aUb-b* (sm-concat ab* a-aUb-b*)) #t)
(check-equal? (sm-apply ab*-o-aab* '()) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a b a)) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a a b b a a)) 'reject)
(check-equal? (sm-apply ab*-o-aab* '(a b b a a b b)) 'accept)
(check-equal? (sm-apply ab*-o-aab* '(a a a)) 'accept)
(check-equal? (sm-testequiv? ab*-o-aab* (sm-concat ab* aab*)) #t)



;; ndfa → ndfa
;; Purpose: Construct ndfa for the Kleene star of given ndfa’s language
(define (kstar-fsa M)
  (let* [(new-start (generate-symbol 'K (sm-states M))) (new-sigma (sm-sigma M))
                                                        (new-states (cons new-start (sm-states M)))
                                                        (new-finals (cons new-start (sm-finals M)))
                                                        (new-rules (cons (list new-start EMP (sm-start M))
                                                                         (append (sm-rules M)
                                                                                 (map (λ (f) (list f EMP new-start))
                                                                                      (sm-finals M)))))]
    (make-ndfa new-states new-sigma new-start new-finals new-rules)))

;; Tests for kstar-fsa
(define a-aUb-b*-* (kstar-fsa a-aUb-b*))
(define ab*-* (kstar-fsa ab*))


(check-equal? (sm-apply a-aUb-b*-* '(b b b)) 'reject)
(check-equal? (sm-apply a-aUb-b*-* '(a b a b a a a a)) 'reject)
(check-equal? (sm-apply a-aUb-b*-* '()) 'accept)
(check-equal? (sm-apply a-aUb-b*-* '(a a a a b b b b)) 'accept)
(check-equal? (sm-apply a-aUb-b*-* '(a a b a a b b a a)) 'accept)
(check-equal? (sm-testequiv? a-aUb-b*-* (sm-kleenestar a-aUb-b*)) #t)
(check-equal? (sm-apply ab*-* '(b)) 'reject)
(check-equal? (sm-apply ab*-* '(b b b)) 'reject)
(check-equal? (sm-apply ab*-* '()) 'accept)
(check-equal? (sm-apply ab*-* '(a a a a)) 'accept)
(check-equal? (sm-apply ab*-* '(a b a b b a b b b)) 'accept)
(check-equal? (sm-testequiv? ab*-* (sm-kleenestar ab*)) #t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; regexp alphabet → ndfa
;; Purpose: Build an ndfa for the given regexp
(define (regexp->ndfa e sigma)
  (let* [(simple-tbl (map
                      (λ (a)
                        (let [(S (generate-symbol 'S '(S)))
                              (A (generate-symbol 'A '(A)))]
                          (list a (make-ndfa (list S A)
                                             sigma
                                             S
                                             (list A)
                                             (list (list S a A))))))
                      (cons EMP sigma)))]
    (cond [(empty-regexp? e) (second (assoc EMP simple-tbl))]
          [(singleton-regexp? e)
           (second (assoc (string->symbol (singleton-regexp-a e))
                          simple-tbl))]
          [(concat-regexp? e)
           (concat-fsa (regexp->ndfa (concat-regexp-r1 e) sigma)
                       (regexp->ndfa (concat-regexp-r2 e) sigma))]
          [(union-regexp? e)
           (union-fsa (regexp->ndfa (union-regexp-r1 e) sigma)
                      (regexp->ndfa (union-regexp-r2 e) sigma))]
          [else (kstar-fsa (regexp->ndfa (kleenestar-regexp-r1 e) sigma))])))
