#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "viz-state.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")


;; check-accept M w [n] - returns #t if machine accepts, #f if rejects
(define-syntax (check-accept stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (check-equal? (sm-apply M w) 'accept)
           #t
           #f)]
    [(_ M w n)
     #'(if (string? (tm-apply M w n))
           #f
           #t)]))


;; check-reject M w [n] - returns #t if machine accepts, #f if rejects
(define-syntax (check-reject stx)
  (syntax-parse stx
    [(_ M w)
     #'(if (check-equal? (sm-apply M w) 'accept)
           #f
           #t)]
    [(_ M w n)
     #'(if (check-equal? (not (string? (tm-apply M w n))) #t)
           #f
           #t)]))


;; TESTING

    
(define AB*B*UAB*
  (make-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

;; Write b

;; PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in {a b BLANK}*
;;      AND s in (a b BLANK)
;; POST: tape = (LM w) AND i=k AND tape[i]=b
(define Wb (make-tm '(S H)
                    '(a b)
                    `(((S a) (H b))
                      ((S b) (H b))
                      ((S ,BLANK) (H b)))
                    'S
                    '(H)))

;; Tests for Wb
(check-equal? (last (sm-showtransitions Wb `(,LM ,BLANK ,BLANK) 2))
              `(H 2 (,LM ,BLANK b)))
(check-equal? (last (sm-showtransitions Wb `(,LM b b b b) 3))
              `(H 3 (,LM b b b b)))