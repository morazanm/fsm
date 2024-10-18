#lang racket

(require (for-syntax syntax/parse
                     racket/base
                     "viz-state.rkt"
                     racket/struct-info)
         2htdp/universe
         2htdp/image
         "../../fsm-core/private/sm-apply.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/csg.rkt"
         "../../fsm-core/private/cfg.rkt"
         "../../fsm-core/private/regular-grammar.rkt"
         "viz-state.rkt"
         "check-derive-not-derive-macros.rkt"
         "check-accept-reject-macro.rkt"
         rackunit
         "../../fsm-core/interface.rkt"
         "default-viz-functions.rkt")




;; TESTING MACHINES

    
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


(check-accept Wb `(,LM ,BLANK b) 2)
(check-reject Wb `(,LM ,BLANK b) 2)


;; TESTING GRAMMARS

(define even-bs-odd-as
  (make-unchecked-cfg '(S A B C)
                      '(a b)
                      `((S ,ARROW aA) (S ,ARROW bB)
                                      (S ,ARROW a)
                                      (A ,ARROW aS)
                                      (A ,ARROW bC)
                                      (B ,ARROW aC)
                                      (B ,ARROW bS)
                                      (C ,ARROW aB)
                                      (C ,ARROW bA)
                                      (C ,ARROW b))
                      'S))


(check-derive even-bs-odd-as '(b b b b a a a))

(check-not-derive even-bs-odd-as '(b b b b a a a))

