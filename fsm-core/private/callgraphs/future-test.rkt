#lang racket
(require "../../private/tm.rkt"
         "../../private/macros/constructors.rkt"
         "../../private/constants.rkt"
         )
(provide COPY COPYL2)


(define/contract (make-tm states sigma rules start finals
                             [accept 'null]
                             #:accepts [accepts '()]
                             #:rejects [rejects '()]
                             )
    make-tm/c
    (if (equal? accept 'null)
        (make-unchecked-tm states sigma rules start finals)
        (make-unchecked-tm states sigma rules start finals accept))
    )
(define R (make-tm '(S F)
                   '(a b d)
                   `(((S a) (F ,RIGHT))
                     ((S b) (F ,RIGHT))
                     ((S d) (F ,RIGHT))
                     ((S ,BLANK) (F ,RIGHT)))
                   'S
                   '(F)))
(define L (make-tm '(S H)
                   '(a b d)
                   `(((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S d) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))
(define FBL (combine-tms
             (list 0
                   L
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 1))
                               (list LM (list GOTO 0))))
                   1)
             (list 'a 'b 'd)))


(define WB (make-tm '(S H)
                    '(a b d)
                    `(((S a) (H ,BLANK))
                      ((S b) (H ,BLANK))
                      ((S d) (H ,BLANK))
                      ((S ,BLANK) (H ,BLANK)))
                    'S
                    '(H)))
(define FBR (combine-tms
             (list 0
                   R
                   (cons BRANCH
                         (list (list 'a (list GOTO 0))
                               (list 'b (list GOTO 0))
                               (list 'd (list GOTO 0))
                               (list BLANK (list GOTO 10))))
                   10)
             (list 'a 'b 'd)))
(define RR (combine-tms (list R R) '(a b d)))
(define COPY (combine-tms
              (list FBL
                    0
                    R
                    (cons BRANCH (list (list BLANK (list GOTO 2))
                                       (list 'a (list GOTO 1))
                                       (list 'b (list GOTO 1))))
                    1
                    (list (list VAR 'k)
                          WB
                          FBR
                          FBR
                          'k
                          FBL
                          FBL
                          'k
                          (list GOTO 0))
                    2
                    FBR
                    L
                    (cons BRANCH (list (list BLANK (list GOTO 3))
                                       (list 'a (list GOTO 4))
                                       (list 'b (list GOTO 4))))
                    3
                    RR
                    (list GOTO 5)
                    4
                    R
                    (list GOTO 5)
                    5)
              '(a b)))
(define COPYL2
  '(FBL
    0
    R
    (cons BRANCH (list (list _ (list GOTO 2)) (list 'a (list GOTO 1)) (list 'b (list GOTO 1))))
    1
    (list (list VAR 'k) WB FBR FBR k FBL FBL k (list GOTO 0))
    2
    FBR
    L
    (cons BRANCH (list (list _ (list GOTO 3)) (list 'a (list GOTO 4)) (list 'b (list GOTO 4))))
    3
    RR
    (list GOTO 5)
    4
    R
    (list GOTO 5)
    5))