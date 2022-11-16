#lang racket
(require "../../main.rkt" "../test-helpers.rkt")


(define R (make-tm '(S H)
                   `(a b ,LM)
                   `(;((S ,LM) (S ,RIGHT))
                     ((S a) (H ,RIGHT))
                     ((S b) (H ,RIGHT))
                     ((S ,BLANK) (H ,RIGHT)))
                   'S
                   '(H)))

(define L (make-tm '(S H)
                   `(a b ,LM)
                   `(;((S ,LM) (S ,RIGHT))
                     ((S a) (H ,LEFT))
                     ((S b) (H ,LEFT))
                     ((S ,BLANK) (H ,LEFT)))
                   'S
                   '(H)))

(define HALT (make-tm '(S)
                      `(a b)
                      `()
                      'S
                      '(S)))

(define Mblank (make-tm '(S H)
                        `(a b ,LM)
                        `(;((S ,LM) (S ,RIGHT))
                          ((S a) (H ,BLANK))
                          ((S b) (H ,BLANK))
                          ((S ,BLANK) (H ,BLANK)))
                        'S
                        '(H)))

(define FBL (combine-tms (list 0 L (cons BRANCH
                                         (list (list 'a (list GOTO 0))
                                               (list 'b (list GOTO 0))
                                               (list LM (list GOTO 0))
                                               (list BLANK ))))
                         (list 'a 'b LM)))

(define FBR (combine-tms (list 0 R (cons BRANCH
                                         (list (list 'a (list GOTO 0))
                                               (list 'b (list GOTO 0))
                                               (list LM (list GOTO 0))
                                               (list BLANK ))))
                         (list 'a 'b)))

(define SHIFTR (combine-tms
                (list FBR
                      100
                      L
                      (cons BRANCH (list (list BLANK (list GOTO 500))
                                         (list 'a (list GOTO 300))
                                         (list 'b (list GOTO 300))
                                         (list LM R (list GOTO 500))))
                      300
                      (list (list VAR 'i)
                            Mblank
                            R
                            'i
                            FBL
                            (list GOTO 100))
                      500
                      FBR)
                `(a b ,LM)))





(module+ test
  (require rackunit)

  (define error-checks
    (test-suite "Makes sure invalid machines fail gracefully"
                
                (test-case "Empty machine"
                           (check-exn
                            exn:fail?
                            (lambda () (sm-visualize 'ctm))))
                
                (test-case "Premade machine - no invariants"
                           (check-exn
                            exn:fail?
                            (lambda () (sm-visualize SHIFTR))))

                (test-case "Premade machine - with invariants"
                           (check-exn
                            exn:fail?
                            (lambda () (sm-visualize SHIFTR
                                                     (lambda (v) #f)
                                                     (lambda (v) #t)))))))
              

  (test-all 'verbose
            (error-checks))

  ) ;; end module+ test