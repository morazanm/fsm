#lang racket
(require "../../fsm-core/interface.rkt")
(require rackunit)

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
