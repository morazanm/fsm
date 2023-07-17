(module predicates racket
  (require
    "../../tm.rkt"
    "../../../../main.rkt"
    )
  (provide
   check-input-tm
   correct-members-tm?
   valid-tm-rule?
   )

  (define (check-input-tm states
                          sigma
                          start
                          finals
                          rules
                          accept
                          accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-tm states
                                              sigma
                                              rules
                                              start
                                              finals
                                              accept))
      (andmap (lambda (x) (equal? (sm-apply temp-machine x) accepts?)) words)
      )
    )
  
  (define (correct-members-tm? states sigma rules)
    (andmap (lambda (x) (and (member (car (car x)) states)
                             (member (cadr (car x)) sigma)
                             (member (car (cadr x)) states)
                             (member (cadr (cadr x)) sigma))) rules))

  (define (valid-tm-rule? rule)
    (and (list? rule)
         (= (length rule) 2)
         (= (length (car rule)) 2)
         (= (length (cadr rule)) 2))
    )
  )