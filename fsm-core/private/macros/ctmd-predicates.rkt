; FSM Library Version 1.0
; Copyright (C) 2020 by Marco T. Morazan
; Written by: Marco T. Morazan

(module ctmd-predicates racket 
  (require "../constants.rkt" "../sm-getters.rkt" "../../interface.rkt")

  (local-require test-engine/racket-tests)
                               
  ;;  PRE: tape = (LMw) AND i=k>0 AND w in (a b)*
  ;; POST: tape = (LMw) AND i=k+1 AND w in (a b)*
  (define R (make-tm '(S F)
                     '(a b)
                     `(((S a) (F ,RIGHT))
                       ((S b) (F ,RIGHT))
                       ((S ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))


  ;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
  ;; POST: tape = (LMw) AND i=k+1
  (define L (make-tm '(S H)
                     `(a b)
                     `(((S a) (H ,LEFT))
                       ((S b) (H ,LEFT))
                       ((S ,BLANK) (H ,LEFT)))
                     'S
                     '(H)))


  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
  ;; POST: tape = (LM w) AND i=k+2 AND w in (a b BLANK)*
  (define RR (combine-tms (list R R) '(a b)))



  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)* AND exists j<i s.t. tape[j]=BLANK
  ;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
  (define FBL (combine-tms (list 0 L (cons BRANCH
                                           (list (list 'a (list GOTO 0))
                                                 (list 'b (list GOTO 0))
                                                 (list LM (list GOTO 0))
                                                 (list BLANK ))))
                           (list 'a 'b LM)))


  ;; Infinite recursion, PRE not met
  ;;    (ctm-run FBL `(,LM a a b) 3)

  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
  ;; POST: tape = (LM w) AND i>k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
  (define FBR (combine-tms (list 0 R (cons BRANCH
                                           (list (list 'a (list GOTO 0))
                                                 (list 'b (list GOTO 0))
                                                 (list LM (list GOTO 0))
                                                 (list BLANK ))))
                           (list 'a 'b LM)))

  ;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (a b BLANK)* AND s in (a b BLANK)
  ;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
  (define WB (make-tm '(S H)
                      `(a b)
                      `(((S a) (H ,BLANK))
                        ((S b) (H ,BLANK))
                        ((S ,BLANK) (H ,BLANK)))
                      'S
                      '(H)))


  ;;  PRE: tape = (LM BLANK w BLANK) and head on blank after w
  ;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w
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
                `(a b)))
  
  (define (gather-labels input)
    (filter number? input))
  

  ;;;;;;; empty list
  ;; (cons m ctmd) <-- m is a tm or ctmd
  ;;;;;;; (cons LABEL ctmd) <-- labels are numbers
  ;;;;;;; (cons (list GOTO LABEL) ctmd)
  ;;;;;;; (cons (list BRANCH (listof (list symbol (list GOTO LABEL)))) ctmd)
  ;; (cons (list (list VAR symbol) ctmd) ctmd)
  ;; (cons variable ctmd)

  (define (in-labels? num labels)
    (if (member num labels) #t #f))

  ;;takes as input a list
  ;;must be a valid list of 2
  ;; starting with the symbol GOTO
  ;; ending with a valid label
  (define (valid-goto? input labels)
    (and (list? input)
         (= (length input) 2)
         (equal? (car input) GOTO)
         (in-labels? (cadr input) labels)))

  (check-expect (valid-goto? `(,GOTO 10) '(10)) #t)
  (check-expect (valid-goto? `(,GOTO 'a) '()) #f)

  ;;invalid-branches: takes as input a list of items in a branch expression
  ;; these must all be valid branches of the format
  ;;   list of 2
  ;;   starting with a symbol
  ;;   ending with a valid GOTO expression
  (define (invalid-branches input labels sigma)
    (filter (lambda (x) (not (and (list? x)
                                  (= (length x) 2)
                                  (member (car x) sigma)
                                  (valid-goto? (cadr x) labels)))) input))

  (check-expect (invalid-branches `((a (,GOTO 10))
                                    (b (,GOTO 15))) '(10 15) '(a b)) '())
  (check-expect (invalid-branches `((10 (,GOTO 10))
                                    (b (,GOTO 15))) '(10 15) '(a b)) `((10 (,GOTO 10))))
  (check-expect (invalid-branches `((a (,BRANCH 10))
                                    (b (,GOTO 15))) '(10 15) '(a b)) `((a (,BRANCH 10))))
  (check-expect (invalid-branches `((a (,GOTO 10))
                                    (b (,GOTO 20))) '(10 20) '(a)) `((b (,GOTO 20)))) 

  ;;valid-list-case: takes the first portion of a ctmd, if that portion is a list
  ;;it must either be
  ;;    a valid GOTO expression
  ;;    a valid BRANCH expression
  ;;    a valid VAR declaration followed by a valid CTMD
  ;;    a full valid ctmd or valid tm
  (define (valid-list-case? input labels sigma variables)
    (cond [(equal? GOTO (car input)) (if (not (= (length input) 2)) "A GOTO expression must be of length 2"
                                         (if (in-labels? (cadr input) labels) #t
                                             "The second part of GOTO must be a label that exists in your machine"))] ;;; must be an existing label
          [(equal? BRANCH (car input)) (if (not (= (length input) 2)) "A BRANCH expression must be of length 2"
                                           (if (not (list? (cadr input))) "The second part of a BRANCH must be a list"
                                               (let [(all-invalid-branches (invalid-branches (cadr input) labels sigma))]
                                                 (if (empty? all-invalid-branches) #t
                                                     (format "The following branches have errors: ~a" all-invalid-branches)))))]
          [(and (list? (car input))
                (equal? (car (car input)) VAR)) (if (not (= (length (car input)) 2))
                                                    "A VAR declaration must be of length 2"
                                                    (if (not (symbol? (cadr (car input))))
                                                        "The second part of a VAR declaration must be a symbol"
                                                        (valid-ctmd? (cadr input) labels sigma (cons (cadr (car input)) variables))))]
          [else (valid-ctmd? input labels sigma variables)]
          ))

  (check-expect (valid-list-case? `(,GOTO 10) '(10) '() '()) #t)
  (check-expect (valid-list-case? `(,GOTO 20 30) '(20) '() '()) "A GOTO expression must be of length 2")
  (check-expect (valid-list-case? `(,GOTO 'a) '() '() '()) "The second part of GOTO must be a label that exists in your machine")

  (check-expect (valid-list-case? `(,BRANCH ((a (,GOTO 10))
                                             (b (,GOTO 20)))) '(10 20) '(a b) '())
                #t)
  (check-expect (valid-list-case? `(,BRANCH (a (,GOTO 10))
                                            (b (,GOTO 20))) '(10 20) '(a b) '())
                "A BRANCH expression must be of length 2")
  (check-expect (valid-list-case? `(,BRANCH a) '() '(a) '())
                "The second part of a BRANCH must be a list")
  (check-expect (valid-list-case? `(,BRANCH ((a (,GOTO a))
                                             (b (,GOTO 20)))) '(20) '(a b) '())
                "The following branches have errors: ((a (GOTO a)))")
  (check-expect (valid-list-case? `((,VAR a) (b)) '() '() '())
                "The variable being referenced is not defined in this scope.")

  ; valid-variable?: symbol (list of symbols) -> boolean
  ; Purpose: Returns true if the variable is in the given list, and false otherwise.
  (define (valid-variable? variable variables)
    (if (member variable variables) #t #f))

  ;; may need accumulator for variables defined in scope
  (define (valid-ctmd? input labels sigma variables)
    (cond [(empty? input) #t]
          [(list? (car input)) (let [(list-case (valid-list-case? (car input) labels sigma variables))]
                                 (if (string? list-case) list-case
                                     (valid-ctmd? (cdr input) labels sigma variables)))]
          [(number? (car input)) (valid-ctmd? (cdr input) labels sigma variables)]
          [(symbol? (car input)) (if (not (valid-variable? (car input) variables))
                                     "The variable being referenced is not defined in this scope."
                                     (valid-ctmd? (cdr input) labels sigma variables))] ;; is symbol in accumulated list
          [(procedure? (car input)) (if (or (equal? (sm-type (car input)) 'tm)
                                            (equal? (sm-type (car input)) 'tm-language-recognizer))
                                        (valid-ctmd? (cdr input) labels sigma variables)
                                        "Only machines allowed are turing machines")]
          )
    )

  (check-expect (valid-ctmd? '() '() '() '()) #t)
  (check-expect (valid-ctmd? '(5) '(5) '() '()) #t) ;; Rest of ctmd is empty, which is valid
  (check-expect (valid-ctmd? '(sym) '() '() '(sym)) #t) ;; Rest of ctmd is empty, which is valid
  (check-expect (valid-ctmd? `((,GOTO 5) s) '(5) '() '(s)) #t)
  (check-expect (valid-ctmd? `((,BRANCH ((a (,GOTO 10)) (b (,GOTO 20)))) d) '(10 20) '(a b d) '(d)) #t)
  (check-expect (valid-ctmd? `((,BRANCH ((a (,GOTO a)) (b (,GOTO 20)))) 'd) '(20) '(a b d) '())
                "The following branches have errors: ((a (GOTO a)))")
  (check-expect (valid-ctmd? `(s) '() '() '()) "The variable being referenced is not defined in this scope.")

  ; check-ctmd: any (list of symbol) -> true or string
  ; Purpose: Parses the given input. If the input is an s-exp representing a valid
  ; CTMD, then returns true. Otherwise, it returns a string containing an error message.
  (define (check-ctmd input sigma)
    (let [(labels (gather-labels input))
          (initial-variables '())]
      (valid-ctmd? input labels sigma initial-variables)))

  (check-expect (check-ctmd `(((,VAR a) (a))) '()) #t)
  (check-expect (check-ctmd `(((,VAR a) (b))) '()) "The variable being referenced is not defined in this scope.")
  (check-expect (check-ctmd `((, BRANCH ((a (,GOTO 10)) (b (,GOTO 20)))) 10 20) '(a b)) #t)
  (check-expect (check-ctmd `((, BRANCH ((a (,GOTO 10)) (b (,GOTO 20)))) 10 230) '(a b))
                "The following branches have errors: ((b (GOTO 20)))") ; TODO: Should improve this to incorporate the underlying error message.
  
  (test)

  #;(define copy-ctmd (list FBL 
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
                          5))

  #;(check-ctmd copy-ctmd '(a b))
  )