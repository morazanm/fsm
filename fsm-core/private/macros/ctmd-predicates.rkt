; FSM Library Version 1.0
; Copyright (C) 2020 by Marco T. Morazan
; Written by: Marco T. Morazan

(module ctmd-predicates racket 
  (require "../constants.rkt" "../sm-getters.rkt" "../../interface.rkt")

  (local-require test-engine/racket-tests)


  (define ZERO  (list BLANK))
  (define THREE '(d d d))
  (define EIGHT '(d d d d d d d d))

  ;;  PRE: tape = (LMw) AND i=k>0 AND w in (d)*
  ;; POST: tape = (LMw) AND i=k+1 AND w in (d)*
  (define R (make-tm '(S F)
                     '(d)
                     `(((S d) (F ,RIGHT))
                       ((S ,BLANK) (F ,RIGHT)))
                     'S
                     '(F)))


  ;;  PRE: tape = (LMw) AND i=k>0, where w in (a b)*
  ;; POST: tape = (LMw) AND i=k+1
  (define L (make-tm '(S H)
                     `(d)
                     `(((S d) (H ,LEFT))
                       ((S ,BLANK) (H ,LEFT)))
                     'S
                     '(H)))

  ;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (d BLANK)* AND s in (d BLANK)
  ;; POST: tape = (LM w) AND i=k AND tape[i]=d
  (define Wd (make-tm '(S H)
                      `(d)
                      `(((S d) (H d))
                        ((S ,BLANK) (H d)))
                      'S
                      '(H)))

  ;;  PRE: tape = (LM w) AND i=k>0 AND tape[i]=s, where w in (d BLANK)* AND s in (d BLANK)
  ;; POST: tape = (LM w) AND i=k AND tape[i]=BLANK
  (define WB (make-tm '(S H)
                      `(d)
                      `(((S d) (H ,BLANK))
                        ((S ,BLANK) (H ,BLANK)))
                      'S
                      '(H)))


  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (a b BLANK)*
  ;; POST: tape = (LM w) AND i>k AND tape[i]=BLANK AND tape[k+1..i-1] != BLANK
  (define FBR (combine-tms (list 0
                                 R
                                 (cons BRANCH
                                       (list (list 'd (list GOTO 0))
                                             (list BLANK (list GOTO 10))))
                                 10)
                           (list 'd)))



  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (d BLANK)*
  ;; POST: tape = (LM w) AND i>k AND tape[i]=d AND tape[k+1..i-1] != d
  (define FdR (combine-tms (list 0
                                 R
                                 (cons BRANCH
                                       (list (list 'd (list GOTO 10))
                                             (list BLANK (list GOTO 0))))
                                 10)
                           (list 'd)))

  (define swap (combine-tms (list (list (list VAR 'i)
                                        R
                                        (list (list VAR 'j)
                                              'i
                                              L
                                              'j)))
                            '(d)))

  ;; PRE:  tape = (LM w) AND i=k>0 AND w in (d BLANK)* AND exists j<i s.t. tape[j]=BLANK
  ;; POST: tape = (LM w) AND i<k AND tape[i]=BLANK AND tape[i+1..|w|] != BLANK
  (define FBL (combine-tms (list 0
                                 L
                                 (cons BRANCH
                                       (list (list 'd (list GOTO 0))
                                             (list BLANK (list GOTO 1))
                                             (list LM (list GOTO 0))))
                                 1)
                           '(d)))

  (define SHL (combine-tms (list 0
                                 R
                                 `(,BRANCH (d (GOTO 20))
                                           (,BLANK (GOTO 10)))
                                 10
                                 FBL
                                 '(GOTO 30)
                                 20
                                 L
                                 swap
                                 R
                                 '(GOTO 0)
                                 30)
                           '(d)))
                                 
                               


  ;;  PRE: tape = (LM BLANK a BLANK b AND i=1)
  ;; POST: tape = (LM BLANK a b BLANK) AND i=3 if a=b=0, i=a+b+2 otherwise
  #;(define ADD (combine-tms
                 (list FBR Wd FBR L WB)
                 '(d)))

  (define ADD (combine-tms
               (list R
                     `(,BRANCH (,BLANK (GOTO 100))
                               (d (GOTO 200)))
                     100 ;; a=0
                     R
                     R
                     `(,BRANCH (,BLANK (GOTO 500)) ;; a=b=0
                               (d (GOTO 300)))
                     200 ;; a!=0
                     FBR
                     Wd
                     FBR
                     L
                     WB
                     `(,GOTO 1000)
                     300 ;; a=0 and b!=0
                     L
                     SHL
                     FBL
                     SHL
                     `(,GOTO 1000)
                     500 ;; a=0 and b=0
                     L
                     `(,GOTO 1000)
                     1000)
             
               '(d)))

  ;;;;;;; empty list
  ;; (cons m ctmd) <-- m is a tm or ctmd
  ;;;;;;; (cons LABEL ctmd) <-- labels are numbers
  ;;;;;;; (cons (list GOTO LABEL) ctmd)
  ;;;;;;; (cons (list BRANCH (listof (list symbol (list GOTO LABEL)))) ctmd)
  ;; (cons (list (list VAR symbol) ctmd) ctmd)
  ;; (cons variable ctmd)

  ;;takes as input a list
  ;;must be a valid list of 2
  ;; starting with the symbol GOTO
  ;; ending with a valid label
  (define (valid-goto? input)
    (and (list? input)
         (= (length input) 2)
         (equal? (car input) GOTO)
         (number? (cadr input))))

  (check-expect (valid-goto? `(,GOTO 10)) #t)
  (check-expect (valid-goto? `(,GOTO 'a)) #f)

  ;;invalid-branches: takes as input a list of items in a branch expression
  ;; these must all be valid branches of the format
  ;;   list of 2
  ;;   starting with a symbol
  ;;   ending with a valid GOTO expression
  (define (invalid-branches input)
    (filter (lambda (x) (not (and (list? x)
                                  (= (length x) 2)
                                  (symbol? (car x))
                                  (valid-goto? (cadr x))))) input))

  (check-expect (invalid-branches `((a (,GOTO 10))
                                    (b (,GOTO 15)))) '())
  (check-expect (invalid-branches `((10 (,GOTO 10))
                                    (b (,GOTO 15)))) `((10 (,GOTO 10))))
  (check-expect (invalid-branches `((a (,BRANCH 10))
                                    (b (,GOTO 15)))) `((a (,BRANCH 10))))

  ;;valid-list-case: takes the first portion of a ctmd, if that portion is a list
  ;;it must either be
  ;;    a valid GOTO expression
  ;;    a valid BRANCH expression
  ;;    a valid VAR declaration followed by a valid CTMD
  ;;    a full valid ctmd or valid tm
  (define (valid-list-case? input)
    (cond [(equal? GOTO (car input)) (if (not (= (length input) 2)) "A GOTO expression must be of length 2"
                                         (if (number? (cadr input)) #t
                                             "The second part of GOTO must be a label"))] ;;; must be an existing label
          [(equal? BRANCH (car input)) (if (not (= (length input) 2)) "A BRANCH expression must be of length 2"
                                           (if (not (list? (cadr input))) "The second part of a BRANCH must be a list"
                                               (let [(all-invalid-branches (invalid-branches (cadr input)))]
                                                 (if (empty? all-invalid-branches) #t
                                                     (format "The following branches have errors: ~a" all-invalid-branches)))))]
          [(and (list? (car input))
                (equal? (car (car input)) VAR)) (if (not (= (length (car input)) 2)) "A VAR declaration must be of length 2"
                                                    (if (not (symbol? (cadr (car input)))) "The second part of a VAR declaration must be a symbol"
                                                        (valid-ctmd? (cadr input))))]
          [else (valid-ctmd? input)]
          ))

  (check-expect (valid-list-case? `(,GOTO 10)) #t)
  (check-expect (valid-list-case? `(,GOTO 20 30)) "A GOTO expression must be of length 2")
  (check-expect (valid-list-case? `(,GOTO 'a)) "The second part of GOTO must be a label")

  (check-expect (valid-list-case? `(,BRANCH ((a (,GOTO 10))
                                             (b (,GOTO 20)))))
                #t)
  (check-expect (valid-list-case? `(,BRANCH (a (,GOTO 10))
                                            (b (,GOTO 20))))
                "A BRANCH expression must be of length 2")
  (check-expect (valid-list-case? `(,BRANCH a))
                "The second part of a BRANCH must be a list")
  (check-expect (valid-list-case? `(,BRANCH ((a (,GOTO a))
                                             (b (,GOTO 20)))))
                "The following branches have errors: ((a (GOTO a)))")

  ;; may need accumulator for variables defined in scope
  (define (valid-ctmd? input)
    (cond [(empty? input) #t]
          [(number? (car input)) (valid-ctmd? (cdr input))]
          [(symbol? (car input)) (valid-ctmd? (cdr input))] ;; is symbol in accumulated lsit
          [(list? (car input)) (let [(list-case (valid-list-case? (car input)))]
                                 (if (string? list-case) list-case
                                    (valid-ctmd? (cdr input))))]
          [(procedure? (car input)) (if (or (equal? (sm-type (car input)) 'tm)
                                            (equal? (sm-type (car input)) 'tm-language-recognizer))
                                        (valid-ctmd? (cdr input))
                                        "Only machines allowed are turing machines")]
          )
    )

  (check-expect (valid-ctmd? '()) #t)
  (check-expect (valid-ctmd? '(5)) #t) ;; Rest of ctmd is empty, which is valid
  (check-expect (valid-ctmd? '(sym)) #t) ;; Rest of ctmd is empty, which is valid
  (check-expect (valid-ctmd? `((,GOTO 5) 's)) #t)
  (check-expect (valid-ctmd? `((,BRANCH ((a (,GOTO 10)) (b (,GOTO 20)))) 'd)) #t)
  (check-expect (valid-ctmd? `((,BRANCH ((a (,GOTO a)) (b (,GOTO 20)))) 'd))
                "The following branches have errors: ((a (GOTO a)))")
  
  (test)
  )