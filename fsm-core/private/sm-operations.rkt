#lang racket/base

(require "sm-getters.rkt"
         "sm-apply.rkt"
         "constants.rkt"
         "word.rkt"
         "misc.rkt"
         "pda.rkt"
         "tm.rkt"
         "fsa.rkt"
         racket/list)

(provide sm-union sm-concat sm-kleenestar sm-complement sm-intersection sm-rename-states sm->grammar
         sm-sameresult? sm-testequiv? sm-test)

; grammar --> fsm
(define (sm->grammar m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (fsa->rg m)]
          [(eq? t1 'pda) (error (format "Converting a PDA to a Context-Free Grammar is not yet implemented....stay tuned!"))]
          [(eq? t1 'tm) (error (format "Converting a Turing machine to a Context-Sensitive Grammar is not yet implemented....stay tuned!"))]
          [else (error "Input is not a valid grammar.")])))

; fsm fsm --> fsm
(define (sm-union m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(not (eq? t1 t2)) 
           (error "Machines have different types: union-fsm")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (union-fsa m1 m2)]
          [(eq? t1 'pda) (union-pda m1 m2)]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-union m1 m2)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Union not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine types as input to sm-union: first input is of type ~s and second input is of type ~s." t1 t2))])))
  

; fsm fsm --> fsm
(define (sm-concat m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(not (eq? t1 t2)) 
           (error "Machines have different types: concat-fsm")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (concat-fsa m1 m2)]
          [(eq? t1 'pda) (concat-pda m1 m2)]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2))
           (error (format "Stay tuned: sm-concat for tm language recognizers is not yet implemented"))]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Concatenation not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine types as input to sm-concat: first input is of type ~s and second input is of type ~s." t1 t2))])))


; fsm --> fsm
(define (sm-kleenestar m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa)) 
           (kleenestar-fsa m)]
          [(eq? t1 'pda) (kleenestar-pda m )]
          [(eq? t1 'tm-language-recognizer)
           (error (format "Stay tuned: sm-kleenestar for tm language recognizers is not yet implemented"))]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Kleene star not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine type as input to sm-kleenestar: input is of type ~s" t1))])))


; fsm --> fsm or error
(define (sm-complement m)
  (let (( t1 (sm-type m)))
    (cond [(eq? t1 'pda)
           (error "Cannot complement a pda")]
          [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (complement-fsa m)]
          [(eq? t1 'tm-language-recognizer) (tm-complement m )]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Complement not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine type as input to sm-complement: input is of type ~s" t1))])))


  
; sm sm --> sm or error
(define (sm-intersection m1 m2)
  (let ((t1 (sm-type m1))
        (t2 (sm-type m2)))
    (cond [(and (or (eq? t1 'dfa) (eq? t1 'ndfa))
                (or (eq? t2 'dfa) (eq? t2 'ndfa)))
           (intersection-fsa m1 m2)]
          [(eq? t1 'pda)
           (error "Cannot intersect two pdas")]
          [(and (eq? t1 'tm-language-recognizer) (eq? t1 t2)) (tm-intersection m1 m2)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Intersection not supported for multitape Turing machines")]
          [else (error (format "Unknown/Invalid machine types as input to sm-intersection: first input is of type ~s and second input is of type ~s." t1 t2))])))


; (listof state) fsm --> fsm
(define (sm-rename-states sts m)
  (let ((t1 (sm-type m)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (rename-states-fsa sts m)]
          [(eq? t1 'pda) (rename-states-pda sts m)]
          [(or (eq? t1 'tm) (eq? t1 'tm-language-recognizer)) (tm-rename-states sts m)]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "State renaming not supported for multitape Turing machines")]
          [else (error "Incorrect input to sm-rename-states")])))
  

; fsm fsm word --> boolean
(define (sm-sameresult? M1 M2 w)
  (let ((s1 (sm-sigma M1))
        (s2 (sm-sigma M2)))
    (if (equal? s1 s2)
        (equal? (sm-apply M1 w) (sm-apply M2 w))
        (error (format "The alphabets of the given machines are different: ~s ~s" s1 s2)))))
  
; fsm fsm [natnum] --> true or (listof words)
(define (sm-testequiv?  M1 M2 . l)
  (define number-tests (if (null? l) NUM-TESTS (car l)))
  (if (or (eq? (sm-type M1) 'mttm)
          (eq? (sm-type M1) 'mttm-language-recognizer)
          (eq? (sm-type M2) 'mttm)
          (eq? (sm-type M2) 'mttm-language-recognizer))
      (error "Random testing of Multitape Turing Machines is not possible.")
      (let* ((test-m1 (generate-words number-tests
                                      (remove* `(,LM) (sm-sigma M1))
                                      null))
             (test-m2 (generate-words number-tests
                                      (remove* `(,LM) (sm-sigma M2))
                                      null))
             (test-words (append test-m1 test-m2))
             (res-m1 (map (lambda (w) (list w (sm-apply M1 w)))
                          (if (or (eq? (sm-type M1) 'tm)
                                  (eq? (sm-type M1) 'tm-language-recognizer))
                              (map (λ (w) (cons LM w)) test-words)
                              test-words)))
             (res-m2 (map (lambda (w) (list w (sm-apply M2 w)))
                          (if (or (eq? (sm-type M2) 'tm)
                                  (eq? (sm-type M2) 'tm-language-recognizer))
                              (map (λ (w) (cons LM w)) test-words)
                              test-words))))
        (cond [(or (eq? (sm-type M1) 'tm)
                   (eq? (sm-type M2) 'tm))
               (error "Random testing of Turing Machines is not possible.")]
              [(equal? res-m1 res-m2) #t]
              [else (remove-duplicates (get-differences res-m1 res-m2 test-words))]))))
  
; sm [natnum] --> (listof (list word symbol))
(define (sm-test M . l)
  (let ((numtests (if (null? l) NUM-TESTS (car l)))
        (t1 (sm-type M)))
    (cond [(or (eq? t1 'dfa)
               (eq? t1 'ndfa))
           (test-fsa M numtests)]
          [(eq? t1 'pda) (test-pda M numtests)]
          [(eq? t1 'tm-language-recognizer) (tm-test M numtests)]
          [(eq? t1 'tm) (error "Random testing of Turing Machines is not possible.")]
          [(or (eq? t1 'mttm) (eq? t1 'mttm-language-recognizer))
           (error "Random testing of Multitape Turing Machines is not possible.")]
          [else (error "Incorrect input to test-fsm")])))