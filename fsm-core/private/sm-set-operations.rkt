#lang racket/base

(require "sm-getters.rkt"
         "pda.rkt"
         "tm.rkt"
         "fsa.rkt")

(provide sm-union sm-concat sm-kleenestar sm-complement sm-intersection)

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
  