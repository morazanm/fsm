; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan
; Written by: Marco T. Morazan

;;; REGULAR EXPRESSIONS

; A regular expression (regexp) is either
;  0. empty-regexp
;  1. A singleton-regexp
;  2. A concat-regexp
;  3. A union-regexp
;  4. A kleenestar-regexp

(module regexp racket
  (require "constants.rkt")
  (provide null-regexp null-regexp? 
           empty-regexp empty-regexp? 
           make-unchecked-singleton singleton-regexp? singleton-regexp-a
           make-unchecked-concat concat-regexp? concat-regexp-r1 concat-regexp-r2
           make-unchecked-union union-regexp? union-regexp-r1 union-regexp-r2
           make-unchecked-kleenestar kleenestar-regexp? kleenestar-regexp-r1
           printable-regexp
           NULL-REGEXP-STRING EMPTY-REGEXP-STRING
           simplify-regexp
           regexp?
           gen-regexp-word gen-concat-word gen-ks-word extract-concat-regexps
           convert-singleton pick-regexp extract-union-regexps pick-reps
           )
  
  (define NULL-REGEXP-STRING "()")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  
  ;   --> regexp
  (struct null-regexp () #:transparent) ;;; represents no transition
  
  ; --> empty-regexp
  (struct empty-regexp () #:transparent) ;;; represents empty transition
  
  ; single character string --> singleton-regexp
  (struct singleton-regexp (a) #:transparent)
  (define (make-unchecked-singleton a)
    (singleton-regexp a)
    )
  
  ; regexp regexp --> concat-regexp
  (struct concat-regexp (r1 r2) #:transparent)
  (define (make-unchecked-concat a b)
    (concat-regexp a b)
    )
  
  ; regexp regexp --> union-regexp
  (struct union-regexp (r1 r2) #:transparent)
  (define (make-unchecked-union a b)
    (union-regexp a b)
    )
  
  ; regexp --> kleenestar-regexp
  (struct kleenestar-regexp (r1) #:transparent)
  (define (make-unchecked-kleenestar a)
    (kleenestar-regexp a)
    )
  
  (define R1 (singleton-regexp "a"))
  (define R2 (singleton-regexp "b"))
  (define R3 (union-regexp  R1 R2))
  (define R4 (kleenestar-regexp R3))
  (define R5 (concat-regexp R2 R4))
  (define R6 (empty-regexp))
  (define R7 (null-regexp))
  
  ; union-regexp --> boolean
  (define (union-has-empty? r)
    (let ((r1 (union-regexp-r1 r))
          (r2 (union-regexp-r2 r)))
      (or (empty-regexp? r1) 
          (empty-regexp? r1))))
  
  (define (next-concat-element r)
    (cond [(null-regexp? r) r]
          [(singleton-regexp? r) r]
          [(union-regexp? r) r]
          [(kleenestar-regexp? r) r]
          [(concat-regexp? r) (next-concat-element (concat-regexp-r1 r))]))
          
          
  
  ;simplify-regexp: regexp --> regexp
  ;purpose: to simplify a regexp
  (define (simplify-regexp x)
    ;(define d (displayln (format "in simplify-regexp x: ~s" x)))
    (local [;simplify-kleenestar: kleenestar --> regexp
            ;purpose: to simplify a kleenestar
            (define (simplify-kleenestar x)
              (local [(define simp-x (simplify-regexp (kleenestar-regexp-r1 x)))]
                (cond [(or (kleenestar-regexp? simp-x)
                           (empty-regexp? simp-x)) simp-x]
                      [else (kleenestar-regexp simp-x)])))

            ;simplify-concat: concat --> regexp
            ;purpose: to simplify a concat-regexp
            (define (simplify-concat x)
              (local [(define simp-lhs (simplify-regexp (concat-regexp-r1 x)))
                      (define simp-rhs (simplify-regexp (concat-regexp-r2 x)))]
                (cond [(empty-regexp? simp-lhs) simp-rhs]
                      [(empty-regexp? simp-rhs) simp-lhs]
                      [else (concat-regexp simp-lhs simp-rhs)])))
            ]
      (cond [(or (empty-regexp? x)
                 (null-regexp? x)
                 (singleton-regexp? x)) x]
            [(kleenestar-regexp? x) (simplify-kleenestar x)]
            [(concat-regexp? x) (simplify-concat x)]
            [(union-regexp? x) (simplify-union x)])))


  (define (simplify-union x)
    ;(define d (displayln (format "in simplify-union x: ~s" (printable-regexp x))))
    (local [;simplify the lhs and rhs
            (define simp-lhs (simplify-regexp (union-regexp-r1 x)))
            (define simp-rhs (simplify-regexp (union-regexp-r2 x)))

            #;(define d (displayln (format "e: ~s\nslhs: ~s\nsrhs: ~s\n"
                                         (printable-regexp x)
                                         (printable-regexp simp-lhs)
                                         (printable-regexp simp-rhs))))

            ;return-compnents: regexp --> (listof regexp)
            ;purpose: return all the individual components of a regexp
            (define (return-components x)
              (cond [(null-regexp? x) empty]
                    [(or (empty-regexp? x)
                         (singleton-regexp? x)
                         (kleenestar-regexp? x)
                         (concat-regexp? x))
                     (list x)]
                    [(union-regexp? x)
                     (append (return-components (union-regexp-r1 x))
                             (return-components (union-regexp-r2 x)))]
                    )
              )

            ;remove-duplicates: (listof regexp) --> (listof regexp)
            ;purpose: to remove the duplicates from a list of regular expressions
            #;(define (remove-duplicates a-list)
              (cond [(empty? a-list) empty]
                    [(or (member (first a-list) (rest a-list))
                         (ormap (lambda (x) (and (kleenestar-regexp? x)
                                                 (member (first a-list) (return-components (kleenestar-regexp-r1 x))))) (rest a-list)))                              
                     (remove-duplicates (rest a-list))]
                    [(kleenestar-regexp? (first a-list)) (local [(define comps (return-components (kleenestar-regexp-r1 (first a-list))))]
                                                           (cons (first a-list)
                                                                 (remove-duplicates (filter (lambda (x) (member x comps)) (rest a-list)))))]
                    [else (cons (first a-list) (remove-duplicates (rest a-list)))]))
          
            ;remove the duplicates from the union
            (define clean-list (reverse (remove-duplicates (append (return-components simp-lhs)
                                                                   (return-components simp-rhs)))))

            ;(define ddd (displayln (format "cl: ~s\ncr:~s" (return-components simp-lhs) (return-components simp-rhs))))

            ;re-union: (listof regexp) --> regexp
            ;purpose: to recreate a union-regexp
            (define (re-union a-list)
              (cond [(empty? a-list) (empty-regexp)]
                    [(empty? (rest a-list)) (first a-list)]
                    [else (union-regexp (first a-list) (re-union (rest a-list)))]))

          
            ;reunify the cleaned list
            (define clean-union (re-union clean-list))
            ]
      clean-union))

  (define R (union-regexp (concat-regexp (concat-regexp (empty-regexp) (empty-regexp))
                                         (concat-regexp (kleenestar-regexp (union-regexp (singleton-regexp "b")
                                                                                         (singleton-regexp "c")))
                                                        (empty-regexp)))
                          (concat-regexp (concat-regexp (empty-regexp) (empty-regexp))
                                         (concat-regexp (kleenestar-regexp
                                                         (union-regexp (singleton-regexp "a")
                                                                       (singleton-regexp "c")))
                                                        (empty-regexp)))))
  
  
  ; regexp --> string
  (define (printable-regexp r)
    ; regexp --> string
    (define (helper r)
      (cond [(null-regexp? r) NULL-REGEXP-STRING]
            [(empty-regexp? r) EMPTY-REGEXP-STRING]
            [(singleton-regexp? r) (singleton-regexp-a r)]
            [(concat-regexp? r) 
             (let ((r1 (helper (concat-regexp-r1 r)))
                   (r2 (helper (concat-regexp-r2 r))))
               (string-append r1 r2))]
            [(union-regexp? r) 
             (let ((r1 (helper (union-regexp-r1 r)))
                   (r2 (helper (union-regexp-r2 r))))
               (string-append "(" r1 " U " r2 ")"))]
            [(kleenestar-regexp? r) 
             (let* ((r1 (helper (kleenestar-regexp-r1 r))))
               (cond [(= (string-length r1) 1) (string-append r1 "*")]
                     [(and (equal? (substring r1 0 1) "(")
                           (equal? (substring r1 (sub1 (string-length r1)) (string-length r1))
                                   ")"))
                      (string-append r1 "*")]
                     [else (string-append "(" r1 ")" "*")]))]))
    (helper r))

  ;regexp?: element --> boolean
  (define (regexp? x)
    (or (concat-regexp? x)
        (union-regexp? x)
        (singleton-regexp? x)
        (null-regexp? x)
        (empty-regexp? x)
        (kleenestar-regexp? x)))

  ;; union-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps in the chain for the given union-regexp
  (define (extract-union-regexps urexp)
    (let [(r1 (union-regexp-r1 urexp))
          (r2 (union-regexp-r2 urexp))]
      (if (not (union-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-union-regexps r2)))))
  
  ;; union-regexp --> regexp
  ;; Purpose: Return a randomly chosen sub-regexp from the given union-regexp
  (define (pick-regexp e)
    (let [(uregexps (extract-union-regexps e))]
      (list-ref uregexps (random (length uregexps)))))

  ;; natnum --> natnum
  ;; Purpose: Randomly pick a natnum in [0..n]
  (define (pick-reps n) (random (add1 n)))

  ;; singleton-regexp --> symbol or number
  ;; Purpose: Convert the given singleton-regexp to a symbol or number
  (define (convert-singleton e)
    (let [(element (singleton-regexp-a e))]
      (if (not (string<=? "0" element "9"))
          (list (string->symbol element))
          (list (string->number element)))))

  ;; concat-rexp --> (listof regexp)
  ;; Purpose: Extract the sub-regexps in the chain for the given concat-regexp
  (define (extract-concat-regexps crexp)
    (let [(r1 (concat-regexp-r1 crexp))
          (r2 (concat-regexp-r2 crexp))]
      (if (not (concat-regexp? r2))
          (list r1 r2)
          (cons r1 (extract-concat-regexps r2)))))

  ;; natnum kleene-star-regexp (regexp --> word) --> word
  ;; Purpose: Generate a word of arbitrary length in [0..reps+1] using
  ;;          given regular expression and the given word-generating function
  (define (gen-ks-word reps regexp gen-function)     
    (let [(lst-words (filter
                      (λ (w) (not (eq? w EMP)))
                      (flatten
                       (build-list
                        (random (add1 reps))
                        (λ (i) (gen-function (kleenestar-regexp-r1 regexp) reps))))))]
      (if (empty? lst-words) EMP lst-words)))

  ;; concat-regexp (regexp --> word) --> word
  ;; Purpose: Generate a word by concatenating a words generated
  ;;          from the sub-regexps in the given concat-regexp using
  ;;          the given word-generting function
  (define (gen-concat-word concat-rexp gen-function reps)
    (let [(res (filter (λ (w) (not (eq? w EMP)))
                       (flatten (map (λ (re) (gen-function re reps))
                                     (extract-concat-regexps concat-rexp)))))]
      (if (empty? res) EMP res)))

              
  ;; regexp [natnum] --> word
  ;; Purpose: Nondeterministically generate a word in the language
  ;;          of the given regexp. The maximum repetitions for a Kleene
  ;;          star is the the given natnum if provided. Otherwise, it is 20
  (define (gen-regexp-word rexp . reps)
    (define MAX-KLEENESTAR-REPS (if (null? reps) 20 (first reps)))
    (cond [(empty-regexp? rexp) EMP]
          [(singleton-regexp? rexp) (convert-singleton rexp)]
          [(kleenestar-regexp? rexp)
           (gen-ks-word MAX-KLEENESTAR-REPS rexp gen-regexp-word)]
          [(union-regexp? rexp) (gen-regexp-word (pick-regexp rexp) MAX-KLEENESTAR-REPS)]
          [else (gen-concat-word rexp gen-regexp-word MAX-KLEENESTAR-REPS)]))

  
  
 
  ;;; END REGULAR EXPRESSIONS

  
  
  ) ;; closes module