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
           word-in-regexp
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




  ;simplify-union: union --> regexp
  ;purpose: to simplify a union-regexp
  ;  (define (simplify-union x)
  ;    (define d (displayln (format "in simplify-union x: ~s" x)))
  ;    (local [;simplify the lhs and rhs
  ;            (define simp-lhs (simplify-regexp (union-regexp-r1 x)))
  ;            (define simp-rhs (simplify-regexp (union-regexp-r2 x)))
  ;
  ;            ;return-compnents: regexp --> (listof regexp)
  ;            ;purpose: return all the individual components of a regexp
  ;            (define (return-components x)
  ;              (cond [(or (empty-regexp? x)
  ;                         (null-regexp? x))
  ;                     empty]
  ;                    [(or (singleton-regexp? x)
  ;                         (kleenestar-regexp? x)
  ;                         (concat-regexp? x))
  ;                     (list x)]
  ;                    [(union-regexp? x)
  ;                     (append (return-components (union-regexp-r1 x))
  ;                             (return-components (union-regexp-r2 x)))]
  ;                    )
  ;              )
  ;
  ;            ;remove-duplicates: (listof regexp) --> (listof regexp)
  ;            ;purpose: to remove the duplicates from a list of regular expressions
  ;            (define (remove-duplicates a-list)
  ;              (cond [(empty? a-list) empty]
  ;                    [(or (member (first a-list) (rest a-list))
  ;                         (ormap (lambda (x) (and (kleenestar-regexp? x)
  ;                                                 (member (first a-list) (return-components (kleenestar-regexp-r1 x))))) (rest a-list)))                              
  ;                     (remove-duplicates (rest a-list))]
  ;                    [(kleenestar-regexp? (first a-list)) (local [(define comps (return-components (kleenestar-regexp-r1 (first a-list))))]
  ;                                                           (cons (first a-list)
  ;                                                                 (remove-duplicates (filter (lambda (x) (member x comps)) (rest a-list)))))]
  ;                    [else (cons (first a-list) (remove-duplicates (rest a-list)))]))
  ;          
  ;            ;remove the duplicates from the union
  ;            (define clean-list (reverse (remove-duplicates (append (return-components simp-lhs)
  ;                                                                                      (return-components simp-rhs)))))
  ;
  ;            ;re-union: (listof regexp) --> regexp
  ;            ;purpose: to recreate a union-regexp
  ;            (define (re-union a-list)
  ;              (cond [(empty? a-list) (empty-regexp)]
  ;                    [(empty? (rest a-list)) (first a-list)]
  ;                    [else (union-regexp (first a-list) (re-union (rest a-list)))]))
  ;
  ;          
  ;            ;reunify the cleaned list
  ;            (define clean-union (re-union clean-list))
  ;            ]
  ;      clean-union
  ;      )
  ;    )

  (define (simplify-union x)
    ;(define d (displayln (format "in simplify-union x: ~s" x)))
    (local [;simplify the lhs and rhs
            (define simp-lhs (simplify-regexp (union-regexp-r1 x)))
            (define simp-rhs (simplify-regexp (union-regexp-r2 x)))

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
            (define (remove-duplicates a-list)
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

            ;re-union: (listof regexp) --> regexp
            ;purpose: to recreate a union-regexp
            (define (re-union a-list)
              (cond [(empty? a-list) (empty-regexp)]
                    [(empty? (rest a-list)) (first a-list)]
                    [else (union-regexp (first a-list) (re-union (rest a-list)))]))

          
            ;reunify the cleaned list
            (define clean-union (re-union clean-list))
            ]
      clean-union
      )
    )
  
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

  ;; regexp --> word
  ;; Purpose: Generate a word in the language of the given regexp
  (define (word-in-regexp rexp)

    (define MAX-REPS 20)

    ;; union-rexp --> (listof regexp)
    ;; Purpose: Extract the sub-regexps of the given union-regexp
    (define (extract-union-regexps urexp)
      (let [(r1 (union-regexp-r1 urexp))
            (r2 (union-regexp-r2 urexp))]
        (if (not (union-regexp? r2))
            (list r1 r2)
            (cons r1 (extract-union-regexps r2)))))

    ;; concat-rexp --> (listof regexp)
    ;; Purpose: Extract the sub-regexps of the given concat-regexp
    (define (extract-concat-regexps crexp)
      (let [(r1 (concat-regexp-r1 crexp))
            (r2 (concat-regexp-r2 crexp))]
        (if (not (concat-regexp? r2))
            (list r1 r2)
            (cons r1 (extract-concat-regexps r2)))))
  
    (cond [(empty-regexp? rexp) EMP]
          [(singleton-regexp? rexp)
           (let [(element (singleton-regexp-a rexp))]
             (if (not (string<=? "0" element "9"))
                 (list (string->symbol element))
                 (list (string->number element))))]
          [(kleenestar-regexp? rexp)
           (let* [(reps (random MAX-REPS))
                  (element-list (append-map
                                 (λ (x) (list x))
                                 (build-list
                                  reps
                                  (λ (i) (word-in-regexp (kleenestar-regexp-r1 rexp))))))]
             (if (empty? element-list) EMP element-list))]
          [(union-regexp? rexp)
           (let* [(uregexps (extract-union-regexps rexp))
                  (element (list-ref uregexps (random (length uregexps))))]
             (word-in-regexp element))]
          [else (let [(cregexps (extract-concat-regexps rexp))]
                  (filter (λ (w) (not (eq? w EMP)))
                          (flatten (map word-in-regexp cregexps))))]))
  
 
  ;;; END REGULAR EXPRESSIONS
  
  
  )