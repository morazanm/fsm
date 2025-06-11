#lang racket

(require "../fsm-core/private/constants.rkt"
         racket/hash
         racket/string)

(provide null-cfexp
         empty-cfexp
         singleton-cfexp
         concat-cfexp
         union-cfexp
         kleene-cfexp
         add2env
         gen-cfexp-word)

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. add2env (base case)
;; 5. concat 
;; 6. union
;; 7. kleene

(struct mk-null-cfexp (env) #:transparent)

(struct mk-empty-cfexp (env) #:transparent)

(struct mk-singleton-cfexp (a env) #:transparent)

(struct mk-concat-cfexp (cfe1 cfe2 env) #:transparent)

(struct mk-union-cfexp (cfe1 cfe2 env) #:transparent)

(struct mk-kleene-cfexp (cfe env) #:transparent)

(define (null-cfexp)
  (mk-null-cfexp (hash)))

(define (empty-cfexp)
  (mk-empty-cfexp (hash)))

(define (singleton-cfexp a-char)
  (mk-singleton-cfexp a-char (hash)))

(define (concat-cfexp cfe1 cfe2)
  (let ([cfe1-env (cond [(mk-null-cfexp? cfe1) (mk-null-cfexp-env cfe1)]
                        [(mk-empty-cfexp? cfe1) (mk-empty-cfexp-env cfe1)]
                        [(mk-singleton-cfexp? cfe1) (mk-singleton-cfexp-env cfe1)]
                        [(mk-concat-cfexp? cfe1) (mk-concat-cfexp-env cfe1)]
                        [(mk-union-cfexp? cfe1) (mk-union-cfexp-env cfe1)]
                        [else (mk-kleene-cfexp-env cfe1)])]
        [cfe2-env (cond [(mk-null-cfexp? cfe2) (mk-null-cfexp-env cfe2)]
                        [(mk-empty-cfexp? cfe2) (mk-empty-cfexp-env cfe2)]
                        [(mk-singleton-cfexp? cfe2) (mk-singleton-cfexp-env cfe2)]
                        [(mk-concat-cfexp? cfe2) (mk-concat-cfexp-env cfe2)]
                        [(mk-union-cfexp? cfe2) (mk-union-cfexp-env cfe2)]
                        [else (mk-kleene-cfexp-env cfe2)])])
    (mk-concat-cfexp cfe1 cfe2 (hash-union cfe1-env cfe2-env))))

(define (union-cfexp cfe1 cfe2)
  (let ([cfe1-env (cond [(mk-null-cfexp? cfe1) (mk-null-cfexp-env cfe1)]
                        [(mk-empty-cfexp? cfe1) (mk-empty-cfexp-env cfe1)]
                        [(mk-singleton-cfexp? cfe1) (mk-singleton-cfexp-env cfe1)]
                        [(mk-concat-cfexp? cfe1) (mk-concat-cfexp-env cfe1)]
                        [(mk-union-cfexp? cfe1) (mk-union-cfexp-env cfe1)]
                        [else (mk-kleene-cfexp-env cfe1)])]
       [cfe2-env (cond [(mk-null-cfexp? cfe2) (mk-null-cfexp-env cfe2)]
                        [(mk-empty-cfexp? cfe2) (mk-empty-cfexp-env cfe2)]
                        [(mk-singleton-cfexp? cfe2) (mk-singleton-cfexp-env cfe2)]
                        [(mk-concat-cfexp? cfe2) (mk-concat-cfexp-env cfe2)]
                        [(mk-union-cfexp? cfe2) (mk-union-cfexp-env cfe2)]
                        [else (mk-kleene-cfexp-env cfe2)])])
    (mk-union-cfexp cfe1 cfe2 (hash-union cfe1-env cfe2-env))))

(define (kleene-cfexp cfe)
  (let ([cfe-env (cond [(mk-null-cfexp? cfe) (mk-null-cfexp-env cfe)]
                       [(mk-empty-cfexp? cfe) (mk-empty-cfexp-env cfe)]
                       [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-env cfe)]
                       [(mk-concat-cfexp? cfe) (mk-concat-cfexp-env cfe)]
                       [(mk-union-cfexp? cfe) (mk-union-cfexp-env cfe)]
                       [else (mk-kleene-cfexp-env cfe)])])
    (mk-kleene-cfexp cfe cfe-env)))

(define (add2env cfe language-map)
  (define (add-to-env-helper hash category langauge)
  (if (hash-has-key? hash category)
      (hash-set hash category (cons langauge (hash-ref hash category)))
      (hash-set hash category (list langauge))))
  (cond [(mk-null-cfexp? cfe)
         (struct-copy mk-null-cfexp cfe
                 [env (add-to-env-helper (mk-null-cfexp-env cfe) (first language-map) (second language-map))])]
        [(mk-empty-cfexp? cfe)
         (struct-copy mk-empty-cfexp cfe
                 [env (add-to-env-helper (mk-empty-cfexp-env cfe) (first language-map) (second language-map))])]
        [(mk-singleton-cfexp? cfe)
         (struct-copy mk-singleton-cfexp cfe
                 [env (add-to-env-helper (mk-singleton-cfexp-env cfe) (first language-map) (second language-map))])]
        [(mk-concat-cfexp? cfe)
         (struct-copy mk-concat-cfexp cfe
                 [env (add-to-env-helper (mk-concat-cfexp-env cfe) (first language-map) (second language-map))])]
        [(mk-union-cfexp? cfe)
         (struct-copy mk-union-cfexp cfe
                 [env (add-to-env-helper (mk-union-cfexp-env cfe) (first language-map) (second language-map))])]
        [else (struct-copy mk-kleene-cfexp cfe
                 [env (add-to-env-helper (mk-kleene-cfexp-env cfe) (first language-map) (second language-map))])]))


  


(define (gen-cfexp-word cfe . reps)
  (define MAX-KLEENESTAR-REPS (if (empty? reps) 20 (first reps)))
  ;; union-regexp --> regexp
  ;; Purpose: Return a randomly chosen sub-regexp from the given union-regexp
  (define (pick-cfexp e)
    ;; union-rexp --> (listof regexp)
    ;; Purpose: Extract the sub-regexps in the chain for the given union-regexp
    (define (extract-union-cfexps cfexp)
      (let [(r1 (mk-union-cfexp-cfe1 cfexp))
            (r2 (mk-union-cfexp-cfe2 cfexp))]
        (if (not (mk-union-cfexp? r2))
            (list r1 r2)
            (cons r1 (extract-union-cfexps r2)))))
    (let [(cfexps (extract-union-cfexps e))]
      (list-ref cfexps (random (length cfexps)))))
  ;; -> (listof )
  ;;Purpose: Extracts the singleton 
  (define (convert-singleton)
    (define (substitute-symbol sym)
      (let ([env (mk-singleton-cfexp-env cfe)])
      sym))
    (let ([element (substitute-symbol (mk-singleton-cfexp-a cfe))])
      (if (not (string<=? "0" element "9"))
          (list (string->symbol element))
          (list (string->number element)))))
  (define (gen-concat-word concat-rexp gen-function reps)
    ;; concat-rexp --> (listof regexp)
    ;; Purpose: Extract the sub-regexps in the chain for the given concat-regexp
    (define (extract-concat-regexps crexp)
      (let [(r1 (mk-concat-cfexp-cfe1 crexp))
            (r2 (mk-concat-cfexp-cfe2 crexp))]
        (if (not (mk-concat-cfexp? r2))
            (list r1 r2)
            (cons r1 (extract-concat-regexps r2)))))
    (let [(res (filter (λ (w) (not (eq? w EMP)))
                       (flatten (map (λ (re) (gen-function re reps))
                                     (extract-concat-regexps concat-rexp)))))]
      (if (empty? res) EMP res)))

  ;; natnum kleene-star-regexp (regexp --> word) --> word
  ;; Purpose: Generate a word of arbitrary length in [0..reps+1] using
  ;;          given regular expression and the given word-generating function
  (define (gen-cfe-kleene-word cfe reps gen-function)
    (let [(lst-words (filter
                      (λ (w) (not (eq? w EMP)))
                      (flatten
                       (build-list
                        (random (add1 reps))
                        (λ (i) (gen-function (mk-kleene-cfexp-cfe cfe) reps))))))]
      (if (empty? lst-words) EMP lst-words)))
  
 
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (convert-singleton)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word MAX-KLEENESTAR-REPS)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word (pick-cfexp cfe) MAX-KLEENESTAR-REPS)]
        [else (gen-cfe-kleene-word cfe MAX-KLEENESTAR-REPS gen-cfexp-word)]))



#|
(define sample-hash (hash))

(define sh1 (if (hash-has-key? sample-hash 'S)
    (hash-set sample-hash 'S (cons 'aSb (hash-ref sample-hash 'S)))
    (hash-set sample-hash 'S (list 'aSb))))

(define (sample-add-to-env hash category langauge)
  (if (hash-has-key? hash category)
      (hash-set hash category (cons langauge (hash-ref hash category)))
      (hash-set hash category (list langauge))))
|#