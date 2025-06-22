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
         make-varcfexp-binding
         gen-cfexp-word
         empty-cfexp-env
         (struct-out var-cfexp) ;;change to remove struct out
         env-cfexp)

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. variable
;; 5. concat 
;; 6. union
;; 7. kleene


;;hash ;;Purpose: To represent an empty environment
(define (empty-cfexp-env) (hash))

;;null-cfexp ;;Purpose: Only contains an environment
(struct mk-null-cfexp (env) #:transparent)

;;empty-cfexp ;;Purpose: A cfexp to represent the empty word
(struct mk-empty-cfexp (env) #:transparent)

;;singleton-cfexp ;;Purpose: A cfexp to represnt a single word
(struct mk-singleton-cfexp (char env) #:transparent)

;;concat-cfexp ;;Purpose: A cfexp to represent the concatentation of cfexps
(struct mk-concat-cfexp (locfe env) #:transparent)

;;union-cfexp ;;Purpose: A cfexp to represent the union of cfexps
(struct mk-union-cfexp (locfe env) #:transparent)

;;Kleene-cfexp ;;Purpose: A cfexp to represent the Kleen of a cfexp
(struct mk-kleene-cfexp (cfe env) #:transparent)

;;variable-cfexp ;;Purpose: A cfexp to represent a variable that needs to be substituted
(struct var-cfexp (cfe [env #:mutable]) #:transparent)

;; -> null-cfexp
;;Purpose: A wrapper to create a null-cfexp
(define (null-cfexp)
  (mk-null-cfexp (empty-cfexp-env)))

;; -> empty-cfexp
;;Purpose: A wrapper to create a empty-cfexp
(define (empty-cfexp)
  (mk-empty-cfexp (empty-cfexp-env)))

;; symbol -> singleton-cfexp
;;Purpose: A wrapper to create a singleton-cfexp
(define (singleton-cfexp a-char)
  (mk-singleton-cfexp a-char (empty-cfexp-env)))


;;cfe -> env
;;Purpose: Extracts the environment from the given cfe 
(define (get-cfe-env cfe)
  (cond [(mk-null-cfexp? cfe) (mk-null-cfexp-env cfe)]
        [(mk-empty-cfexp? cfe) (mk-empty-cfexp-env cfe)]
        [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-env cfe)]
        [(mk-concat-cfexp? cfe) (mk-concat-cfexp-env cfe)]
        [(mk-union-cfexp? cfe) (mk-union-cfexp-env cfe)]
        [(var-cfexp? cfe) (var-cfexp-env cfe)]
        [else (mk-kleene-cfexp-env cfe)]))

;;(listof cfexp) -> env
;;Purpose: Merges all of the environments from the given (listof cfexp) in to one environment
(define (merge-env locfexp)
  (foldl (λ (cfe env)
           (hash-union env (get-cfe-env cfe)))
         (hash)
         locfexp))

;; . cfexp -> concat-cfexp
;;Purpose: A wrapper to create a concat-cfexp
(define (concat-cfexp . cfexp)
  (let ([locfexp (flatten cfexp)])
  (mk-concat-cfexp locfexp (merge-env locfexp))))

;; . cfexp -> union-cfexp
;;Purpose: A wrapper to create a union-cfexp
(define (union-cfexp . cfexp)
  (let ([locfexp (flatten cfexp)])
    (mk-union-cfexp locfexp (merge-env locfexp))))

;;cfexp -> Kleene-cfexp
;;Purpose: A wrapper to create a Kleene-cfexp
(define (kleene-cfexp cfe)
  (let ([cfe-env (get-cfe-env cfe)])
    (mk-kleene-cfexp cfe cfe-env)))

;;cfe-id cfe -> env
;;Purpose: Creates an environment where the given cfe-id is the key and cfe is the value
(define (env-cfexp cfe-id binding)
  (hash cfe-id binding))

;;var-cfexp cfe -> var-cfexp
;;Purpose: Creates a binding where the cfe is bound to the given var-cfexp's environment
(define (make-varcfexp-binding bindee binding)
  (let ([env (var-cfexp-env bindee)]
        [sym (var-cfexp-cfe bindee)])
    (begin
      (set! env (env-cfexp sym binding))
      (set-var-cfexp-env! bindee env)
      (set! bindee (var-cfexp sym env))
      bindee)))

;;singleton-cfe -> word
;;Purpose: Extracts the singleton 
(define (convert-singleton cfe)
  (list (mk-singleton-cfexp-char cfe))
  ;;change to allow for numbers
  #;(let ([element (mk-singleton-cfexp-a cfe)])
    (if (not (string<=? "0" element "9"))
        (list (string->symbol element))
        (list (string->number element)))))

;; union-cfexp --> cfexp
;; Purpose: Return a randomly chosen sub-cfexp from the given union-cfexp
(define (pick-cfexp cfexp)
  (let [(cfexps (mk-union-cfexp-locfe cfexp))]
    (list-ref cfexps (random (length cfexps)))))

;;var-cfexp --> word
;;Purpose: Substitutes the given var-cfexp with it's environment bindings 
(define (substitute-var cfe)
  (gen-cfexp-word (hash-ref (var-cfexp-env cfe) (var-cfexp-cfe cfe))))

;;concat-cfexp --> word
;;Purpose: Returns the concatenation of the sub context-free expressions 
(define (gen-concat-word concat-cfexp gen-function reps)
  (let [(res (filter (λ (w) (not (eq? w EMP)))
                     (flatten (map (λ (cfe) (gen-function cfe reps))
                                   (mk-concat-cfexp-locfe concat-cfexp)))))]
      (if (empty? res) EMP res)))

;; natnum kleene-star-cfexp (cfexp --> word) --> word
;; Purpose: Generate a word of arbitrary length in [0..reps+1] using
;;          given context-free expression and the given word-generating function
(define (gen-cfe-kleene-word cfe reps gen-function)
  (let [(lst-words (filter
                    (λ (w) (not (eq? w EMP)))
                    (flatten
                     (build-list
                      (random (add1 reps))
                      (λ (i) (gen-function (mk-kleene-cfexp-cfe cfe) reps))))))]
    (if (empty? lst-words) EMP lst-words)))


;; cfe [natnum] -> word
;; Purpose: Generates a word using 
(define (gen-cfexp-word cfe . reps)
  (define MAX-KLEENESTAR-REPS (if (empty? reps) 20 (first reps)))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (convert-singleton cfe)]
        [(var-cfexp? cfe) (substitute-var cfe)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word MAX-KLEENESTAR-REPS)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word (pick-cfexp cfe)  MAX-KLEENESTAR-REPS)]
        [else (gen-cfe-kleene-word cfe MAX-KLEENESTAR-REPS gen-cfexp-word)]))