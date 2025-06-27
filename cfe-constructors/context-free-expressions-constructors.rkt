#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/cfg.rkt"
         racket/hash
         racket/string)

(provide null-cfexp
         empty-cfexp
         singleton-cfexp
         concat-cfexp
         union-cfexp
         kleene-cfexp
         update-binding!
         gen-cfexp-word
         empty-cfexp-env
         var-cfexp ;;change to remove struct out
         #;env-cfexp)

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. variable
;; 5. concat 
;; 6. union
;; 7. kleene

(struct cfexp ([env #:mutable]) #:transparent)

;;hash ;;Purpose: To represent an empty environment
(define (empty-cfexp-env) (hash))

;;null-cfexp ;;Purpose: Only contains an environment
(struct mk-null-cfexp cfexp () #:transparent)

;;empty-cfexp ;;Purpose: A cfexp to represent the empty word
(struct mk-empty-cfexp cfexp () #:transparent)

;;singleton-cfexp ;;Purpose: A cfexp to represnt a single word
(struct mk-singleton-cfexp cfexp (char) #:transparent)

;;concat-cfexp ;;Purpose: A cfexp to represent the concatentation of cfexps
(struct mk-concat-cfexp cfexp (locfe) #:transparent)

;;union-cfexp ;;Purpose: A cfexp to represent the union of cfexps
(struct mk-union-cfexp cfexp (locfe) #:transparent)

;;Kleene-cfexp ;;Purpose: A cfexp to represent the Kleen of a cfexp
(struct mk-kleene-cfexp cfexp (cfe) #:transparent)

;;variable-cfexp ;;Purpose: A cfexp to represent a variable that needs to be substituted
(struct mk-var-cfexp cfexp (cfe) #:transparent)

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
  (mk-singleton-cfexp (empty-cfexp-env) a-char))

;;symbol -> variable-cfexp
;;Purpose: A wrapper to create a variable-cfexp
(define (var-cfexp symbol)
  (mk-var-cfexp (empty-cfexp-env) symbol))

;;(listof cfexp) -> env
;;Purpose: Merges all of the environments from the given (listof cfexp) in to one environment
(define (merge-env locfexp)
  (foldl (λ (cfe env)
           (let ([cfe-env-id (extract-hash-key cfe)])
             (if (hash-has-key? env cfe-env-id)
               (append (hash-ref (cfexp-env cfe) cfe-env-id) (hash-ref env cfe-env-id))
                     (hash-union env (cfexp-env cfe)))))
         (hash)
         locfexp))

;; . cfexp -> concat-cfexp
;;Purpose: A wrapper to create a concat-cfexp
(define (concat-cfexp . cfexp)
  (mk-concat-cfexp (merge-env cfexp) cfexp))

;; . cfexp -> union-cfexp
;;Purpose: A wrapper to create a union-cfexp
(define (union-cfexp . cfexp)
  (mk-union-cfexp (merge-env cfexp) cfexp))

;;cfexp -> Kleene-cfexp
;;Purpose: A wrapper to create a Kleene-cfexp
(define (kleene-cfexp cfe)
  (mk-kleene-cfexp (cfexp-env cfe) cfe))

;;cfe-id cfe -> env
;;Purpose: Creates an environment where the given cfe-id is the key and cfe is the value
(define (env-cfexp cfe-id binding)
  (hash cfe-id (list binding)))

;;var-cfexp cfe -> var-cfexp
;;Purpose: Creates a binding where the cfe is bound to the given var-cfexp's environment
(define (update-binding! cfe bindee-id binding)
  (let ([env (cfexp-env cfe)]
        [sym bindee-id])
    (begin
      (if (hash-has-key? env bindee-id)
          (hash-set! env bindee-id (cons binding (hash-ref env bindee-id)))
          (set! env (env-cfexp bindee-id binding)))
      (set-cfexp-env! cfe env)
      (set! cfe (mk-var-cfexp env bindee-id)))))
          
;;singleton-cfe -> word
;;Purpose: Extracts the singleton 
(define (convert-singleton cfe)
  (list (mk-singleton-cfexp-char cfe)))

;; union-cfexp --> cfexp
;; Purpose: Return a randomly chosen sub-cfexp from the given union-cfexp
(define (pick-cfexp cfexp)
  (let [(cfexps (mk-union-cfexp-locfe cfexp))]
    (list-ref cfexps (random (length cfexps)))))

;;var-cfexp --> word
;;Purpose: Substitutes the given var-cfexp with it's environment bindings 
(define (substitute-var cfe)
  (let ([bindings (hash-ref (cfexp-env cfe) (mk-var-cfexp-cfe cfe))])
    (gen-cfexp-word (list-ref bindings (random (length bindings))))))

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
        [(mk-var-cfexp? cfe) (substitute-var cfe)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word MAX-KLEENESTAR-REPS)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word (pick-cfexp cfe)  MAX-KLEENESTAR-REPS)]
        [else (gen-cfe-kleene-word cfe MAX-KLEENESTAR-REPS gen-cfexp-word)]))



(define (cfe->cfg cfe)
  cfe)