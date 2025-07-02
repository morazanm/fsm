#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/misc.rkt"
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
         cfg->cfe
         cfe->cfg
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
           (let ([new-cfe-env (hash-map/copy (cfexp-env cfe)
                                             (λ (k v) (if (hash-has-key? env k)
                                                          (values (gen-state (hash-keys env)) v)
                                                          (values k v))))])
             (hash-union env new-cfe-env)))
         (hash)
         locfexp))

;; . cfexp -> concat-cfexp
;;Purpose: A wrapper to create a concat-cfexp
(define (concat-cfexp . cfexp)
  (let ([cfexp (flatten cfexp)])
    (mk-concat-cfexp (merge-env cfexp) cfexp)))

;; . cfexp -> union-cfexp
;;Purpose: A wrapper to create a union-cfexp
(define (union-cfexp . cfexp)
  (let ([cfexp (flatten cfexp)])
    (mk-union-cfexp (merge-env cfexp) cfexp)))

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
  (let ([env (cfexp-env cfe)])
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
        [(mk-union-cfexp? cfe) (gen-cfexp-word (pick-cfexp cfe) MAX-KLEENESTAR-REPS)]
        [else (gen-cfe-kleene-word cfe MAX-KLEENESTAR-REPS gen-cfexp-word)]))

(define-struct cfg (nts signa rules start) #:transparent)

(define (cfg->cfe G)

  (define (make-hash-table lox f)
    (foldl (λ (x h)
             (hash-set h x (f x)))
           (hash)
           lox))

  (define (explode string)
    (define (explode-helper string idx acc)
      (if (= idx 0)
          acc
          (explode-helper string (sub1 idx) (cons (string->symbol (substring string (sub1 idx) idx)) acc))))
    (explode-helper string (string-length string) '()))


  (define (make-cfexps-frm-rules rules singletons variables)

    (define (make-expression value)
      (if (hash-has-key? singletons value)
          (hash-ref singletons value)
          (hash-ref variables value)))
    
    (define (make-translation key value)
      (cond [(and (= (length value) 1)
                  (eq? (first value) EMP))
             (empty-cfexp)]
            [(= (length value) 1) (make-expression (first value))]
            [else (concat-cfexp (map (λ (v) (make-expression v)) value))]))
    
    (hash-map/copy rules (λ (k v)
                           (values k (if (> (length v) 1)
                                         (union-cfexp (map (λ (v) (make-translation k v)) v))
                                         (make-translation k (first v)))))))
  
  (let* ([nts (cfg-get-v G)]
         [alphabet (cfg-get-alphabet G)]
         [rules (make-hash-table nts (λ (x) (filter-map (λ (rule) (and (eq? (first rule) x)
                                                                       (explode (symbol->string (third rule))))) (cfg-get-rules G))))]
         [start (cfg-get-start G)]
         [singletons (make-hash-table alphabet singleton-cfexp)]
         [variables (make-hash-table nts var-cfexp)]
         [rules->cfexp (make-cfexps-frm-rules rules singletons variables)]
         [updated-bindings (hash-map/copy rules->cfexp (λ (k v)
                                  (begin
                                    (update-binding! (hash-ref variables k) k v)
                                    (values k (hash-ref variables k)))))])
     (hash-ref updated-bindings start)))
  

(define (cfe->cfg cfe)
 (define-struct pair-cfe (cfe-sym env) #:transparent)

  (define (get-symbol cfe)
    (cond [(mk-null-cfexp? cfe) 'null]
          [(mk-empty-cfexp? cfe) EMP]
          [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
          [(mk-var-cfexp? cfe) (mk-var-cfexp-cfe cfe)]
          [(mk-concat-cfexp? cfe) (map extract-data (mk-concat-cfexp-locfe cfe))]
          [(mk-union-cfexp? cfe)  (map extract-data (mk-union-cfexp-locfe cfe))]
          [else (get-symbol (mk-kleene-cfexp-cfe cfe))]))

  (define (get-cfe-symbol cfe)
    (cond [(mk-null-cfexp? cfe) 'null]
          [(mk-empty-cfexp? cfe) EMP]
          [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
          [(mk-var-cfexp? cfe) (extract-all-cfexp (cfexp-env cfe) (list (extract-data cfe)))]
          [(mk-concat-cfexp? cfe) (flatten (map get-cfe-symbol (mk-concat-cfexp-locfe cfe)))]
          [(mk-union-cfexp? cfe) (flatten (map get-cfe-symbol (mk-union-cfexp-locfe cfe)))]
          [else (get-cfe-symbol (mk-kleene-cfexp-cfe cfe))]))

  (define (extract-data cfe)
    (pair-cfe (get-symbol cfe) cfe))
  
  (define (base-case? cfe)
    (or (mk-null-cfexp? cfe)
        (mk-empty-cfexp? cfe)
        (mk-singleton-cfexp? cfe)))
  
  (define (extract-all-cfexp cfe-env acc)
    (foldl (λ (cfe acc) (flatten (cons (extract-all-cfexp (cfexp-env cfe) (list (extract-data cfe)))#;(if (not (base-case? cfe))
                                           (extract-all-cfexp (cfexp-env cfe) (list (extract-data cfe)))
                                           (extract-data cfe)) acc)))
             acc
             (flatten (hash-values cfe-env)))
    #;(flatten (hash-values cfe-env)))
  
  (let* ([all-cfes (flatten (extract-all-cfexp (cfexp-env cfe) (list (extract-data cfe))))])
    all-cfes))