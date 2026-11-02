#lang racket/base

(require "../constants.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
         "cfexp-transformations.rkt"         
         racket/contract/region
         racket/vector
         racket/list         
         racket/set)

(provide cfexp?
         null-cfexp
         empty-cfexp
         singleton-cfexp
         concat-cfexp
         union-cfexp
         (rename-out (mk-union-cfexp? union-cfexp?)
                     (mk-concat-cfexp? concat-cfexp?)
                     (mk-singleton-cfexp? singleton-cfexp?)
                     (mk-kleene-cfexp? kleenestar-cfexp?)
                     (mk-empty-cfexp? empty-cfexp?)
                     (mk-null-cfexp? null-cfexp?)
                     (kleene-cfexp kleenestar-cfexp)         
                     (mk-singleton-cfexp-char singleton-cfexp-a)
                     (mk-union-cfexp-locfe union-cfexp-cfes)
                     (mk-concat-cfexp-locfe concat-cfexp-cfes)
                     (mk-kleene-cfexp-cfe kleenestar-cfexp-c1))
         gen-cfexp-word          
         cfg->cfe
         cfe->cfg
         pda->cfe
         cfe->pda
         pick-cfexp
         ;printable-cfexp
         )

(define MAX-KLEENESTAR-LIMIT 20)

(define EMPTY-CHANCE .25)

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. box
;; 5. concat 
;; 6. union
;; 7. kleene

;; -> null-cfexp
;;Purpose: A wrapper to create a null-cfexp
(define (null-cfexp)
  (mk-null-cfexp))

;; -> empty-cfexp
;;Purpose: A wrapper to create a empty-cfexp
(define (empty-cfexp)
  (mk-empty-cfexp))

;; string -> singleton-cfexp
;;Purpose: A wrapper to create a singleton-cfexp
(define/contract (singleton-cfexp a-char)
  singleton-cfexp/c 
  (mk-singleton-cfexp a-char))

;; . cfexp -> concat-cfexp/null-cfexp/empty-cfexp/singleton-cfexp
;;Purpose: A wrapper to create a concat-cfexp unless all the given cfexps are empty-cfexp
(define/contract (concat-cfexp . cfexps)
  concat-cfexp/c
  ;;cfe -> cfe 
  ;;Purpose: If the given cfe is a union then it is put into a box otherwise nothing happens
  (define (unnest-unions cfe)
    (if (mk-union-cfexp? cfe)
        (box cfe)
        cfe))
  ;;(listof cfexp) -> boolean
  ;;Purpose: Determines if the given (listof cfexp) contains the null-cfexp
  (define (contains-null? locfe)
    (ormap mk-null-cfexp? locfe))
  (cond [(or (null? cfexps) (contains-null? cfexps)) (null-cfexp)] ;; no input cfes -> null
        [(andmap mk-empty-cfexp? cfexps) (empty-cfexp)] ;; only empty cfes -> empty
        [(is-length-one? cfexps) (car cfexps)] ;;only one cfe -> cfe
        [else (mk-concat-cfexp (list->vector (map unnest-unions cfexps)))])) ;;otherwise box any unboxed-union cfes -> concat

;; . cfexp -> union-cfexp/null-cfexp/empty-cfexp/singleton-cfexp
;;Purpose: A wrapper to create a union-cfexp unless all the given cfexps are empty-cfexp
(define/contract (union-cfexp . cfexps)
  union-cfexp/c
  (cond [(or (null? cfexps) (andmap mk-null-cfexp? cfexps)) (null-cfexp)] ;; no input cfes -> null
        [(andmap mk-empty-cfexp? cfexps) (empty-cfexp)] ;; only empty cfes -> empty
        [(is-length-one? cfexps) (car cfexps)] ;;only one cfe -> cfe
        [else (mk-union-cfexp (vector-append (list->vector (filter (compose1 not mk-union-cfexp?)
                                                                   cfexps)) ;;otherwise flatten nested unions -> union
                                             (foldl (λ (u-cfe acc)
                                                      (vector-append acc (mk-union-cfexp-locfe u-cfe)))
                                                    (vector)
                                                    (filter mk-union-cfexp? cfexps))))]))

;;cfexp -> Kleene-cfexp/empty-cfexp/null-cfexp
;;Purpose: A wrapper to create a Kleene-cfexp
(define/contract (kleene-cfexp cfe)
  kleene-cfexp/c
  (if (or (mk-null-cfexp? cfe)
          (mk-empty-cfexp? cfe))
      cfe
      (mk-kleene-cfexp cfe)))

;;(vectorof cfexp) -> boolean
;;Purpose: Determines if the given (vectorof cfexp) contains an empty-cfexp
(define (contains-empty? Vocfe)
  (for/or ([cfe (in-vector Vocfe)])
    (mk-empty-cfexp? cfe)))

;; (vectorof cfexp) --> cfexp
;; Purpose: Return a randomly chosen sub-cfexp from the given union-cfexp weigthed towards a non-empty-cfexp
(define (pick-cfexp cfexps)
  (if (contains-empty? cfexps)
      (let ([filtered-empties (vector-filter-not mk-empty-cfexp? cfexps)])
        (if (or (vector-empty? filtered-empties)
                (< (random) EMPTY-CHANCE))
            (empty-cfexp)
            (vector-ref filtered-empties (random (vector-length filtered-empties)))))
      (vector-ref cfexps (random (vector-length cfexps)))))

;;concat-cfexp --> word
;;Purpose: Returns the concatenation of the sub context-free expressions 
(define (gen-concat-word concat-cfexp gen-function reps)
  (for/fold ([word ""])
            ([cfe (in-vector (mk-concat-cfexp-locfe concat-cfexp))])
    (string-append word (gen-function cfe reps))))

;; natnum kleene-star-cfexp (cfexp --> word) --> word
;; Purpose: Generate a word of arbitrary length in [0..reps+1] using
;;          given context-free expression and the given word-generating function
(define (gen-cfe-kleene-word kleene-cfexp reps gen-function)
  (let [(lst-words (filter
                    (λ (w) (not (eq? w EMP)))
                    (flatten
                     (build-list
                      (random (add1 reps))
                      (λ (i) (gen-function (mk-kleene-cfexp-cfe kleene-cfexp) reps))))))]
    (if (null? lst-words) "" (apply string-append lst-words))))

;;string -> Boolean
;;Purpose: Determines if the given string is empty
(define (string-empty? str)
  (string=? str ""))

;;string -> (listof symbol)
;;Purpose: Converts the given string into a fsm word
(define (string->word str)
  (let ([end-idx (string-length str)])
    ;;natnum (listof symbol) -> (listof symbol)
    ;;Purpose: Converts the string into a fsm word
    (define (string->word-helper idx acc)
      (if (= idx end-idx)
          (reverse acc)
          (string->word-helper (add1 idx)
                               (cons (string->symbol (substring str idx (add1 idx))) acc))))
    (string->word-helper 0 '())))

;; cfe [natnum] -> word
;; Purpose: Generates a word using the given cfexp
(define/contract (gen-cfexp-word cfe [reps MAX-KLEENESTAR-LIMIT])
  gen-cfexp-word/c
  (define MAX-KLEENESTAR-REPS (if (null? reps) MAX-KLEENESTAR-LIMIT reps))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-cfexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (list (string->symbol (mk-singleton-cfexp-char cfe)))]
        [(box? cfe) (gen-cfexp-word (unbox cfe) MAX-KLEENESTAR-REPS)]
        [else (let ([res (gen-cfexp-word-helper cfe MAX-KLEENESTAR-REPS)])
                (if (string-empty? res)
                    EMP
                    (string->word res)))]))

;;cfexp natnum -> word
;;Purpose: Generates a word that is in the given cfexp's language
(define (gen-cfexp-word-helper cfe reps)
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-cfexp.")]
        [(mk-empty-cfexp? cfe) ""]
        [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word-helper reps)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word-helper (pick-cfexp (mk-union-cfexp-locfe cfe)) reps)]
        [(box? cfe) (gen-cfexp-word-helper (unbox cfe) reps)]
        [else (gen-cfe-kleene-word cfe reps gen-cfexp-word-helper)]))
      
;;cfexp [(setof cfe)] -> string
;;Purpose: Converts the given cfe into a string to make it readable
(define (printable-cfexp cfe #:seen[seen (set)]) 
  ;;(listof cfe) string (setof cfe) -> string
  ;;Purpose: Converts and appends all of the cfes in the given (listof cfe) 
  (define (printable-helper locfe connector seen)
    (cond [(is-length-one? locfe) (printable-cfexp (car locfe) #:seen #;(set-add seen (car locfe)) seen)]
          [else (let ([new-seen (set-add seen (car locfe))])
                  (string-append (printable-cfexp (car locfe) #:seen new-seen)
                                 connector
                                 (printable-helper (cdr locfe) connector new-seen)))]))

  ;;box (setof cfe) -> string
  ;;Purpose: Prints the variable 
  (define (printable-box lang-box seen)
    (printable-cfexp (unbox lang-box) #:seen seen))
  
  (define NULL-REGEXP-STRING "∅")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  #;(displayln (format "cfe: ~a\nseen: ~a\n\n" cfe seen))
  (cond [(mk-null-cfexp? cfe) NULL-REGEXP-STRING]
        [(mk-empty-cfexp? cfe) EMPTY-REGEXP-STRING]
        [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
        [(box? cfe) (if (set-member? seen cfe)
                        "x"
                        (string-append "" (printable-box cfe (set-add seen cfe))))]
        [(mk-concat-cfexp? cfe) (printable-helper (vector->list (mk-concat-cfexp-locfe cfe)) "" seen)]
        [(mk-union-cfexp? cfe) (string-append "(" (printable-helper (vector->list (mk-union-cfexp-locfe cfe)) " U " seen) ")")]
        [else (if (set-member? seen cfe)
                  ""
                  (string-append (printable-cfexp (mk-kleene-cfexp-cfe cfe) #:seen (set-add seen cfe)) "*"))]))


;;Moved into a different bc the functions (namely pda->cfe) are HUGE

;;CFE -> CFG
;;Purpose: Converts the given CFE into its equivalent CFG
(define/contract (cfe->cfg cfe)
  cfe->cfg/c
  (unchecked-cfe->cfg cfe))

;;CFG -> CFE
;;Purpose: Converts the given CFG into its equivalent CFE
(define/contract (cfg->cfe cfg)
  cfg->cfe/c
  (unchecked-cfg->cfe cfg))

;;CFE -> PDA
;;Purpose: Converts the given CFE into its equivalent PDA
(define/contract (cfe->pda cfe)
  cfe->pda/c
  (unchecked-cfe->pda cfe))

;;PDA -> cfe
;;Purpose: Converts the given PDA into its equivalent CFE
(define #;define/contract (pda->cfe pda)
  #;pda->cfe/c
  (unchecked-pda->cfe pda))

 