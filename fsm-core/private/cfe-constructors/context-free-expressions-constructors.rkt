#lang racket/base

(require "../constants.rkt"
         "../cfg-struct.rkt"
         (except-in "../pda.rkt" pda->spda)
         "../misc.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
         "cfexp-helpers.rkt"
         ;"construct-cfe-macro.rkt"
         racket/contract
         racket/vector
         racket/list
         racket/string
         racket/treelist
         racket/function
         racket/hash
         racket/set
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax))

(provide null-cfexp
         empty-cfexp
         singleton-cfexp
         concat-cfexp
         union-cfexp
         kleene-cfexp
         update-binding!
         gen-cfexp-word
         var-cfexp 
         cfg->cfe
         cfe->cfg
         pda->cfe
         cfe->pda
         printable-cfexp
         #;construct-cfe
         )

(define MAX-KLEENESTAR-LIMIT 10)

(define EMPTY-CHANCE .25)

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. variable
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

;; symbol -> singleton-cfexp
;;Purpose: A wrapper to create a singleton-cfexp
(define/contract (singleton-cfexp a-char)
  singleton-cfexp/c 
  (mk-singleton-cfexp a-char))

;;symbol -> variable-cfexp
;;Purpose: A wrapper to create a variable-cfexp
(define/contract (var-cfexp symbol)
  var-cfexp/c 
  (mk-var-cfexp (empty-cfexp-env) symbol #f))


(define (all-empty? locfe)
  (andmap mk-empty-cfexp? locfe))

(define (contains-null? locfe)
  (ormap mk-null-cfexp? locfe))

;; . cfexp -> concat-cfexp/empty-cfexp
;;Purpose: A wrapper to create a concat-cfexp unless all the given cfexps are empty-cfexp
(define #;define/contract (concat-cfexp . cfexps)
  #;concat-cfexp/c
  ;;cfe -> cfe 
  ;;Purpose: If the given cfe is a union then it is put into a box otherwise nothing happens
  (define (unnest-unions cfe)
  (if (mk-union-cfexp? cfe)
      (box cfe)
      cfe))
  (cond [(or (empty? cfexps) (contains-null? cfexps)) (null-cfexp)] ;; no input cfes -> null
        [(all-empty? cfexps) (empty-cfexp)] ;; only empty cfes -> empty
        [(= (length cfexps) 1) (identity (first cfexps))] ;;only one cfe -> cfe
        [else (mk-concat-cfexp (list->vector (map unnest-unions cfexps)))])) ;;otherwise box unboxed-union cfes -> concat


;; . cfexp -> union-cfexp/empty-cfexp
;;Purpose: A wrapper to create a union-cfexp unless all the given cfexps are empty-cfexp
(define #;define/contract (union-cfexp . cfexps)
  #;union-cfexp/c
  (cond [(empty? cfexps) (null-cfexp)] ;; no input cfes -> null
        [(all-empty? cfexps) (empty-cfexp)] ;; only empty cfes -> empty
        [(= (length cfexps) 1) (identity (first cfexps))] ;;only one cfe -> cfe
        [else (mk-union-cfexp (vector-append (list->vector (filter-not mk-union-cfexp? cfexps)) ;;otherwise flatten nested unions -> union
                                             (foldl (λ (u-cfe acc)
                                                      (vector-append acc (mk-union-cfexp-locfe u-cfe)))
                                                    (vector)
                                                    (filter mk-union-cfexp? cfexps))))]))

;;cfexp -> Kleene-cfexp/empty-cfexpmessage
;;Purpose: A wrapper to create a Kleene-cfexp
(define #;define/contract (kleene-cfexp cfe)
  #;kleene-cfexp/c
  (if (or (mk-null-cfexp? cfe)
          (mk-empty-cfexp? cfe))
      cfe
      (mk-kleene-cfexp cfe)))

;;cfe-id cfe -> env
;;Purpose: Creates an environment where the given cfe-id is the key and cfe is the value
(define (env-cfexp cfe-id binding)
  (let ([binding (if (vector? binding) binding (vector binding))])
    (hash cfe-id binding)))

;;var-cfexp symbol cfe -> void
;;Purpose: Creates a binding where the cfe is bound to the given var-cfexp's environment unless the binding is an empty-cfexp
(define/contract (update-binding! cfe bindee-id binding)
  update-binding!/c
  (let ([env (mk-var-cfexp-env cfe)])
    (begin
      (set! env (env-cfexp bindee-id (if (hash-has-key? env bindee-id)
                                         (vector-extend (hash-ref env bindee-id)
                                                        (add1 (vector-length (hash-ref env bindee-id)))
                                                        binding)
                                         binding)))
      (set-mk-var-cfexp-only-empty?! cfe (and (= (vector-length (hash-ref env bindee-id)) 1)
                                              (mk-empty-cfexp? (vector-ref (hash-ref env bindee-id) 0))))
      (set-mk-var-cfexp-env! cfe env)
      ;(set! cfe (mk-var-cfexp env bindee-id)) ;;may reinstate later, need to discuss whether a variable should overwrite
      ;; for instance (update-binding! (var-cfexp 'S) 'K (union-cfexp A S A))<---- shouldnt be allowed!
      )))


;;singleton-cfe -> word
;;Purpose: Extracts the singleton 
(define (convert-singleton singleton-cfexp) (mk-singleton-cfexp-char singleton-cfexp))

(define (contains-empty? Vocfe)
  (for/or ([cfe (in-vector Vocfe)])
    (mk-empty-cfexp? cfe)
    #;(or (mk-empty-cfexp? cfe)
        (and (mk-var-cfexp? cfe)
             (mk-var-cfexp-only-empty? cfe)))))

;; Vocfexp --> cfexp
;; Purpose: Return a randomly chosen sub-cfexp from the given union-cfexp weigthed towards a non-empty-cfexp
(define (pick-cfexp cfexps)
  (if (contains-empty? cfexps)
        (let ([filtered-empties (vector-filter-not mk-empty-cfexp? cfexps)])
          (if (or (vector-empty? filtered-empties)
                  (< (random) EMPTY-CHANCE))
            (empty-cfexp)
            (vector-ref filtered-empties (random (vector-length filtered-empties)))))
        (vector-ref cfexps (random (vector-length cfexps)))))

;;var-cfexp --> word
;;Purpose: Substitutes the given var-cfexp with it's environment bindings 
#;(define (substitute-var var-cfexp reps)
  (let* ([bindings (hash-ref (mk-var-cfexp-env var-cfexp) (mk-var-cfexp-cfe var-cfexp))])
    (gen-cfexp-word-helper (pick-cfexp bindings) reps)))

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
    (if (empty? lst-words) EMP (append-map append lst-words))))

(define (string-empty? str)
  (not (non-empty-string? str)))

(define (string->word str)
  (let ([end-idx (string-length str)])
    (define (string->word-helper idx acc)
      (if (= idx end-idx)
          (reverse acc)
          (string->word-helper (add1 idx)
                               (cons (string->symbol (substring str idx (add1 idx))) acc))))
    (string->word-helper 0 '())))

;; cfe [natnum] -> word
;; Purpose: Generates a word using 

(define #;define/contract (gen-cfexp-word cfe . reps)
  #;gen-cfexp-word/c
  (define MAX-KLEENESTAR-REPS (if (empty? reps) MAX-KLEENESTAR-LIMIT (first reps)))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (list (mk-singleton-cfexp-char cfe))]
        [(box? cfe) (gen-cfexp-word (unbox cfe) reps)]
        ;[(mk-kleene-cfexp? cfe) )]
        [else (let ([res (gen-cfexp-word-helper cfe MAX-KLEENESTAR-REPS)])
                #;(displayln res)
                (if (string-empty? res)
                    EMP
                    (string->word res)))]))

;;cfexp ;;natnum
;;Purpose: Generates a word that is in the given cfexp's language
(define (gen-cfexp-word-helper cfe reps)
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) ""]
        [(mk-singleton-cfexp? cfe) (convert-singleton cfe)]
        #;[(mk-var-cfexp? cfe) (substitute-var cfe reps)]
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
    (cond [(= (length locfe) 1) (printable-cfexp (first locfe) #:seen seen)]
          [else (let ([new-seen (set-add seen (first locfe))])
                  (string-append (printable-cfexp (first locfe) #:seen new-seen)
                               connector
                               (printable-helper (rest locfe) connector new-seen)))]))

  ;;var-cfexp (setof cfe) -> string
  ;;Purpose: Prints the variable 
  (define (printable-var var-cfe seen)
    (if (set-member? seen var-cfe)
        (symbol->string (mk-var-cfexp-cfe cfe))
        (string-append (symbol->string (mk-var-cfexp-cfe cfe))
                       " -> "
                       (printable-helper (vector->list (first (hash-values (mk-var-cfexp-env cfe))))
                                         "|"
                                         (set cfe)))))
  (define NULL-REGEXP-STRING "∅")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  (cond [(mk-null-cfexp? cfe) NULL-REGEXP-STRING]
        [(mk-empty-cfexp? cfe) EMPTY-REGEXP-STRING]
        [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
        [(mk-var-cfexp? cfe) (printable-var cfe (if (set-empty? seen)
                                                    (set)
                                                    seen))]
        [(mk-concat-cfexp? cfe) (printable-helper (vector->list (mk-concat-cfexp-locfe cfe)) "" seen)
                                #;(string-append "(" (printable-helper (vector->list (mk-concat-cfexp-locfe cfe)) "" seen) ")")]
        [(mk-union-cfexp? cfe) (printable-helper (vector->list (mk-union-cfexp-locfe cfe)) " | "#;" ∪ " seen)
                               #;(string-append "(" (printable-helper (vector->list (mk-union-cfexp-locfe cfe)) " | "#;" ∪ " seen) ")")]
        [else (string-append (printable-cfexp (mk-kleene-cfexp cfe)) "*")]))
  

;;context-free grammar -> cfe
;;Purpose: Converts the given cfg its equivalent cfe
(define #;define/contract (cfg->cfe G)
 #;cfg->cfe/c
  ;;(listof X) (X -> Y) -> (hash X . Y)
  ;;Purpose: Creates a hash table using the given (listof x) and function where x is a key and (f x) is the value
  (define (make-hash-table lox f)
    (foldl (λ (x h)
             (hash-set h x (f x)))
           (hash)
           lox))

  ;;A lang-box is either:
  ;;1. (box (void))
  ;;2. (box cfe)
  
  ;;SYM is either EMP U (grammar-sigma G) U (grammar-nts G)
  
  ;;A CFG-RHS is either:
  ;;1. (list SYM) 
  ;;2. non-empty (listof SYM)
  
  ;;(hash nts . CFG-RHS) (hash symbol . singleton-cfe)) (hash nts . lang-box) -> (hash nts . cfe))
  ;;Purpose: Converts the RHS of cfg rules into cfes
  (define (make-cfexps-frm-rules rules singletons lang-boxes)
    ;;SYM -> cfe
    ;;Purpose: Matches the given symbol with the corresponding cfe
    (define (convert-to-expression portion-of-RHS)
      (cond [(eq? portion-of-RHS EMP) (empty-cfexp)]
            [(hash-has-key? singletons portion-of-RHS) (hash-ref singletons portion-of-RHS)]
            [(hash-has-key? lang-boxes portion-of-RHS) (hash-ref lang-boxes portion-of-RHS)
             #;(let* ([future-var-binding (hash-ref rules RHS-of-rule)]
                    [only-empty? (and (= (length future-var-binding) 1)
                                      (equal? (first future-var-binding) (list EMP)))])
               (if only-empty?
                   (empty-cfexp)
                   (hash-ref variables RHS-of-rule)))]
            [else (error (format "unreadable RHS: ~a" portion-of-RHS))]))
    ;;CFG-RHS -> cfe
    ;;Purpose: Translates the given CFG-RHS into its corresponding cfe
    (define (rule->expression RHS-of-rule)
      (if (= (length RHS-of-rule) 1)
          (convert-to-expression (first RHS-of-rule))
          (apply concat-cfexp (map (λ (sym) (convert-to-expression sym)) RHS-of-rule))))
    (hash-map/copy rules (λ (nts RHS)
                           (values nts (cond [(empty? RHS) (error (format "invalid RHS from nt: ~s" nts))]
                                             [(= (length RHS) 1) (rule->expression (first RHS))]
                                             [else (apply union-cfexp (map (λ (rule) (rule->expression rule)) RHS))])))))
  
  (let* ([nts (cfg-get-v G)]
         [rules (make-hash-table nts (λ (nt) (filter-map (λ (rule)
                                                           (and (eq? (first rule) nt)
                                                                (symbol->fsmlos (third rule))))
                                                         (cfg-get-rules G))))]
         [start (cfg-get-start G)]
         [singletons (make-hash-table (cfg-get-alphabet G) (λ (sig)
                                                             (singleton-cfexp (symbol->string sig))))]
         [lang-boxes (make-hash-table nts (λ (x) (box (void))))]
         [rules->cfexp (make-cfexps-frm-rules rules singletons lang-boxes)]
         [updated-bindings (hash-map/copy rules->cfexp (λ (key value)
                                                         (begin
                                                           (set-box! (hash-ref lang-boxes key) value)
                                                           (values key (hash-ref lang-boxes key)))))])
     (hash-ref updated-bindings start)))

;;cfe -> cfg
;;Purpose: Converts the given cfe into its corresponding cfg
(define #;define/contract (cfe->cfg cfe)
  #;cfe->cfg/c
  ;;vars    | the accumulated variables found from traversing the given cfe  | (listof var-cfexp)
  ;;singles | the accumulated singletons found from traversing the given cfe | (listof singleton-cfexp)
  (struct extraction-results (lang-boxes singles) #:transparent)

  (define qempty? treelist-empty?)

  (define E-QUEUE empty-treelist) 

  ;; (qof X) → X throws error
  ;; Purpose: Return first X of the given queue
  (define (qfirst a-qox)
    (if (qempty? a-qox)
        (error "qfirst applied to an empty queue")
        (treelist-first a-qox)))

  ;; (tllistof X) (qof X) → (qof X)
  ;; Purpose: Add the given list of X to the given queue of X
  (define (enqueue a-lox a-qox) (treelist-append a-qox a-lox))

  ;; (qof X) → (qof X) throws error
  ;; Purpose: Return the rest of the given queue
  (define (dequeue a-qox)
    (if (qempty? a-qox)
        (error "dequeue applied to an empty queue")
        (treelist-rest a-qox)))
  ;;natnum -> (listof nt)
  ;;Purpose: Generates natnum amount of nts
  (define (gen-nts num)
    (for/fold ([nts '()])
              ([x (in-range num)])
      (cons (gen-nt nts) nts)))

  (define (tl-foldl f acc tl)
    (if (treelist-empty? tl)
        acc
        (tl-foldl f (f (treelist-first tl) acc) (treelist-rest tl))))

  ;;cfe -> cfe
  ;;Purpose: Updates the cfe to be bound to a box if it is not already a box
  (define (update-cfe cfe)
    (if (or (mk-kleene-cfexp? cfe) (box? cfe))
        cfe
        (let ([S (box (void))])
          (begin
            (set-box! S cfe)
            S))))

  
  ;;cfe -> extraction-results
  ;;Purpose: Extracts all var-cfexp and singleton-cfexp from the given cfe
  (define (extract-var-and-singles-cfe cfe)
    (let ([init-queue (tl-foldl (λ (env acc)
                               (enqueue acc (treelist env)))
                             E-QUEUE
                             (extract-cfe-data cfe))])
      (extract-var-and-singles init-queue
                               (update-extraction-results cfe (extraction-results '() '()))
                               (set cfe))))

  ;;cfe extraction-results -> extraction-results
  ;;Purpose: Updates the given extraction-results to add the given cfe if it is a singleton or variable
  (define (update-extraction-results cfe extract-res)
    (cond [(or (mk-kleene-cfexp? cfe)
               (box? cfe)) (struct-copy extraction-results
                                            extract-res
                                            [lang-boxes (cons cfe (extraction-results-lang-boxes extract-res))])]
          [(mk-singleton-cfexp? cfe) (struct-copy extraction-results
                                                  extract-res
                                                  [singles (cons cfe (extraction-results-singles extract-res))])]
          [else extract-res]))

  ;;(queueof cfe) extraction-results (listof cfe) -> extraction-results
  ;;Purpose: Extracts the cfe and adds it to the extraction-results if its a singleton or variable
  (define (extract-var-and-singles qocfe extract-res visited)
    (if (qempty? qocfe)
        extract-res
        (let* ([cfe (qfirst qocfe)]
               [cfes-to-add (extract-cfe-data cfe)]
               [new-queue (enqueue (dequeue qocfe)
                                   (treelist-filter (λ (cfe) (not (set-member? visited cfe))) cfes-to-add))]
               [new-acc (update-extraction-results cfe extract-res)]
               [new-visited (set-add visited cfe)])
          (extract-var-and-singles new-queue new-acc new-visited))))

  ;;cfe -> (listof cfe)
  ;;Purpose: Extracts the sub-expressions from the given cfe
  (define (extract-cfe-data cfe)
    (cond [(mk-concat-cfexp? cfe) (vector->treelist (mk-concat-cfexp-locfe cfe))]
          [(mk-union-cfexp? cfe) (vector->treelist (mk-union-cfexp-locfe cfe))]
          [(mk-kleene-cfexp? cfe) (treelist (mk-kleene-cfexp-cfe cfe))]
          [(box? cfe) (treelist (unbox cfe))]
          [else empty-treelist]))

  ;;(listof lang-boxes) (hash old-nt . new-nt) (listof rule) -> (listof rule)
  ;;Purpose: Converts every var-cfexp into the corresponding grammar rule
  (define (lang-boxes->rules loLabox new-nts)
    ;;nonterminal (queueof cfe) (listof rule) -> (listof rule)
    ;;Purpose: Converts each cf in the (queueof cfe) into the proper grammar rules
    (define (remake-rules nt rules-to-convert finished-rules)
      
      ;;non-terminal cfe -> rule
      ;;Purpose: Converts the cfe into a grammar rule using the given non-terminal
      (define (cfe->rule nt cfe)
        ;;cfe -> cfe
        ;;Purpose: converts the cfe into a grammar production rule
        ;;Assumption: The given cfe is NOT bound to a box and has to be concatenated a rule cfe that is bound to a box 
        (define (convert-rhs cfe)
          (cond [(mk-empty-cfexp? cfe) EMP]
                [(mk-singleton-cfexp? cfe) (string->symbol (mk-singleton-cfexp-char cfe))]
                [(box? cfe) (hash-ref new-nts cfe #;(unbox cfe))]
                [(mk-concat-cfexp? cfe) (string->symbol #;(construct-RHS cfe)
                                                        (tl-foldl (λ (cfe acc)
                                                                    (string-append
                                                                     (if (mk-singleton-cfexp? cfe)
                                                                         (mk-singleton-cfexp-char cfe)
                                                                         (symbol->string (hash-ref new-nts cfe)))
                                                                     acc))
                                                                  ""
                                                                  (treelist-reverse (vector->treelist (mk-concat-cfexp-locfe cfe)))))]
                [(mk-union-cfexp? cfe)
                 ]
                [else (error (format "unsuitable cfe ~a" cfe))]))
        ;;if union found in concat split union and make concat using every branch
        (let ([RHS (cond [(mk-empty-cfexp? cfe) EMP]
                         [(mk-singleton-cfexp? cfe) (string->symbol (mk-singleton-cfexp-char cfe))]
                         [(box? cfe) (hash-ref new-nts cfe #;(unbox cfe))]
                         [(mk-concat-cfexp? cfe)
                          (string->symbol (tl-foldl (λ (cfe acc)
                                                      (string-append
                                                       (cond [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
                                                             [(or (mk-kleene-cfexp? cfe)
                                                                  (box? cfe))
                                                              (symbol->string (hash-ref new-nts cfe))] ;;sub with NT
                                                             [else (symbol->string (convert-rhs cfe))]) 
                                                       acc))
                                                    ""
                                                    (treelist-reverse (vector->treelist (mk-concat-cfexp-locfe cfe)))))]
                         [(mk-kleene-cfexp? cfe) (string->symbol (string-append
                                                                  (let [(cfe (mk-kleene-cfexp-cfe cfe))]
                                                                    (cond [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
                                                                          [(or (mk-kleene-cfexp? cfe)
                                                                               (box? cfe))
                                                                           (symbol->string (hash-ref new-nts cfe))] ;;sub with NT
                                                                          [else (symbol->string (convert-rhs cfe))])) 
                                                                  (symbol->string (hash-ref new-nts cfe))))]
                         [else (error (format "unsuitable cfe ~a" cfe))])])
          (list nt ARROW RHS)))
      (if (qempty? rules-to-convert)
          finished-rules
          (let ([cfe (qfirst rules-to-convert)])
            (cond [(mk-union-cfexp? cfe)
                   (remake-rules nt (enqueue (dequeue rules-to-convert) (vector->treelist (mk-union-cfexp-locfe cfe))) finished-rules)]
                  [(mk-kleene-cfexp? cfe)
                   (remake-rules nt (dequeue rules-to-convert) (cons (list nt ARROW EMP) (cons (cfe->rule nt cfe) finished-rules)))]
                  [else (remake-rules nt (dequeue rules-to-convert) (cons (cfe->rule nt cfe) finished-rules))]))))
    (foldl (λ (lang-box res)
             (append (remake-rules (hash-ref new-nts lang-box)
                                   (treelist (if (box? lang-box) (unbox lang-box) lang-box))
                                   '())
                     res))
           '()
           loLabox))
  (let* ([cfe (update-cfe cfe)]
         [extracted-components (extract-var-and-singles-cfe cfe)]
         [lang-boxes (extraction-results-lang-boxes extracted-components)]
         [new-nts (foldl (λ (nt lang-box acc)
                           (hash-set acc lang-box nt))
                         (hash)
                         (gen-nts (length lang-boxes))
                         lang-boxes)]
         [singletons (foldl (λ (single acc)
                              (set-add acc ((compose1 string->symbol mk-singleton-cfexp-char) single)))
                            (set)
                            (extraction-results-singles extracted-components))]
         [alphabet (set->list singletons)]
         [rules (lang-boxes->rules lang-boxes new-nts)]
         [nts (hash-values new-nts)]
         [starting-nt (hash-ref new-nts cfe)])
    (make-unchecked-cfg nts alphabet rules starting-nt)
    #;(values lang-boxes "newline" new-nts
            "newline" (hash-keys new-nts))))

;; pda -> cfe
;;Purpose: Converts the given pda into a cfe
(define #;define/contract (pda->cfe pda)
  #;pda->cfe/c

  ;;nts   | the non-terminals for the given CFG     | (listof non-terminal)
  ;;sigma | the alphabet for the given CFG          | (listof symbol)
  ;;rules | the productions rules for the given CFG | (listof cfg-rule)
  ;;start | the starting nt of the given CFG        | non-terminal
  (struct CFG (nts sigma rules start) #:transparent)

  ;;cfg -> CFG
  ;;Purpose: Converts the given cfg into a CFG
  (define (unchecked->cfg G)
    (CFG (cfg-get-v G) (cfg-get-alphabet G) (cfg-get-the-rules G) (cfg-get-start G)))

  ;;CFG -> cfg
  ;;Purpose: Converts the given CFG into a cfg
  (define (cfg->unchecked G)
    (cfg (CFG-nts G) (CFG-sigma G) (CFG-rules G) (CFG-start G)))
  
  ;;CFG -> (hash nt . nt)
  ;;Purpose: Creates a hash table with all the nts and new respective name  
  (define (rename-nts G)
  
    ;;(listof nt) (listof nt) (hash nt . nt)
    ;;Purpose: Renames the given (listof nt) and pairs the each nt with a new name in a hash
    (define (rename-nts-helper old-nts new-nts acc)
      (if (empty? old-nts)
          acc
          (let* ([translated-nt (gen-nt new-nts)]
                 [new-acc (hash-set acc (first old-nts) translated-nt)]
                 [new-nts (cons translated-nt new-nts)])
            (rename-nts-helper (rest old-nts) new-nts new-acc))))
    (rename-nts-helper (CFG-nts G) '() (hash)))

  ;;CFG (hash nt . nt) -> cfg
  ;;Purpose: Rebuilds the cfg by renaming all of the nts and remaking the 
  (define (rebuild-cfg improper-cfg nts-mapping)
    (let ([sigma (CFG-sigma improper-cfg)])
      (make-unchecked-cfg (hash-values nts-mapping)
                          sigma
                          (map (λ (rule)
                                 (list (hash-ref nts-mapping (cfg-rule-lhs rule))
                                       ARROW
                                       (let ([RHS (cfg-rule-rhs rule)])
                                         (los->symbol (map (λ (r)
                                                             (if (member r (cons EMP sigma))
                                                                 r
                                                                 (hash-ref nts-mapping r)))
                                                           RHS)))))
                               (CFG-rules improper-cfg))
                          (hash-ref nts-mapping (CFG-start improper-cfg)))))

  ;;cfg -> cfg
  ;;Purpose: minimizes the given cfg by removing nts and rules that aren't needed
  (define (minimize-cfg G)
    ;;X (listof Y) -> boolean
    ;;Purpose: Determines if the given X is in the (listof Y)
    (define (member? X lst)
      (ormap (λ (y) (eq? X y)) lst))
  
    ;;CFG -> cfg
    ;;Purpose: Minimizes the given CFG by repeatedly removing rules that can't
    ;;         be used the grammar until the same grammar is created twice
    (define (clean-up-cfg cfg acc)

      ;;CFG -> CFG
      ;;Purpose: Cleans up the given CFG's rules and nts by removing rules that cannot be generated
      ;;         or contain nts that produce nothing
      (define (clean-up-cfg-rules-and-nts G)

        ;;symbol (listof nt) (listof cfg-rule) (listof symbol) -> (listof cfg-rule)
        ;;Purpose: Remvoes rules that dont have produce an nt that has no production rule
        ;;         (e.i a rule that produces a nt on the RHS but doesnt have a LHS substitution)
        (define (remove-useless-rules start nts rules sigma)

          ;;(listof nt) (listof cfg-rule) (listof symbol) (listof cfg-rule) -> (listof cfg-rule)
          ;;Purpose: Removes that produces a nt on the RHS but doesnt have a LHS substitution
          (define (remove-rules-without-nts nts rules sigma acc)
    
            ;;(listof cfg-rule) (listof cfg-rule) -> Boolean
            ;;Purpose: Determines if the first given (listof cfg-rule) is the same as the second given (listof cfg-rule)
            (define (same-rules? rules1 rules2)
              (and (andmap (λ (rule) (member? rule rules2)) rules1)
                   (andmap (λ (rule) (member? rule rules1)) rules2)))
            (if (and (>= (length acc) 2)
                     (same-rules? (first acc) (second acc)))
                (first acc)
                (let* ([new-rules (filter (λ (rule)
                                            (andmap (λ (rhs)
                                                      (or (member? rhs sigma)
                                                          (member? rhs nts)))
                                                    (cfg-rule-rhs rule)))
                                          rules)]
                       [new-nts (map cfg-rule-lhs new-rules)])
                  (remove-rules-without-nts new-nts new-rules sigma (cons new-rules acc)))))
  
          ;;(listof cfg-rules) (listof cfg-rules) -> (listof cfg-rules)
          ;;Purpose: Removes rules whose LHS nt only produces self-loops 
          (define (remove-only-self-loops rules new-rules)
            (if (empty? rules)
                new-rules
                (let* ([rule-lhs (cfg-rule-lhs (first rules))]
                       [related-rules (filter (λ (rule) (eq? (cfg-rule-lhs rule) rule-lhs)) rules)]
                       [only-self-loop? (or (and (= (length related-rules) 1)
                                                 (not (member? rule-lhs (cfg-rule-rhs (first related-rules)))))
                                            (and (> (length related-rules) 1)
                                                 (ormap (λ (rule) (not (member? rule-lhs (cfg-rule-rhs rule)))) related-rules)))])
                  (if (not only-self-loop?)
                      (remove-only-self-loops (rest rules) new-rules)
                      (remove-only-self-loops (rest rules) (cons (first rules) new-rules))))))
  
          (let* ([rhs-nts (remove-duplicates (append-map cfg-rule-rhs rules))]
                 [rules-that-can-be-generated (filter (λ (rule)
                                                        (or (eq? (cfg-rule-lhs rule) start)
                                                            (member? (cfg-rule-lhs rule) rhs-nts)))
                                                      rules)]
                 [lhs-nts (map cfg-rule-lhs rules-that-can-be-generated)])
            (remove-only-self-loops (remove-rules-without-nts lhs-nts rules-that-can-be-generated sigma '()) '())))

        ;;(listof cfg-rule) (listof nt) (listof nt) -> (listof nt)
        ;;Purpose: Extrascts rules who rhs contain needed nts
        (define (extract-needed-nts rules needed seen)
          (let* ([new-needed-nts (filter-not (λ (nt)
                                               (member? nt seen))
                                             (remove-duplicates (filter-map (λ (rule)
                                                                              (and (ormap (λ (rhs) (member? rhs needed))
                                                                                          (cfg-rule-rhs rule))
                                                                                   (cfg-rule-lhs rule)))
                                                                            rules)))])
            (if (empty? new-needed-nts)
                seen 
                (extract-needed-nts rules (append needed new-needed-nts) (append seen new-needed-nts)))))
  
        (let* ([sigma (cons EMP (CFG-sigma G))]
               [needed-nts (extract-needed-nts (remove-useless-rules (CFG-start G) (CFG-nts G) (CFG-rules G) sigma) sigma '())]
               [rules-that-contain-needed-nts (filter (λ (rule)
                                                        (member? (cfg-rule-lhs rule) needed-nts))
                                                      (CFG-rules G))])
          (CFG needed-nts
               (CFG-sigma G)
               (filter (λ (rule)
                         (andmap (λ (rhs) (or (member? rhs sigma)
                                              (member? rhs needed-nts))) (cfg-rule-rhs rule)))
                       rules-that-contain-needed-nts)
               (CFG-start G))))
      (if (and (>= (length acc) 2)
               (equal? (first acc) (second acc)))
          (cfg->unchecked (first acc))
          (let ([new-cfg (clean-up-cfg-rules-and-nts cfg)])
            (clean-up-cfg new-cfg (cons new-cfg acc)))))
    (clean-up-cfg G '()))

  ;;(listof X) (X -> Y) -> (hash X . Y)
  ;;Purpose: Creates a hash table using the given (listof x) and function where x is a key and (f x) is the value
  (define (make-hash-table lox f)
    (foldl (λ (x h)
             (hash-set h x (f x)))
           (hash)
           lox))

  (define (simplify-rules curr-rules acc-rules)
    (if (and (>= (length acc-rules) 2)
             (equal? (first acc-rules) (second acc-rules)))
        (hash-filter (first acc-rules)
                     (λ (k v)
                       (andmap (λ (r) (not (or (empty? r)
                                          (equal? (list EMP) r)))) v)))
        (let* ([only-empty-rules
                (hash-filter curr-rules (λ (k v)
                                     (andmap (λ (r) (or (empty? r)
                                                        (equal? (list EMP) r))) v)))]
               [e-nts (hash-keys only-empty-rules)]
               [new-rules (hash-map/copy curr-rules (λ (k v)
                                       (if (ormap (λ (rule)
                                                    (ormap (λ (r)
                                                             (hash-has-key? only-empty-rules r)) rule)) v)
                                           (values k (map (λ (val) (filter-not (λ (r) (member r e-nts)) val)) v))
                                           (values k v))))])
          (simplify-rules new-rules (cons new-rules acc-rules)))))
  
  (let* ([G (unchecked->cfg (pda2cfg pda))]
         [renamed-nts-mapping (rename-nts G)]
         [renamed-cfg (unchecked->cfg (rebuild-cfg G renamed-nts-mapping))]
         [proper-cfg  (minimize-cfg renamed-cfg)]
         [new-rules (make-hash-table (cfg-get-v proper-cfg) (λ (nt) (filter-map (λ (rule)
                                                           (and (eq? (first rule) nt)
                                                                (symbol->fsmlos (third rule))))
                                                         (cfg-get-rules proper-cfg))))]
         [only-empty-rules (hash-filter new-rules (λ (k v)
                                     (or (empty? v)
                                         (andmap (λ (r) (equal? (list EMP) r)) v))))]
         [e-nts (hash-keys only-empty-rules)]
         [simp-rules (simplify-rules new-rules '())]
         [sub-only-rules (hash-filter simp-rules (λ (k v)
                                      (andmap (λ (r)
                                                (and (= (length r) 1)
                                                     (ormap (λ (el)
                                                            (member el (hash-keys simp-rules))) r))) v)))]
         [sub-nts (hash-keys sub-only-rules)]
         [simp-rules2 (hash-map/copy simp-rules (λ (k v)
                                       (if (ormap (λ (rule)
                                                    (ormap (λ (r)
                                                             (hash-has-key? sub-only-rules r)) rule)) v)
                                           (values k (map (λ (val) (map (λ (r)
                                                                          (if (hash-has-key? sub-only-rules r)
                                                                       (first (flatten (hash-ref sub-only-rules r)))
                                                                       r)) val)) v))
                                           (values k v))))]
         [startt (first (filter (λ (x) (eq? x (cfg-get-start proper-cfg))) (hash-keys simp-rules2)))]
         [final-rules (hash-filter simp-rules2 (λ (k v)
                                      (or (eq? k startt)
                                           (not (member k sub-nts)))))]
         [usable-rules (append-map (λ (lhs)
                          (map (λ (rhs)
                                 (cfg-rule lhs rhs))
                               (hash-ref final-rules lhs)))
                        (hash-keys final-rules))])
    #;(values ;proper-cfg
            new-rules
            only-empty-rules
            simp-rules
            sub-only-rules
            simp-rules2
            final-rules
            (append-map (λ (lhs)
                          (map (λ (rhs)
                                 (cfg-rule lhs rhs))
                               (hash-ref final-rules lhs)))
                        (hash-keys final-rules)))
            
            #;(cfg (CFG-nts G) (CFG-sigma G) (append-map (λ (lhs)
                                                         (map (λ (rh)
                                                                (cfg-rule lhs rh))
                                                              (hash-ref lrhs))
                                                       (hash-keys final-rules)
                                                       (hash-values final-rules)) (CFG-start G)))
       final-rules #;(cfg->cfe (cfg (hash-keys final-rules) (cfg-get-alphabet proper-cfg) usable-rules startt))                            
    #;(cfg->cfe proper-cfg)))

;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define #;define/contract (cfe->pda cfe)
 #;cfe->pda/c
  (cfg->pda (cfe->cfg cfe)))