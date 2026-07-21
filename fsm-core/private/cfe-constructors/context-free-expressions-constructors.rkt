#lang racket/base

(require "../../../sm-graph.rkt"
         "../constants.rkt"
         "../cfg-struct.rkt"
         (except-in "../pda.rkt" pda->spda)
         "../misc.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
         "cfexp-helpers.rkt"
         racket/contract/region
         racket/vector
         racket/list
         racket/treelist
         racket/hash
         racket/set
         racket/pretty
         racket/format
         racket/function)

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
         printable-cfexp
         #;make-smallest-paths
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

;;(listof X) -> boolean
;;Purpose: Determines if the (listof X) is of length 1
(define (is-length-one? lox)
  (and (not (null? lox))
       (null? (cdr lox))))

;;(listof cfexp) -> boolean
;;Purpose: Determines if the given (listof cfexp) contains the null-cfexp
(define (contains-null? locfe)
  (ormap mk-null-cfexp? locfe))

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
(define (string->word2 str)

  (define (string-first str)
    (substring str 0 1))
  
  (define (string-rest str)
    (substring str 1))
 
    ;;natnum (listof symbol) -> (listof symbol)
    ;;Purpose: Converts the string into a fsm word
    (define (string->word-helper str acc)
      (if (string-empty? str)
          (reverse acc)
          (string->word-helper (string-rest str)
                               (cons (string->symbol (string-first str)) acc))))
    (string->word-helper str '()))


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
;; Purpose: Generates a word using 
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

;;cfexp ;;natnum
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
  (displayln (format "cfe: ~a\nseen: ~a\n\n" cfe seen))
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
  

;;context-free grammar -> cfe
;;Purpose: Converts the given cfg its equivalent cfe
(define/contract (cfg->cfe G)
  cfg->cfe/c
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
            [(hash-has-key? lang-boxes portion-of-RHS) (hash-ref lang-boxes portion-of-RHS)]
            [else (error (format "unreadable RHS: ~a" portion-of-RHS))]))
    ;;CFG-RHS -> cfe
    ;;Purpose: Translates the given CFG-RHS into its corresponding cfe
    (define (rule->expression RHS-of-rule)
      (if (= (length RHS-of-rule) 1)
          (convert-to-expression (car RHS-of-rule))
          (apply concat-cfexp (map (λ (sym) (convert-to-expression sym)) RHS-of-rule))))
    (hash-map/copy rules (λ (nts RHS)
                           (values nts (cond [(null? RHS) (error (format "invalid RHS from nt: ~s" nts))]
                                             [(= (length RHS) 1) (rule->expression (car RHS))]
                                             [else (apply union-cfexp (map (λ (rule) (rule->expression rule)) RHS))])))))
  
  (let ([rules (cfg-get-rules G)])
    (if (empty? rules)
        (null-cfexp)
        (let* ([nts (cfg-get-v G)]
               [rules-hash (make-hash-table nts (λ (nt) (filter-map (λ (rule)
                                                                 (and (eq? (car rule) nt)
                                                                      (symbol->fsmlos (caddr rule))))
                                                               rules)))]
               [start (cfg-get-start G)]
               [singletons (make-hash-table (cfg-get-alphabet G) (λ (sig)
                                                                   (singleton-cfexp (symbol->string sig))))]
               [lang-boxes (make-hash-table nts (λ (x) (box (void))))]
               [rules->cfexp (make-cfexps-frm-rules rules-hash singletons lang-boxes)]
               [updated-bindings (hash-map/copy rules->cfexp (λ (key value)
                                                               (begin
                                                                 (set-box! (hash-ref lang-boxes key) value)
                                                                 (values key (hash-ref lang-boxes key)))))])
          (hash-ref updated-bindings start)))))

;;cfe -> cfg
;;Purpose: Converts the given cfe into its corresponding cfg
(define/contract (cfe->cfg cfe)
  cfe->cfg/c
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

  ;;(X -> Y) Z (treelistof X) -> Z
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
                [(box? cfe) (hash-ref new-nts cfe)]
                [(mk-concat-cfexp? cfe) (string->symbol (tl-foldl (λ (cfe acc)
                                                                    (string-append
                                                                     (if (mk-singleton-cfexp? cfe)
                                                                         (mk-singleton-cfexp-char cfe)
                                                                         (symbol->string (hash-ref new-nts cfe)))
                                                                     acc))
                                                                  ""
                                                                  (treelist-reverse
                                                                   (vector->treelist (mk-concat-cfexp-locfe cfe)))))]
                [else (error (format "unsuitable cfe ~a" cfe))]))
        ;;if union found in concat split union and make concat using every branch
        (let ([RHS (cond [(mk-empty-cfexp? cfe) EMP]
                         [(mk-singleton-cfexp? cfe) (string->symbol (mk-singleton-cfexp-char cfe))]
                         [(box? cfe) (hash-ref new-nts cfe)]
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
                   (remake-rules nt (enqueue (dequeue rules-to-convert)
                                             (vector->treelist (mk-union-cfexp-locfe cfe))) finished-rules)]
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
  (if (mk-null-cfexp? cfe)
      (make-unchecked-cfg '(S) '() '() 'S)
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
        (make-unchecked-cfg nts alphabet rules starting-nt))))

;; pda -> cfe
;;Purpose: Converts the given pda into a cfe
(define #;define/contract (pda->cfe P)
  #;pda->cfe/c
  #|
  pda-struct is a structural representation of a pda
  states | The states for the given pda => (listof states)
  sigma  | The alphabet that the given pda works over => (listof symbol)
  gamma  | The stack alphabet that given pda works over => (listof symbol)
  start  | The starting state => symbol
  finals | The final states => symbol
  rules  | The transition relation for the given pda => (listof pda-rule)
  |#
  (struct pda (states sigma gamma start final rules) #:transparent)

  #|
  pda-rule is a structural representation of a pda rule
  source | The state the rule is coming from => symbol
  action | The action the pda takes when using the rule => pda-action
  destin | The state the rule transitions to => symbol
  tag    | The cfe-template for the given rule => symbol / cfe-template
  |#
  (struct pda-rule (source action destin) #:transparent)

  #|
  a pda-action is a structural representation of a pda action
  read | The element that the pda reads => symbol
  pop  | The element(s) that the pda pops of the stack => symbol / (listof symbol)
  push | The element(s) that the pda pushes to the stack => symbol / (listof symbol)
  |#
  (struct pda-action (read pop push) #:transparent)

  #|
  A cfe-template is an annotation for a pda-rule in preparation to be converted to a cfe
  A cfe-template is either:
  1. kleene
  2. union
  3. concat
  4. empty
  5. singleton
  |#
  ;;kleene is a cfe-template
  ;;rule | the rule to be annotated with a Kleenestar
  (struct kleene (rule) #:transparent)
  ;;union is a cfe-template
  ;;rule | the rule to be annotated with a union
  (struct union (rules) #:transparent)
  ;;concat is a cfe-template
  ;;rule | the rule to be annotated with a concatenation
  (struct concat (rules) #:transparent)
  ;;empty is a cfe-template
  ;;rule | the rule to be annotated with a empty
  (struct empty (rule) #:transparent)
  ;;singleton is a cfe-template
  ;;rule | the rule to be annotated with a singleton
  (struct singleton (rule) #:transparent)

  ;;(list (list state symbol los) (list state los)) -> pda-rule
  ;;Purpose: Converts the given pda rule into a pda-rule struct
  (define (rule->struct rule)
    ;;symbol los los -> pda-action
    ;;Purpose: Creates a pda action from the given input
    (define (make-pda-action read pop push)
      (pda-action read pop push))
    ;;pda-rule -> cfe-template
    ;;Purpose: Converts the given pda-rule to a cfe-template
    (define (rules->cfe pda-action)
      ;;pda-rule -> Boolean
      ;;Purpose: Determines if the given pda-rule is an empty transition
      (define (e-transition? action)
        (and (eq? EMP (pda-action-read action))
             (eq? EMP (pda-action-pop action))
             (eq? EMP (pda-action-push action))))
      (if (e-transition? pda-action)
          (empty pda-action) 
          (singleton pda-action)))
    (pda-rule (first (first rule))
              (rules->cfe (make-pda-action (second (first rule)) (third (first rule)) (second (second rule))))
              (first (second rule))))
  
  ;; pda --> cfe-template
  ;; Purpose: Recursively rips nodes out from the given M and converts the ripped nodes to cfe-templates
  ;; Assume: The transition diagram of the given machine is a connected directed graph
  (define (pda2temp P)
    ;; dgraph --> dgraph
    ;; Purpose: Collapse multiple edges between nodes
    (define (remove-multiple-edges g)
      ;; (listof pda-rule) -> cfe-template
      ;; Purpose: Collapse the given pda-rule into a cfe-template
      (define (collapse-edges loe)
        ;; (listof pda-rule) (listof pda-action) -> cfe-template
        ;; Purpose: Collapse the given pda-rule into a cfe-template
        (define (collapse-edges-helper loe acc)
          (cond [(null? loe) (error "erm 2")]
                [(null? (rest loe)) (union (cons (pda-rule-action (first loe)) acc))]
                [else (collapse-edges-helper (rest loe) (cons (pda-rule-action (first loe)) acc))]))
        (cond [(null? loe) (error "erm")]
              [(null? (rest loe)) (pda-rule-action (first loe))]
              [else (collapse-edges-helper (rest loe) (cons (pda-rule-action (first loe)) (list)))]))
      (if (null? g)
          '()
          (let* [(curr-edge (first g))
                 (from-state (pda-rule-source curr-edge))
                 (to-state (pda-rule-destin curr-edge))
                 (to-collapse (filter (λ (e) (and (eq? (pda-rule-source e) from-state)
                                                  (eq? (pda-rule-destin e) to-state)))
                                      g))
                 (remaining-g (filter (λ (e) (not (member e to-collapse))) g))]
            (cons (pda-rule from-state (collapse-edges to-collapse) to-state)
                  (remove-multiple-edges remaining-g)))))

    ;; (listof node) dgraph --> dgraph
    ;; Purpose: Rip out the given nodes from the given graph
    (define (rip-out-nodes lon g)
      ;; node dgraph --> dgraph
      ;; Purpose: Rip out given state from given graph
      (define (rip-out-node n g)
        ;;rule -> (listof rules)
        ;;Purpose: Extracts the rules that make the concat if possible
        (define (extract-concat rule)
          (if (concat? rule)
              (concat-rules rule)
              (list rule)))
        (let* [(non (filter (λ (r) (and (not (eq? (pda-rule-destin r) n))
                                        (not (eq? (pda-rule-source r) n))))
                            g))
               (into-n (filter (λ (r) (and (eq? (pda-rule-destin r) n)
                                           (not (eq? (pda-rule-source r) n))))
                               g))
               (outof-n (filter (λ (r) (and (eq? (pda-rule-source r) n)
                                            (not (eq? (pda-rule-destin r) n))))
                                g))
               (self-edges (filter (λ (r) (and (eq? (pda-rule-source r) n)
                                               (eq? (pda-rule-destin r) n)))
                                   g))]
          (remove-multiple-edges
           (append
            non
            (if (not (null? self-edges))
                (let [(self-edge (first self-edges))]
                  (append-map (λ (into-edge)
                                (map (λ (outof-edge) (pda-rule (pda-rule-source into-edge)
                                                               (concat (append (extract-concat (pda-rule-action into-edge))
                                                                               (list (kleene (pda-rule-action self-edge)))
                                                                               (extract-concat (pda-rule-action outof-edge))))
                                                               (pda-rule-destin outof-edge)))
                                     outof-n))
                              into-n))
                (append-map (λ (into-edge)
                              (map (λ (outof-edge) (pda-rule (pda-rule-source into-edge)
                                                             (concat (append (extract-concat (pda-rule-action into-edge))
                                                                             (extract-concat (pda-rule-action outof-edge))))
                                                             (pda-rule-destin outof-edge)))
                                   outof-n))
                            into-n))))))
      (foldr (λ (s g) (rip-out-node s g)) g lon))
  
    ;;cfe-template -> cfe-template
    ;;Purpose: Simplifies the given cfe-template
    (define (simplify-templates temp)
      ;;union-cfe-template -> cfe-template
      ;;Purpose: Simplifies the given union-cfe-template
      (define (simplify-empty temp)
        (if (andmap empty? (union-rules temp))
            (empty (first (union-rules temp)))
            (union (map simplify-templates (union-rules temp)))))
      (cond [(concat? temp) (let ([res (filter-not empty? (map simplify-templates (concat-rules temp)))])
                              (if (is-length-one? res)
                                  (first res)
                                  (concat res)))]
            [(union? temp) (simplify-empty temp)]
            [(kleene? temp) (kleene (simplify-templates (kleene-rule temp)))]
            [else temp]))
  
    (let* ([new-states (for/fold ([st (pda-getstates P)])
                                 ([x (in-range 2)])
                         (cons (gen-state st) st))]
           [new-start (first new-states)]
           [new-final (second new-states)]
           [new-rules-to-final (for/fold ([acc '()])
                                         ([final (pda-getfinals P)])
                                 (cons (list (list final EMP EMP) (list new-final EMP)) acc))]
           [new-rules-to-start (list (list new-start EMP EMP) (list (pda-getstart P) EMP))]
           [init-dgraph (map rule->struct (append (cons new-rules-to-start new-rules-to-final) (pda-getrules P)))]
           [collapsed-dgraph (rip-out-nodes (pda-getstates P)
                                            (remove-multiple-edges init-dgraph))])
    
      (pda (take new-states 2)
           (pda-getalphabet P)
           (pda-getgamma P)
           new-start
           new-final
           (if (null? collapsed-dgraph)
               '()
               (struct-copy pda-rule (first collapsed-dgraph)
                            [action (simplify-templates (pda-rule-action (first collapsed-dgraph)))])))))


  ;;cfe-template -> (listof cfe-template)
  ;;Purpose: Simplifies the main cfe-template by splitting it into sub cfe-template 
  (define (make-sub-languages cfe-template)
    ;;(listof cfe-template) (listof cfe-template) -> (listof cfe-template)
    ;;Purpose: Constructs sublanguages from the given concat cfe-template
    (define (make-concat-sublanguage rules acc)
      ;;(listof cfe-template) (listof cfe-template) -> (listof cfe-template)
      ;;Purpose: Constructs sublanguages from the given concat cfe-template
      (define (process-concat-temp cfe-temp acc)
        (cond [(kleene? cfe-temp) (map (λ (cfe-acc)
                                         (struct-copy concat cfe-acc
                                                      [rules (append (concat-rules cfe-acc) (list cfe-temp))]))
                                       acc)]
              [(union? cfe-temp) (append-map (λ (cfe-acc)
                                               (map (λ (temp)
                                                      (struct-copy concat cfe-acc
                                                                   [rules (append (concat-rules cfe-acc) (list temp))]))                                            
                                                    (union-rules cfe-temp)))
                                             acc)]
              [(concat? cfe-temp) (append-map (λ (cfe-acc)
                                                (map (λ (temp)
                                                       (struct-copy concat cfe-acc
                                                                    [rules (append (concat-rules cfe-acc) (list temp))]))                                            
                                                     (concat-rules cfe-temp)))
                                              acc)]
              [else (map (λ (cfe-acc)
                           (struct-copy concat cfe-acc
                                        [rules (append (concat-rules cfe-acc) (list cfe-temp))]))
                         acc)]))
      (if (null? rules)
          acc
          (make-concat-sublanguage (rest rules)
                                   (process-concat-temp (first rules) acc))))    
    (cond [(kleene? cfe-template) (let ([res (make-sub-languages (kleene-rule cfe-template))])
                                    (list (kleene (if (is-length-one? res) (first res) (union res)))))]
          [(union? cfe-template) (union-rules cfe-template)]
          [(concat? cfe-template) (make-concat-sublanguage (concat-rules cfe-template) (list (concat (list))))]
          [else (list cfe-template)]))

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule only reads an letter in sigma
  (define (read-only? action)
    (and (not (eq? EMP (pda-action-read action)))
         (eq? EMP (pda-action-pop action))
         (eq? EMP (pda-action-push action))))
  

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule ONLY pushes to the stack
  (define (push-only? action)
    (and (eq? EMP (pda-action-read action))
         (eq? EMP (pda-action-pop action))
         (list? (pda-action-push action))))

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule ONLY pushes to the stack
  (define (push? action)
    (list? (pda-action-push action)))
  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule ONLY pops off the stack
  (define (pop? action)
    (list? (pda-action-pop action)))

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule ONLY pops off the stack
  (define (pop-only? action)
    (and (eq? EMP (pda-action-read action))
         (list? (pda-action-pop action))
         (eq? EMP (pda-action-push action))))

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule reads AND pops off the stack
  (define (read-and-pop? action)
    (and (not (eq? EMP (pda-action-read action)))
         (list? (pda-action-pop action))
         (eq? EMP (pda-action-push action))))

  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-action reads AND pushes to the stack
  (define (read-and-push? action)
    (and (not (eq? EMP (pda-action-read action)))
         (eq? EMP (pda-action-pop action))
         (list? (pda-action-push action))))

  (define (contruct-cfe loCT)
    (define (contruct-cfe-helper cfe-template)

      (define (complement-push-only? push-rule other)
        other)

      (define (complement-read-and-pop? push-rule other)
        other)
      
      (define (process-singleton pda-rule remaining-rules)

        (define (find-complement-pop push-rule rules)
          (cond [(null? rules) (error "no complement")]
                [(complement-push-only? push-rule (first rules)) (list push-rule (first rules))]
                [else (find-complement-pop push-rule (rest rules))]))

        (define (find-complement-read-and-pop push-rule rules)
          (cond [(null? rules) (error "no complement")]
                [(complement-read-and-pop? push-rule (first rules)) (list push-rule (first rules))]
                [else (find-complement-read-and-pop push-rule (rest rules))]))
              

        
        (cond [(read-only? pda-rule) (mk-singleton-cfexp (pda-action-read pda-rule))]
              [(push-only? pda-rule) (find-complement-pop pda-rule remaining-rules)]
              [(read-and-push? pda-rule) (find-complement-read-and-pop pda-rule remaining-rules)]
              [(error (format "wtf is this: ~a" pda-rule))]))
      
      (define (make-concat-cfe concat-cfe-templates)
        (if (null? concat-cfe-templates)
            '()
            (let ([first-temp (first concat-cfe-templates)])
              (cond [(kleene? first-temp) (mk-kleene-cfexp (contruct-cfe-helper (kleene-rule first-temp)))]
                    [(union? first-temp) (mk-union-cfexp (map contruct-cfe-helper (union-rules first-temp)))]
                    [(concat? first-temp) (error "wuh oh") #;(mk-concat-cfexp (make-concat-cfe (concat-rules cfe-template)))]
                    [(singleton? first-temp) (process-singleton (singleton-rule first-temp) (rest concat-cfe-templates))]
                    [else (make-concat-cfe (rest concat-cfe-templates))]))))
      
      (cond [(kleene? cfe-template) (mk-kleene-cfexp (contruct-cfe-helper (kleene-rule cfe-template)))]
            [(union? cfe-template) (mk-union-cfexp (map contruct-cfe-helper (union-rules cfe-template)))]
            [(concat? cfe-template) (mk-concat-cfexp (map contruct-cfe-helper (concat-rules cfe-template)) #;(list (make-concat-cfe (concat-rules cfe-template))))]
            [(singleton? cfe-template) (mk-singleton-cfexp (singleton-rule cfe-template))]
            [else cfe-template]))
    (let ([res (map contruct-cfe-helper loCT)])
      (if (is-length-one? res)
          (first res)
          (mk-union-cfexp res))))

  (struct inverse-pair (push pop stack homogenous?) #:transparent)
  
  (define (rule-extractor cfe-template)
    (cond [(kleene? cfe-template) (rule-extractor (kleene-rule cfe-template))]
          [(union? cfe-template) (map rule-extractor (union-rules cfe-template))]
          [(concat? cfe-template) (map rule-extractor (concat-rules cfe-template))]
          [(singleton? cfe-template) (singleton-rule cfe-template)]
          [(empty? cfe-template) (empty-rule cfe-template)]
          [else cfe-template]))


  (define (find-reachables rule-structs)

    (define (find-reachables-helper state-to-search states visited-states acc)
      (cond [(and (null? states) (set-member? visited-states state-to-search)) acc]
            [(null? states)
             (let ([next-states (filter-map (λ (rule)
                                              (and (eq? (pda-rule-source rule) state-to-search)
                                                   (pda-rule-destin rule)))
                                            rule-structs)])
               (find-reachables-helper (if (null? next-states) state-to-search (first next-states))
                                       (if (null? next-states) next-states (rest next-states))
                                       (set-add visited-states state-to-search)
                                       (append acc (filter (λ (rule)
                                                             (eq? (pda-rule-source rule) state-to-search))
                                                           rule-structs))))]
            [(set-member? visited-states state-to-search)
             (find-reachables-helper (first states) (rest states) visited-states acc)]
            [else (find-reachables-helper (first states)
                                          (append (rest states)
                                                  (filter-map (λ (rule)
                                                                (and (eq? (pda-rule-source rule) state-to-search)
                                                                     (pda-rule-destin rule)))
                                                              rule-structs))
                                          (set-add visited-states state-to-search)
                                          (append acc (filter (λ (rule)
                                                                (eq? (pda-rule-source rule) state-to-search))
                                                              rule-structs)))]))
      
    
    (let ([states (pda-getstates P)])
      (foldl (λ (state acc)
               (hash-set acc state (find-reachables-helper state
                                                           (filter-map (λ (rule)
                                                                         (and (eq? (pda-rule-source rule) state)
                                                                              (pda-rule-destin rule)))
                                                                       rule-structs)
                                                           (set)
                                                           (list))))
             (hash)
             states)))

  (define (pair-stack-operations reachables-ht rules)

    (define (pair-operations-helper push-pair pop-rules acc)
      ;;stack (listof symbol) -> Boolean
      ;;Purpose: Determines if the give (listof symbol) and pop the elements off the stack
      (define (same-elements? stack pop)
        (and (= (set-count (list->set pop)) 1)
             (andmap (λ (x) (member x stack)) pop)))

      (define (match-operations push-pair stack pop-rules)
        (define (can-pop? stack pop)
          (let ([pop-amount (length pop)]
                [stack-length (length stack)])
            (and (>= stack-length pop-amount)
                 (equal? (take stack pop-amount) pop))))
        (cond [(null? stack) push-pair]
              [(null? pop-rules) push-pair]
              [(equal? stack (pda-action-pop (pda-rule-action (first pop-rules))))
               (match-operations (struct-copy inverse-pair push-pair
                                              [pop (append (inverse-pair-pop push-pair) (list (first pop-rules)))])
                                 stack
                                 (rest pop-rules))]
              [(can-pop? stack (pda-action-pop (pda-rule-action (first pop-rules))))
               (match-operations (struct-copy inverse-pair push-pair
                                              [pop (append (inverse-pair-pop push-pair) (list (first pop-rules)))])
                                 (drop stack (length (pda-action-pop (pda-rule-action (first pop-rules)))))
                                 (rest pop-rules))]
              [else (match-operations push-pair stack (rest pop-rules))]))

      (define (balance-stack push-pair pop-rule)
        (define (balance-pop stack-length acc)
          (if (= 0 stack-length)
              (struct-copy inverse-pair push-pair
                           [pop (append (inverse-pair-pop push-pair) acc)])
              (balance-pop (sub1 stack-length) (cons pop-rule acc))))

        (define (balance-push pop-amount acc)
          push-pair
          #;(if (= 0 stack-length)
                (struct-copy inverse-pair push-pair
                             [pop (append (inverse-pair-pop push-pair) acc)])
                (balance-pop (sub1 stack-length) (cons pop-rule acc))))
        
        (let ([stack-length (length (inverse-pair-stack push-pair))]
              [pop-amount (length (pda-action-pop (pda-rule-action pop-rule)))])
          (if (> stack-length pop-amount)
              (balance-pop stack-length '())
              (balance-push pop-amount '()))))

      (define (same-rule? r1 r2)
        (and (eq? (pda-rule-source r1) (pda-rule-source r2))
             (equal? (pda-rule-action r1) (pda-rule-action r2))
             (eq? (pda-rule-destin r1) (pda-rule-destin r2))))

      (define (stack-wall? pop-rule)
        (and (not (equal? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action pop-rule))))
             (push? (pda-rule-action pop-rule))
             (equal? (pda-action-push (pda-rule-action pop-rule)) (pda-action-pop (pda-rule-action pop-rule)))))
      
      (cond [(not (inverse-pair-homogenous? push-pair)) (match-operations push-pair (inverse-pair-stack push-pair) pop-rules)]
            [(or (null? pop-rules)
                 (stack-wall? (first pop-rules)))
             
             (reverse acc)]
            [(and (not (equal? (inverse-pair-push push-pair) (first pop-rules)))
                  (push? (pda-rule-action (first pop-rules)))
                  (equal? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules)))))
             (cons (struct-copy inverse-pair push-pair
                          [pop (first pop-rules)]) acc)]
            [(and (not (same-rule? (inverse-pair-push push-pair) (first pop-rules)))
                  (equal? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules)))))
                   (pair-operations-helper push-pair
                                     (rest pop-rules)
                                     (cons (struct-copy inverse-pair push-pair
                                                  [pop (first pop-rules)]) acc))]
            [(and (not (same-rule? (inverse-pair-push push-pair) (first pop-rules)))
                  (same-elements? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules)))))
             (cons (balance-stack push-pair (first pop-rules)) acc)
             #;(struct-copy inverse-pair push-pair
                            [pop (cons (first pop-rules) (inverse-pair-pop push-pair))])]
            [else (pair-operations-helper push-pair (rest pop-rules) acc)]))
    (let* ([push-rules (remove-duplicates (filter (λ (rule) (push? (pda-rule-action rule))) rules))]
           [push-pairs (map (λ (x) (inverse-pair x '() (pda-action-push (pda-rule-action x)) (= (set-count (list->set (pda-action-push (pda-rule-action x)))) 1))) push-rules)]
           [pop-rules (map (λ (pu-rule) (filter (λ (rule) (pop? (pda-rule-action rule))) (hash-ref reachables-ht (pda-rule-source pu-rule)))) push-rules)]
           [pair-operations (map (λ (x y) (pair-operations-helper x y '())) push-pairs pop-rules)])
      pair-operations))

  
  (let* ([new-P (pda2temp P)]
         [sub-langs (make-sub-languages (pda-rule-action (pda-rules new-P)))]
         [rule-structs (map (λ (rule)
                              (pda-rule (first (first rule))
                                        (pda-action (second (first rule)) (third (first rule)) (second (second rule)))
                                        (first (second rule))))
                            (pda-getrules P))]
         [reachable-rules (find-reachables rule-structs)]
         [stack-operations (pair-stack-operations reachable-rules rule-structs)])
    
    
    (list (sm-graph P)
          ;sub-langs
          ;(map (compose1 flatten rule-extractor) sub-langs)
          ;reachable-rules
          stack-operations
          #;(contruct-cfe sub-langs))))


;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define (cfe->pda cfe)
  (cfg->pda (cfe->cfg cfe)))