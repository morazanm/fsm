#lang racket/base

(require "../../../sm-graph.rkt"
         "../constants.rkt"
         "../cfg-struct.rkt"
         (except-in "../pda.rkt" pda->spda)
         "../misc.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
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
  ;;vars    | the accumulated variables found from traversing the given cfe  | (listof union-cfexp)
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
    ;;cfe -> (listof cfe)
    ;;Purpose: Extracts the sub-expressions from the given cfe
    (define (extract-cfe-data cfe)
      (cond [(mk-concat-cfexp? cfe) (vector->treelist (mk-concat-cfexp-locfe cfe))]
            [(mk-union-cfexp? cfe) (vector->treelist (mk-union-cfexp-locfe cfe))]
            [(mk-kleene-cfexp? cfe) (treelist (mk-kleene-cfexp-cfe cfe))]
            [(box? cfe) (treelist (unbox cfe))]
            [else empty-treelist]))
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
    (let ([init-queue (tl-foldl (λ (env acc)
                                  (enqueue acc (treelist env)))
                                E-QUEUE
                                (extract-cfe-data cfe))])
      (extract-var-and-singles init-queue
                               (update-extraction-results cfe (extraction-results '() '()))
                               (set cfe))))

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
  (struct singleton (rule source destin) #:transparent)

  ;;(list (list state symbol los) (list state los)) -> pda-rule
  ;;Purpose: Converts the given pda rule into a pda-rule struct
  (define (rule->struct rule)
    ;;symbol los los -> pda-action
    ;;Purpose: Creates a pda action from the given input
    (define (make-pda-action read pop push)
      (pda-action read pop push))
    ;;pda-rule -> cfe-template
    ;;Purpose: Converts the given pda-rule to a cfe-template
    (define (rules->cfe pda-action source destin)
      ;;pda-rule -> Boolean
      ;;Purpose: Determines if the given pda-rule is an empty transition
      (define (e-transition? action)
        (and (eq? EMP (pda-action-read action))
             (eq? EMP (pda-action-pop action))
             (eq? EMP (pda-action-push action))))
      (if (e-transition? pda-action)
          (empty pda-action) 
          (singleton pda-action source destin)))
    (pda-rule (first (first rule))
              (rules->cfe (make-pda-action (second (first rule))
                                           (third (first rule))
                                           (second (second rule)))
                          (first (first rule))
                          (first (second rule)))
              (first (second rule))))
  
  ;; pda --> cfe-template
  ;; Purpose: Recursively rips nodes out from the given M and converts the ripped nodes to cfe-templates
  ;; Assume: The transition diagram of the given machine is a connected directed graph
  (define (pda2temp P)
    ;; dgraph --> dgraph
    ;; Purpose: Collapse multiple edges between nodes
    (define (remove-multiple-edges graph)
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
      (if (null? graph)
          '()
          (let* [(curr-edge (first graph))
                 (from-state (pda-rule-source curr-edge))
                 (to-state (pda-rule-destin curr-edge))
                 (to-collapse (filter (λ (edge) (and (eq? (pda-rule-source edge) from-state)
                                                     (eq? (pda-rule-destin edge) to-state)))
                                      graph))
                 (remaining-g (filter (λ (e) (not (member e to-collapse))) graph))]
            (cons (pda-rule from-state (collapse-edges to-collapse) to-state)
                  (remove-multiple-edges remaining-g)))))

    ;; (listof node) dgraph --> dgraph
    ;; Purpose: Rip out the given nodes from the given graph
    (define (rip-out-nodes lon g)
      ;; node dgraph --> dgraph
      ;; Purpose: Rip out given state from given graph
      (define (rip-out-node node graph)
        ;;rule -> (listof rules)
        ;;Purpose: Extracts the rules that make the concat if possible
        (define (extract-concat rule)
          (if (concat? rule)
              (concat-rules rule)
              (list rule)))
        (let* [(non (filter (λ (rule) (and (not (eq? (pda-rule-destin rule) node))
                                           (not (eq? (pda-rule-source rule) node))))
                            graph))
               (into-n (filter (λ (rule) (and (eq? (pda-rule-destin rule) node)
                                              (not (eq? (pda-rule-source rule) node))))
                               graph))
               (outof-n (filter (λ (rule) (and (eq? (pda-rule-source rule) node)
                                               (not (eq? (pda-rule-destin rule) node))))
                                graph))
               (self-edges (filter (λ (rule) (and (eq? (pda-rule-source rule) node)
                                                  (eq? (pda-rule-destin rule) node)))
                                   graph))]
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
  ;;Purpose: Determines if the given pda-rule ONLY pushes to the stack
  (define (push? action)
    (list? (pda-action-push action)))
  
  ;;pda-action -> Boolean
  ;;Purpose: Determines if the given pda-rule ONLY pops off the stack
  (define (pop? action)
    (list? (pda-action-pop action)))

  #|
    inverse-pair is struct containing a pda rules that keep the stack empty
    push        | pda-rule that pushes to the stack => pda-rule
    pop         | pda-rule that pops off the stack => pda-rule/(listof pda-rule)
    stack       | the stack when push rule is applied => (listof symbol)
    homogenous? | is the stack is homogenous => Boolean
  |#
  (struct inverse-pair (push pop stack homogenous?) #:transparent)

  ;;cfe-template ->  cfe-template / (listof cfe-template)
  ;;Purpose: Extractors the base case from the given cfe-template 
  (define (extractor-prims cfe-template)
    (cond [(kleene? cfe-template) (extractor-prims (kleene-rule cfe-template))]
          [(union? cfe-template) (map extractor-prims (union-rules cfe-template))]
          [(concat? cfe-template) (map extractor-prims (concat-rules cfe-template))]
          [else cfe-template]))

  ;;pda-rule -> (hash state . (listof pda-rule))
  ;;Purpose: Finds the rules reachable from each state of the given pda
  (define (find-reachables rule-structs)
    ;;state (listof state) (setof state) (listof pda-rule) -> (listof pda-rule)
    ;;Purpose: Finds the rules reachable from the given state
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

  ;;(hash state . (listof pda-rule)) (listof pda-rule) -> (listof inverse-pair)
  ;;Purpose: Pairs all push operations with pop operations
  (define (pair-stack-operations reachables-ht rules)
    ;; inverse-pair (listof pda-rule) (listof inverse-pair) -> (listof inverse-pair)
    ;;Purpose: Creates completed inverse-pairs where every push-rule is matched with it's corresponding pop-rule
    (define (pair-operations-helper push-pair pop-rules acc)
      ;;stack (listof symbol) -> Boolean
      ;;Purpose: Determines if the give (listof symbol) and pop the elements off the stack
      (define (same-elements? stack pop)
        (and (= (set-count (list->set pop)) 1)
             (andmap (λ (x) (member x stack)) pop)))
      ;;inverse-pair stack (listof pda-rule) -> inverse-pair
      ;;Purpose: Matches the pop operations to the given push rule 
      (define (match-operations push-pair stack pop-rules all-pop-rules)
        ;;stack (listof symbol) -> Boolean
        ;;Purpose: Determines if the given stack can have the given elements popped off
        (define (can-pop? stack pop)
          (let ([pop-amount (length pop)]
                [stack-length (length stack)])
            (and (>= stack-length pop-amount)
                 (equal? (take stack pop-amount) pop))))
        (cond [(null? stack) (list push-pair)]
              [(null? pop-rules) (match-operations push-pair stack all-pop-rules all-pop-rules)]
              [(equal? stack (pda-action-pop (pda-rule-action (first pop-rules))))
               (match-operations (struct-copy inverse-pair push-pair
                                              [pop (append (inverse-pair-pop push-pair) (list (first pop-rules)))])
                                 (list)
                                 (rest pop-rules)
                                 all-pop-rules)]
              [(can-pop? stack (pda-action-pop (pda-rule-action (first pop-rules))))
               (match-operations (struct-copy inverse-pair push-pair
                                              [pop (append (inverse-pair-pop push-pair) (list (first pop-rules)))])
                                 (drop stack (length (pda-action-pop (pda-rule-action (first pop-rules)))))
                                 (rest pop-rules)
                                 all-pop-rules)]
              [else (match-operations push-pair stack (rest pop-rules) all-pop-rules)]))

      ;;inverse-pair pda-rule -> inverse pair
      ;;Purpose: Balances the stack of the given inverse pair
      (define (balance-stack push-pair pop-rule)
        ;;natnum (listof pda-rule) -> inverse-pair
        ;;Purpose: Balances the pop of the inverse pair to match the amount pushed
        (define (balance-pop stack-length acc)
          (if (= 0 stack-length)
              (struct-copy inverse-pair push-pair
                           [pop (append (inverse-pair-pop push-pair) acc)])
              (balance-pop (sub1 stack-length) (cons pop-rule acc))))

        ;;natnum (listof pda-rule) -> inverse-pair
        ;;Purpose: Balances the push of the inverse pair to match the amount popped
        (define (balance-push pop-amount acc)
          (error (format "ermmm wuh oh!\npush pair:~a\npop-rule:~a" push-pair pop-rule))
          #;(if (= 0 stack-length)
                (struct-copy inverse-pair push-pair
                             [pop (append (inverse-pair-pop push-pair) acc)])
                (balance-pop (sub1 stack-length) (cons pop-rule acc))))
        
        (let ([stack-length (length (inverse-pair-stack push-pair))]
              [pop-amount (length (pda-action-pop (pda-rule-action pop-rule)))])
          (if (> stack-length pop-amount)
              (balance-pop stack-length '())
              (balance-push pop-amount '()))))

      ;;inverse-pair (listof pda-rule) -> inverse pair
      ;;Purpose: Updates the pop rules for the given inverse pair by searching for rules that can empty the stack
      (define (update-stack push-pair pop-rules)
        (cond [(or (null? pop-rules)
                   (null? (inverse-pair-stack push-pair)))
               (struct-copy inverse-pair push-pair
                            [pop (reverse (inverse-pair-pop push-pair))])]
              [(and (not (same-rule? (inverse-pair-push push-pair) (first pop-rules)))
                    (equal? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules)))))
               (update-stack (struct-copy inverse-pair push-pair
                                          [pop (cons (first pop-rules) (inverse-pair-pop push-pair))]
                                          [stack (drop (inverse-pair-stack push-pair)
                                                       (length (pda-action-pop (pda-rule-action (first pop-rules)))))])
                             (rest pop-rules))]
              [else (update-stack push-pair (rest pop-rules))]))
              
      ;;pda-rule pda-rule -> Boolean
      ;;Purpose: Determines if the given pda-rules are the same
      (define (same-rule? r1 r2)
        (and (eq? (pda-rule-source r1) (pda-rule-source r2))
             (equal? (pda-rule-action r1) (pda-rule-action r2))
             (eq? (pda-rule-destin r1) (pda-rule-destin r2))))

      ;;pda-rule -> Boolean
      ;;Purpose: Determines if the given pda-rule forms a wall using the stack
      (define (stack-wall? pop-rule)
        (and (not (equal? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action pop-rule))))
             (push? (pda-rule-action pop-rule))
             (equal? (pda-action-push (pda-rule-action pop-rule)) (pda-action-pop (pda-rule-action pop-rule)))))
      
      (cond [(not (inverse-pair-homogenous? push-pair)) (match-operations push-pair (inverse-pair-stack push-pair) pop-rules pop-rules)]
            [(or (null? pop-rules) (stack-wall? (first pop-rules))) (reverse acc)]
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
                  (same-elements? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules))))
                  (not (eq? (pda-rule-source (first pop-rules)) (pda-rule-destin (first pop-rules)))))
             (pair-operations-helper push-pair
                                     (rest pop-rules)
                                     (cons (update-stack (struct-copy inverse-pair push-pair
                                                                      [pop (list (first pop-rules))]
                                                                      [stack (drop (inverse-pair-stack push-pair)
                                                                                   (length (pda-action-pop (pda-rule-action
                                                                                                            (first pop-rules)))))])
                                                         (rest pop-rules))
                                           acc))]
            [(and (not (same-rule? (inverse-pair-push push-pair) (first pop-rules)))
                  (same-elements? (inverse-pair-stack push-pair) (pda-action-pop (pda-rule-action (first pop-rules))))
                  (eq? (pda-rule-source (first pop-rules)) (pda-rule-destin (first pop-rules))))
             (pair-operations-helper push-pair
                                     (rest pop-rules)
                                     (cons (balance-stack push-pair (first pop-rules)) acc))]
            [else (pair-operations-helper push-pair (rest pop-rules) acc)]))

    ;;pda-rule -> inverse-pair
    ;;Purpose: Makes an inverse-pair from the corresponding push-rule
    (define (make-inverse-pair push-rule)
      (inverse-pair push-rule
                    '()
                    (pda-action-push (pda-rule-action push-rule))
                    (= (set-count (list->set (pda-action-push (pda-rule-action push-rule)))) 1)))
    (let* ([push-rules (remove-duplicates (filter (λ (rule) (push? (pda-rule-action rule))) rules))]
           [push-pairs (map (λ (push-rule)
                              (make-inverse-pair push-rule))
                            push-rules)]
           [pop-rules (map (λ (pu-rule)
                             (filter (λ (rule)
                                       (pop? (pda-rule-action rule)))
                                     (hash-ref reachables-ht (pda-rule-source pu-rule))))
                           push-rules)]
           [pair-operations (append-map (λ (x y) (pair-operations-helper x y '())) push-pairs pop-rules)])
      (if (list? pair-operations)
          pair-operations
          (list pair-operations))))

  ;;(X -> Y) inverse-pair pda-rule -> Boolean
  ;;Purpose: Determines if the given inverse-pair and pda-rule are the same
  (define (same-rule? accessor oper rule)
    (and (not (empty? rule))
         (let ([oper (if (and (list? (accessor oper))
                              (inverse-pair-homogenous? oper))
                         (first (accessor oper))
                         (accessor oper))])
           (and (equal? (pda-rule-action oper) (singleton-rule rule))
                (eq? (pda-rule-source oper) (singleton-source rule))
                (eq? (pda-rule-destin oper) (singleton-destin rule))))))

  ;;(X -> Y) inverse-pair pda-rule -> Boolean
  ;;Purpose: Determines if the given inverse-pair and pda-rule are the same
  (define (same-rules? accessor oper rules)
    (ormap (λ (rule)
             (same-rule? accessor oper rule))
           rules))

  ;;(listof cfe-template) -> Boolean
  ;;Purpose: Determines if the given list of cfe-template uses the stack
  (define (uses-stack? sub-lang)
    ;;cfe-template -> pda-rule / (listof pda-rule)
    ;;Purpose: Extracts the pda-rule from the given cfe-template
    (define (rule-extractor cfe-template)
      (cond [(kleene? cfe-template) (rule-extractor (kleene-rule cfe-template))]
            [(union? cfe-template) (map rule-extractor (union-rules cfe-template))]
            [(concat? cfe-template) (map rule-extractor (concat-rules cfe-template))]
            [(singleton? cfe-template) (singleton-rule cfe-template)]
            [(empty? cfe-template) (empty-rule cfe-template)]
            [else cfe-template]))
    (ormap (λ (lang)
             (or (push? lang)
                 (pop? lang)))
           (flatten (rule-extractor sub-lang))))
  
  ;;(listof cfe-template) (listof inverse-pair) -> (listof inverse-pair)
  ;;Purpose: Finds the inverse pairs applicable to given (listof cfe-template)
  (define (find-applicable-opers sub-lang stack-operations)
    ;;inverse-pair (setof pda-rule) -> Boolean
    ;;Purpose: Determines if the given inverse-pair's pop rules are the same as the given pop-rules
    (define (same-pop-rules? oper rules)
      (map (λ (pop-rule)
             (let ([oper (singleton (pda-rule-action pop-rule) (pda-rule-source pop-rule) (pda-rule-destin pop-rule))])
               (set-member? rules oper)))
           (inverse-pair-pop oper)))
    (let* ([sublang-rules (filter singleton? (flatten (extractor-prims sub-lang)))]
           [push-rules (filter (compose1 push? singleton-rule) sublang-rules)]
           [pop-rules (filter (compose1 pop? singleton-rule) sublang-rules)])
      (remove-duplicates (for*/list ([oper (in-list stack-operations)]
                                     [push-rule (in-list push-rules)]
                                     [pop-rule (in-list pop-rules)]
                                     #:when (and (same-rule? inverse-pair-push oper push-rule)
                                                 (if (list? (inverse-pair-pop oper))
                                                     (same-pop-rules? oper (list->set pop-rules))
                                                     (same-rule? inverse-pair-pop oper pop-rule))))
                           oper))))

  ;;(listof (listof cfe-template)) (listof (listof inverse-pair)) -> cfe
  ;;Purpose: Creates a cfe from the given (listof (listof cfe-template))
  (define (build-cfe sub-langs stack-operations)
    ;;(listof cfe-template) (listof inverse-pair) -> cfe
    ;;Purpose: Creates a cfe from the given (listof cfe-template)
    (define (build-cfe-helper sub-lang stack-oper)

      ;;cfe-template -> cfe
      ;;Purpose: Creates a cfe from the given cfe-template
      (define (build-cfe lang)
        (cond [(singleton? lang) (mk-singleton-cfexp (symbol->string (pda-action-read (singleton-rule lang))))]
              [(kleene? lang) (mk-kleene-cfexp (build-cfe (kleene-rule lang)))]
              [(union? lang) (mk-union-cfexp (list->vector (map build-cfe (union-rules lang))))]
              [(empty? lang) (mk-empty-cfexp)]))

      ;;cfe-template (listof pda-rule) -> Boolean
      ;;Purpose: Determines if the given the cfe-template is a member of the given (listof pda-rule)
      (define (member-of? lang stack-opers)
        (let ([rule (extractor-prims lang)])
          (ormap (λ (oper)
                   (and (not (empty? rule))
                        (equal? (pda-rule-action oper) (singleton-rule rule))
                        (eq? (pda-rule-source oper) (singleton-source rule))
                        (eq? (pda-rule-destin oper) (singleton-destin rule))))
                 stack-opers)))

      ;;cfe-template (listof pda-rule) -> Boolean
      ;;Purpose: Determines if the given the cfe-template is a member of the given (listof pda-rule)
      ;;Note: For union cfe-templates
      (define (members-of? lang stack-opers)
        (let ([rule (flatten (extractor-prims lang))])
          (ormap (λ (rule) (member-of? rule stack-opers))
                 rule)))

      ;;(X -> Y) cfe-template (listof inverse-pair) -> (listof inverse-pair)
      ;;Purpose: Returns the inverse pairs applicable to given cfe-template
      (define (get-stack-oper accessor lang stack-opers)
        (let ([rule (extractor-prims lang)])
          (filter (λ (oper)
                    (same-rule? accessor oper rule))
                  stack-opers)))

      ;;(X -> Y) cfe-template (listof inverse-pair) -> (listof inverse-pair)
      ;;Purpose: Returns the inverse pairs applicable to given cfe-template
      ;;Note: For union cfe-templates
      (define (get-stack-opers accessor lang stack-opers)
        (let ([rule (flatten (extractor-prims lang))])
          (flatten (map (λ (rule) (get-stack-oper accessor rule stack-opers))
                        rule))))

      ;;inverse-pair -> Boolean
      ;;Purpose: Determines if the push rule and pop rule are self-loops
      (define (recursive? pair)
        (and (eq? (pda-rule-source (inverse-pair-push pair))  (pda-rule-destin (inverse-pair-push pair)))
             (cond [(and (list? (inverse-pair-pop pair))
                         (inverse-pair-homogenous? pair))
                    (eq? (pda-rule-source (first (inverse-pair-pop pair)))  (pda-rule-destin (first (inverse-pair-pop pair))))]
                   [(list? (inverse-pair-pop pair))
                    (andmap (λ (pop-rule)
                              (eq? (pda-rule-source pop-rule) (pda-rule-destin pop-rule)))
                            (inverse-pair-pop pair))]
                 (eq? (pda-rule-source (inverse-pair-pop pair)) (pda-rule-destin (inverse-pair-pop pair))))))
      ;;(X -> Y) inverse-pair -> cfe
      ;;Purpose: Converts the given inverse-pair into a cfe
      (define (rule->cfe accessor pair)
        (if (pda-rule? (accessor pair))
            (let ([sym (pda-action-read (pda-rule-action (accessor pair)))])
              (if (eq? EMP sym)
                  (mk-empty-cfexp)
                  (mk-singleton-cfexp (symbol->string sym))))
            (mk-concat-cfexp (list->vector (map (λ (rule)
                                                  (mk-singleton-cfexp (symbol->string (pda-action-read (pda-rule-action rule)))))
                                                (accessor pair))))))
      
      ;;inverse-pair (listof cfe-template) -> cfe
      ;;Purpose: Converts the given inverse pair into a cfe
      (define (inverse-pair->cfe inverse-pair middle)
        (let ([middle (if (null? middle)
                          (mk-empty-cfexp)
                          (make-concat middle '()))])
          (if (recursive? inverse-pair)
              (build-rec-cfe inverse-pair middle)
              (build-non-rec-cfe inverse-pair middle))))

      ;;(listof inverse-pair) (listof cfe-template) -> cfe
      ;;Purpose: Converts the given inverse pairs into a cfe
      (define (inverse-pairs->cfe inverse-pairs middle)
        (let ([middle (if (null? middle)
                          (mk-empty-cfexp)
                          (make-concat middle '()))]
              [recursive-inverse-pairs (filter recursive? inverse-pairs)]
              [non-recursive-inverse-pairs (filter-not recursive? inverse-pairs)])
          (cond [(and (not (null? recursive-inverse-pairs))
                      (not (null? non-recursive-inverse-pairs)))
                 (mk-union-cfexp (vector (build-rec-cfes recursive-inverse-pairs middle)
                                         (build-non-rec-cfes non-recursive-inverse-pairs middle)))]
                [(and (null? recursive-inverse-pairs)
                      (not (null? non-recursive-inverse-pairs)))
                 (build-non-rec-cfes inverse-pairs middle)]
                [(and (not (null? recursive-inverse-pairs))
                      (null? non-recursive-inverse-pairs))
                 (build-rec-cfes inverse-pairs middle)])))

      ;;inverse-pair cfe -> cfe
      ;;Purpose: Builds a recursive cfe using middle as the base case
      (define (build-rec-cfe stack-pair middle)
        (let ([lang-box (box (void))]
              [RHS (rule->cfe inverse-pair-push stack-pair)]
              [LHS (rule->cfe inverse-pair-pop stack-pair)])
          (if (mk-union-cfexp? middle)
              (let ([cfes (mk-union-cfexp-locfe middle)])
                (mk-union-cfexp (vector-map (λ (cfe) (build-rec-cfe stack-pair cfe))  cfes)))
              (begin
                (set-box! lang-box (mk-union-cfexp
                                    (if (list? middle)
                                        (vector-append (vector (mk-concat-cfexp
                                                                (if (mk-concat-cfexp? LHS)
                                                                    (vector-append (vector RHS lang-box)
                                                                                   (mk-concat-cfexp-locfe LHS))
                                                                    (vector RHS lang-box LHS))))
                                                       (list->vector middle))
                                        (vector (mk-concat-cfexp (if (mk-concat-cfexp? LHS)
                                                                     (vector-append (vector RHS lang-box)
                                                                                    (mk-concat-cfexp-locfe LHS))
                                                                     (vector RHS lang-box LHS)))
                                                middle))))
                lang-box))))
           
      ;;inverse-pair cfe -> cfe
      ;;Purpose: Builds a non-recursive cfe 
      (define (build-non-rec-cfe stack-pair middle)
        (let ([RHS (rule->cfe inverse-pair-push stack-pair)]
              [LHS (rule->cfe inverse-pair-pop stack-pair)])
          (if (mk-union-cfexp? middle)
              (let ([cfes (mk-union-cfexp-locfe middle)])
                (mk-union-cfexp (vector-map (λ (cfe) (build-non-rec-cfe stack-pair cfe))  cfes)))
              (mk-concat-cfexp (if (mk-concat-cfexp? LHS)
                                   (vector-append (if (list? middle)
                                                      (list->vector (cons RHS middle))
                                                      (vector RHS middle) )
                                                  (mk-concat-cfexp-locfe LHS))
                                   (if (list? middle)
                                       (list->vector (append (cons RHS middle) (list LHS)))
                                       (vector RHS middle LHS)))))))

      ;;(listof inverse-pair) cfe -> cfe
      ;;Purpose: Builds a non-recursive cfe
      ;;Note: Used when there are multiple rules on the same states
      (define (build-non-rec-cfes stack-pairs middle)
        (let* ([RHS (map (λ (inverse-pair) (rule->cfe inverse-pair-push inverse-pair)) stack-pairs)]
               [LHS (map (λ (inverse-pair) (rule->cfe inverse-pair-pop inverse-pair)) stack-pairs)])
          (mk-union-cfexp (list->vector (map (λ (RHS LHS)
                                               (mk-concat-cfexp (if (mk-concat-cfexp? LHS)
                                                                    (vector-append (if (list? middle)
                                                                                       (list->vector (cons RHS middle))
                                                                                       (vector RHS middle))
                                                                                   (mk-concat-cfexp-locfe LHS))
                                                                    (if (list? middle)
                                                                    (list->vector (append (cons RHS middle) (list LHS)))
                                                                    (vector RHS middle LHS)))))
                                             RHS
                                             LHS)))))

      ;;(listof inverse-pair) cfe -> cfe
      ;;Purpose: Builds a recursive cfe using middle as the base case
      ;;Note: Used when there are multiple rules on the same states
      (define (build-rec-cfes stack-pairs middle)
        (let* ([lang-box (box (void))]
               [RHS (map (λ (inverse-pair) (rule->cfe inverse-pair-push inverse-pair)) stack-pairs)]
               [LHS (map (λ (inverse-pair) (rule->cfe inverse-pair-pop inverse-pair)) stack-pairs)])
          (begin
            (set-box! lang-box (mk-union-cfexp (list->vector
                                                (append (map (λ (RHS LHS)
                                                               (mk-concat-cfexp
                                                                (if (mk-concat-cfexp? LHS)
                                                                    (vector-append (vector RHS lang-box)
                                                                                   (mk-concat-cfexp-locfe LHS))
                                                                    (vector RHS lang-box LHS))))
                                                             RHS
                                                             LHS)
                                                        (if (list? middle) middle (list middle))))))
            lang-box)))      
      
      ;;(listof cfe-template) (listof cfe) -> (listof cfe)
      ;;Purpose: Makes a concat cfexp
      (define (make-concat sub-lang acc)
        (cond [(null? sub-lang) (cond [(null? acc) (mk-empty-cfexp) #;(mk-null-cfexp)]
                                      [(is-length-one? acc) (first acc)]
                                      [else (mk-concat-cfexp (list->vector (reverse acc)))])]
              [(and (singleton? (extractor-prims (first sub-lang)))
                    (member-of? (first sub-lang) (map inverse-pair-push stack-oper)))
               (finish-concat (rest sub-lang) (get-stack-oper inverse-pair-push (first sub-lang) stack-oper) acc)]
              [(and (list? (extractor-prims (first sub-lang)))
                    (members-of? (first sub-lang) (map inverse-pair-push stack-oper)))
               (finish-concat (rest sub-lang) (get-stack-opers inverse-pair-push (first sub-lang) stack-oper) acc)]
              [(member-of? (first sub-lang) (flatten (map inverse-pair-pop stack-oper)))
               (make-concat (rest sub-lang) acc)]
              [else (make-concat (rest sub-lang) (cons (build-cfe (first sub-lang)) acc))]))
      
      ;;(listof cfe-template) (listof stack-pair) (listof cfe) -> cfe
      ;;Purpose: Builds a cfexp
      (define (finish-concat sub-lang stack-pair acc)
        ;;(listof pda-rule) state -> Boolean
        ;;Purpose: Determines if the given pda-rules goes to the same state
        (define (goes-to-same-state? pda-rules state)
          (andmap (λ (rule) (eq? state (pda-rule-destin rule))) pda-rules))

        ;;inverse-pair (listof cfe-template) -> (listof cfe-template)
        ;;Purpose: Returns the (listof cfe-template) that comes AFTER the inverse-pair pop rule
        (define (search-for-after stack-pair sub-langs)
          (if (null? sub-langs)
              sub-langs
              (let ([rule (extractor-prims (first sub-langs))])
                (if (or (and (list? rule) (same-rules? inverse-pair-pop stack-pair rule))
                        (and (singleton? rule) (same-rule? inverse-pair-pop stack-pair rule)))
                    (rest sub-langs)
                    (search-for-after stack-pair (rest sub-langs))))))

        ;;inverse-pair (listof cfe-template) (listof cfe-template) -> (listof cfe-template)
        ;;Purpose: Returns the (listof cfe-template) that comes BEFORE the inverse-pair pop rule
        ;;ACC: The cfe-template found before the inverse-pair pop rule in reverse order
        (define (search-for-before stack-pair sub-langs acc)
          (if (null? sub-langs)
              sub-langs
              (let ([rule (flatten (extractor-prims (first sub-langs)))])
                (if (or (and (list? rule) (same-rules? inverse-pair-pop stack-pair rule))
                        (and (singleton? rule) (same-rule? inverse-pair-pop stack-pair rule)))
                    (reverse acc)
                    (search-for-before stack-pair (rest sub-langs) (cons (first sub-langs) acc))))))
        
        ;;(listof cfe-template) (listof inverse-pair) -> (listof cfe-template)
        ;;Purpose: Returns all of the cfe-templates AFTER the given pop rule from the inverse pair
        (define (get-temps-after sub-langs stack-pairs)
          (cond [(and (is-length-one? stack-pairs) (singleton? (inverse-pair-pop (first stack-pairs))))
                 (search-for-after (first stack-pairs) sub-langs)]
                [(and (is-length-one? stack-pairs) (list? (inverse-pair-pop (first stack-pairs))))
                 (search-for-after (struct-copy inverse-pair (first stack-pairs)
                                                [pop (last (inverse-pair-pop (first stack-pairs)))]) sub-langs)]
                [(let ([pda-rules (flatten (map inverse-pair-pop stack-pairs))])
                   (goes-to-same-state? pda-rules (pda-rule-destin (first pda-rules))))
                 (search-for-after (last stack-pairs) sub-langs)]
                [else (error (format "i need to think: ~a" stack-pairs))]))

        ;;(listof cfe-template) (listof inverse-pair) -> (listof cfe-template)
        ;;Purpose: Returns all of the cfe-templates BEFORE the given pop rule from the inverse pair
        (define (get-temps-before sub-langs stack-pairs)
          (cond [(and (is-length-one? stack-pairs) (singleton? (inverse-pair-pop (first stack-pairs))))
                 (search-for-before (first stack-pairs) sub-langs (list))]
                [(and (is-length-one? stack-pairs) (list? (inverse-pair-pop (first stack-pairs))))
                 (search-for-before (struct-copy inverse-pair (first stack-pairs)
                                                 [pop (first (inverse-pair-pop (first stack-pairs)))]) sub-langs (list))]
                [(let ([pda-rules (flatten (map inverse-pair-pop stack-pairs))])
                   (goes-to-same-state? pda-rules (pda-rule-destin (first pda-rules))))
                 (search-for-before (first stack-pairs) sub-langs (list))]
                [else (error (format "i need to think: ~a" stack-pairs))]))

        #|
          cfe-info is a struct outline how to build a concat-cfexp
          temp-after  | the cfe-templates after the pop rule of the stack pair  => (listof cfe-template)
          temp-before | the cfe-templates before the pop rule of the stack pair => (listof cfe-template)
          stack-pair  | the stack-pair to used to build the concat-cfexp => inverse-pair
        |#
        (struct cfe-info (temp-after temp-before stack-pair) #:transparent)

        ;;(listof cfe-template) (listof cfe-template) (listof inverse-pair) (listof cfe-template) -> cfe
        ;;Purpose: Makes many concat-cfexp using the given (listof cfe-template) (listof cfe-template) (listof inverse-pair)
        (define (make-concats temps-before temps-after stack-pair sub-lang)

          ;;(listof cfe-template) cfe -> cfe
          ;;Purpose: Makes cfexps from the templates after the given rule
          (define (make-concats-after temps-after middle)
            (cond [(null? temps-after) middle]
                  [(is-length-one? temps-after)
                   (let* ([temp (first temps-after)]
                          [stack-pair (get-stack-oper inverse-pair-pop temp stack-pair)])
                     (cond [(null? stack-pair) (mk-concat-cfexp (vector middle (build-cfe temp)))]
                           [(is-length-one? stack-pair)
                            (if (recursive? (first stack-pair))
                                (build-rec-cfe (first stack-pair) middle)
                                (build-non-rec-cfe (first stack-pair) middle))]
                           [else (let ([recursive-inverse-pairs (filter recursive? stack-pair)]
                                       [non-recursive-inverse-pairs (filter-not recursive? stack-pair)])
                                   (cond [(and (not (null? recursive-inverse-pairs))
                                               (not (null? non-recursive-inverse-pairs)))
                                          (mk-union-cfexp (vector (build-rec-cfes recursive-inverse-pairs middle)
                                                                  (build-non-rec-cfes non-recursive-inverse-pairs middle)))]
                                         [(and (null? recursive-inverse-pairs)
                                               (not (null? non-recursive-inverse-pairs)))
                                          (build-non-rec-cfes stack-pair middle)]
                                         [(and (not (null? recursive-inverse-pairs))
                                               (null? non-recursive-inverse-pairs))
                                          (build-rec-cfes stack-pair middle)]))]))]
                  [else (mk-union-cfexp (list->vector (map (λ (temps) (make-concats-after (list temps) middle)) temps-after)))]))

          ;;(listof cfe-templates) inverse-pair -> cfe
          ;;Purpose: Makes cfexps from the templates before 
          (define (make-concats-before temps-before stack-pair)
            ;;(listof cfe-template) (listof cfe-template) (listof cfe) -> cfe
            ;;Purpose: Makes a concat cfexp
            (define (make-concat-helper sub-lang unusables acc)
              (cond [(null? sub-lang) (cond [(null? acc) (mk-empty-cfexp) #;(mk-null-cfexp)]
                                            [(is-length-one? acc) (first acc)]
                                            [else (mk-concat-cfexp (list->vector (reverse acc)))])]
                    [(and (singleton? (extractor-prims (first sub-lang)))
                          (member-of? (first sub-lang) (map inverse-pair-push stack-oper)))
                     (finish-concat (rest sub-lang)
                                    (filter-not (λ (stack-pair)
                                                  (same-rules? inverse-pair-pop stack-pair unusables))
                                                (get-stack-oper inverse-pair-push (first sub-lang) stack-oper))
                                    acc)]
                    [(and (list? (extractor-prims (first sub-lang)))
                          (members-of? (first sub-lang) (map inverse-pair-push stack-oper)))
                     (finish-concat (rest sub-lang)
                                    (filter-not (λ (stack-pair)
                                                  (same-rules? inverse-pair-pop stack-pair unusables))
                                                (get-stack-opers inverse-pair-push (first sub-lang) stack-oper))
                                    acc)]
                    [(member-of? (first sub-lang) (flatten (map inverse-pair-pop stack-oper)))
                     (make-concat-helper (rest sub-lang) unusables acc)]
                    [else (make-concat-helper (rest sub-lang) unusables (cons (build-cfe (first sub-lang)) acc))]))
            (let* ([forbidden-rules (map extractor-prims (search-for-after stack-pair sub-lang))]
                   [middle (cond [(null? temps-before) (mk-empty-cfexp)]
                                 [(is-length-one? temps-before) (make-concat-helper temps-before forbidden-rules '())]
                                 [else (map (λ (middle) (make-concat-helper (list middle) forbidden-rules '())) temps-before)])])
              (if (recursive? stack-pair)
                  (if (list? middle)
                      (mk-union-cfexp
                       (list->vector
                        (map (λ (cfe)
                               (build-rec-cfe stack-pair cfe))
                             middle)))
                      (build-rec-cfe stack-pair middle))
                  (if (list? middle)
                      (mk-union-cfexp
                       (list->vector
                        (map (λ (cfe)
                               (build-non-rec-cfe stack-pair cfe))
                             middle)))
                      (build-non-rec-cfe stack-pair middle)))))            
          ;;cfe-info -> cfe
          ;;Purpose: Converts concat-templates to a cfexp
          (define (concat-temps->cfe concat-temp)
            (let* ([temps-after (cfe-info-temp-after concat-temp)]
                   [temps-before (cfe-info-temp-before concat-temp)]
                   [stack-pair (cfe-info-stack-pair concat-temp)]
                   [middle (if (recursive? stack-pair)
                               (build-rec-cfe stack-pair (mk-empty-cfexp))
                               (build-non-rec-cfe stack-pair (mk-empty-cfexp)))]
                   [cfes-after (make-concats-after temps-after middle)]
                   [cfes-before (make-concats-before temps-before stack-pair)])
              (mk-union-cfexp (vector cfes-after cfes-before))))                
          (let ([concat-temps (map (λ (after before pair) (cfe-info after before pair)) temps-after temps-before stack-pair)])
            (mk-union-cfexp (list->vector (map concat-temps->cfe concat-temps)))))
                    
        (cond [(and (is-length-one? stack-pair) (not (list? (inverse-pair-pop (first stack-pair)))))
               (make-concat (search-for-after (first stack-pair) sub-lang)
                            (cons (inverse-pair->cfe (first stack-pair)
                                                     (search-for-before (first stack-pair) sub-lang (list)))
                                  acc))]
              [(or (and (is-length-one? stack-pair) (list? (inverse-pair-pop (first stack-pair))))
                   (let ([pda-rules (flatten (map inverse-pair-pop stack-pair))]) ;flatten might be an issue
                     (goes-to-same-state? pda-rules (pda-rule-destin (first pda-rules)))))
               (make-concat (get-temps-after sub-lang stack-pair)
                            (cons (inverse-pairs->cfe stack-pair
                                                      (get-temps-before sub-lang stack-pair))
                                  acc))]
              [else (make-union sub-lang
                                (cons (make-concats (map (λ (stack-pair)
                                                           (search-for-before stack-pair sub-lang (list)))
                                                         stack-pair)
                                                    (map (λ (stack-pair)
                                                           (search-for-after stack-pair sub-lang))
                                                         stack-pair)
                                                    stack-pair
                                                    sub-lang)
                                      acc))]))
      
      ;;(listof cfe-template) (listof cfe) -> cfe
      ;;Purpose: Makes a union-cfexp
      (define (make-union sub-lang acc)
            (cond [(null? sub-lang) (cond [(null? acc) (mk-empty-cfexp) #;(mk-null-cfexp)]
                                      [(is-length-one? acc) (first acc)]
                                      [else (mk-union-cfexp (list->vector (reverse acc)))])]
              [(and (singleton? (extractor-prims (first sub-lang)))
                    (member-of? (first sub-lang) (map inverse-pair-push stack-oper)))
               (finish-concat (rest sub-lang) (get-stack-oper inverse-pair-push (first sub-lang) stack-oper) acc)]
              [(and (list? (extractor-prims (first sub-lang)))
                    (members-of? (first sub-lang) (map inverse-pair-push stack-oper)))
               (finish-concat (rest sub-lang) (get-stack-opers inverse-pair-push (first sub-lang) stack-oper) acc)]
              [(member-of? (first sub-lang) (flatten (map inverse-pair-pop stack-oper)))
               (make-union (rest sub-lang) acc)]
              [else (make-union (rest sub-lang) (cons (build-cfe (first sub-lang)) acc))]))
      (cond [(union? sub-lang) (make-union (union-rules sub-lang) '())]
            [(concat? sub-lang) (make-concat (concat-rules sub-lang) '())]
            [(kleene? sub-lang) (mk-kleene-cfexp (build-cfe-helper (kleene-rule sub-lang) stack-oper))]
            [else (build-cfe sub-lang)]))
    (let ([res (map (λ (sub-lang stack-oper) (build-cfe-helper sub-lang stack-oper)) sub-langs stack-operations)])
      (if (is-length-one? res)
          (first res)
          (mk-union-cfexp (list->vector res)))))

  ;;pda -> Boolean
  ;;Purpose: Determines if the given pda simulates a grammar
  (define (simulates-grammar? P)
    (and (= (length (pda-getstates P)) 2)
         (null? (filter (λ (rule)
                          (eq? (first (second rule)) (pda-getstart P)))
                        (pda-getrules P)))
         (is-length-one? (filter (λ (rule)
                                   (and (eq? EMP (second (first rule)))
                                        (eq? EMP (third (first rule)))
                                        (list? (second (second rule)))
                                        (is-length-one? (second (second rule)))
                                        (eq? (first (first rule)) (pda-getstart P))))
                                 (pda-getrules P)))))

  ;;(listof (list (list sym symbol/(listof symbol) symbol/(listof symbol)) (list sym symbol/(listof symbol))) -> cfg
  ;;Purpose: Converts the rules of pda into a cfg
  (define (rules->cfg P)
    (let* ([rule-structs (map (λ (rule)
                                (pda-rule (first (first rule))
                                          (pda-action (second (first rule)) (third (first rule)) (second (second rule)))
                                          (first (second rule))))
                              (pda-getrules P))]
           [start-nt (first (pda-action-push (pda-rule-action (first (filter (λ (rule)
                                                                                    (eq? (pda-rule-source rule) (pda-getstart P)))
                                                                                  rule-structs)))))]
           [terminals (filter-map (λ (rule)
                                    (and (not (eq? EMP (pda-action-read (pda-rule-action rule))))
                                         (pda-action-read (pda-rule-action rule))))
                                  rule-structs)]
           [production-temps (filter-map (λ (rule)
                                           (and (and (eq? EMP (pda-action-read (pda-rule-action rule)))
                                                     (eq? (pda-rule-source rule) (first (pda-getfinals P)))
                                                     (eq? (pda-rule-destin rule) (first (pda-getfinals P))))
                                                (pda-rule-action rule)))
                                         rule-structs)])      
      (make-unchecked-cfg (remove-duplicates (map (λ (action)
                                                    (first (pda-action-pop action)))
                                                  production-temps))
                          terminals
                          (map (λ (action)
                                 (list (first (pda-action-pop action)) ARROW (if (eq? (pda-action-push action) EMP)
                                                                                  (pda-action-push action)
                                                                                  (string->symbol (foldl (λ (sym acc)
                                                                                           (string-append acc (symbol->string sym)))
                                                                                         ""
                                                                                         (pda-action-push action))))))
                               production-temps)
                          start-nt)))
  (cond [(null? (pda-getrules P)) (mk-null-cfexp)]
        [(simulates-grammar? P) (cfg->cfe (rules->cfg P))]
        [else (let* ([new-P (pda2temp P)]
                     [sub-langs (make-sub-languages (pda-rule-action (pda-rules new-P)))]
                     [rule-structs (map (λ (rule)
                                          (pda-rule (first (first rule))
                                                    (pda-action (second (first rule)) (third (first rule)) (second (second rule)))
                                                    (first (second rule))))
                                        (pda-getrules P))]
                     [reachable-rules (find-reachables rule-structs)]
                     [stack-operations (pair-stack-operations reachable-rules rule-structs)]
                     [sublang-stack-pairs (map (λ (sub-lang) (find-applicable-opers sub-lang stack-operations)) sub-langs)])
                (if (and (andmap (λ (sub-lang stack-pair)
                                   (and (null? stack-pair)
                                        (uses-stack? sub-lang)))
                                 sub-langs
                                 sublang-stack-pairs)
                         (set-member? (list->set (pda-getfinals P)) (pda-getstart P)))
                    (mk-empty-cfexp)
                    (build-cfe sub-langs sublang-stack-pairs)))]))


;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define (cfe->pda cfe)
  ;;vars    | the accumulated variables found from traversing the given cfe  | (listof union-cfexp)
  ;;singles | the accumulated singletons found from traversing the given cfe | (setof singleton-cfexp)
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

  ;;(X -> Y) Z (treelistof X) -> Z
  (define (tl-foldl f acc tl)
    (if (treelist-empty? tl)
        acc
        (tl-foldl f (f (treelist-first tl) acc) (treelist-rest tl))))
  
  ;;cfe -> extraction-results
  ;;Purpose: Extracts all var-cfexp and singleton-cfexp from the given cfe
  (define (extract-var-and-singles-cfe cfe)
    ;;cfe -> (listof cfe)
    ;;Purpose: Extracts the sub-expressions from the given cfe
    (define (extract-cfe-data cfe)
      (cond [(mk-concat-cfexp? cfe) (vector->treelist (mk-concat-cfexp-locfe cfe))]
            [(mk-union-cfexp? cfe) (vector->treelist (mk-union-cfexp-locfe cfe))]
            [(mk-kleene-cfexp? cfe) (treelist (mk-kleene-cfexp-cfe cfe))]
            [(box? cfe) (treelist (unbox cfe))]
            [else empty-treelist]))
    ;;cfe extraction-results -> extraction-results
    ;;Purpose: Updates the given extraction-results to add the given cfe if it is a singleton or variable
    (define (update-extraction-results cfe extract-res)
      (cond [(or (mk-kleene-cfexp? cfe) (box? cfe))
             (struct-copy extraction-results
                          extract-res
                          [lang-boxes (set-add (extraction-results-lang-boxes extract-res) cfe)])]
            [(mk-singleton-cfexp? cfe)
             (struct-copy extraction-results
                          extract-res
                          [singles (set-add (extraction-results-singles extract-res) (string->symbol (mk-singleton-cfexp-char cfe)))])]
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
    (let ([init-queue (tl-foldl (λ (env acc)
                                  (enqueue acc (treelist env)))
                                E-QUEUE
                                (extract-cfe-data cfe))])
      (extract-var-and-singles init-queue
                               (update-extraction-results cfe (extraction-results (set) (set)))
                               (set cfe))))

  #|
  pda-struct is a structural representation of a pda
  states | The states for the given pda => (listof states)
  sigma  | The alphabet that the given pda works over => (listof symbol)
  gamma  | The stack alphabet that given pda works over => (listof symbol)
  start  | The starting state => symbol
  finals | The final states => (listof symbol)
  rules  | The transition relation for the given pda => (listof pda-rule)
  |#
  (struct pda (states sigma gamma start finals rules) #:transparent)

  #|
  pda-rule is a structural representation of a pda rule
  source | The state the rule is coming from => symbol
  action | The action the pda takes when using the rule => pda-action
  destin | The state the rule transitions to => symbol
  |#
  (struct pda-rule (source action destin) #:transparent)

  #|
  a pda-action is a structural representation of a pda action
  read | The element that the pda reads => symbol
  pop  | The element(s) that the pda pops of the stack => symbol / (listof symbol)
  push | The element(s) that the pda pushes to the stack => symbol / (listof symbol)
  |#
  (struct pda-action (read pop push) #:transparent)

  (define E-TRANSITION (pda-action EMP EMP EMP))

  ;;pda pda -> pda
  ;;Purpose: Constructions a pda for the union of the langauges of the given pda
  ;;ASSUME: P1 and P2 states are disjoint
  (define (pda-union P1 P2)
    (let* ([old-states (append (pda-states P1) (pda-states P2))]
           [new-start (gen-state old-states)]
           [new-states (cons new-start old-states)]
           [new-sigma (remove-duplicates (append (pda-sigma P1) (pda-sigma P2)))]
           [new-gamma (remove-duplicates (append (pda-gamma P1) (pda-gamma P2)))]
           [new-finals (append (pda-finals P1) (pda-finals P2))]
           [new-rules (append (list (pda-rule new-start E-TRANSITION (pda-start P1))
                                    (pda-rule new-start E-TRANSITION (pda-start P2)))
                              (pda-rules P1)
                              (pda-rules P2))])
    (pda new-states new-sigma new-gamma new-start new-finals new-rules)))

  ;;pda pda -> pda
  ;;Purpose: Constructions a pda for the concatenation of the langauges of the given pda
  ;;ASSUME: P1 and P2 states are disjoint
  (define (pda-concat P1 P2)
    (let* ([old-states (append (pda-states P1) (pda-states P2))]
           [new-start (gen-state old-states)]
           [old-gamma (remove-duplicates (append (pda-gamma P1) (pda-gamma P2)))]
           [new-gamma-sym (gen-state old-gamma)]
           [new-states (cons new-start old-states)]
           [new-sigma (remove-duplicates (append (pda-sigma P1) (pda-sigma P2)))]
           [new-gamma (cons new-gamma-sym old-gamma)]
           [new-finals (pda-finals P2)]
           [new-rules (cons (pda-rule new-start (pda-action EMP EMP (list new-gamma-sym)) (pda-start P1))
                            (append (pda-rules P1)
                                    (pda-rules P2)
                                    (map (λ (final)
                                           (pda-rule final (pda-action EMP (list new-gamma-sym) EMP) (pda-start P2)))
                                         (pda-finals P1))))])
      (pda new-states new-sigma new-gamma new-start new-finals new-rules)))

  ;;pda -> pda
  ;;Purpose: Constructions a pda for the kleenestar of the langauge of the given pda
  (define (pda-kleenestar P)
    (let* ([new-start (gen-state (pda-states P))]
           [new-gamma-sym (gen-state (pda-gamma P))]
           [new-gamma (cons new-gamma-sym (pda-gamma P))]
           [new-states (cons new-start (pda-states P))]
           [new-sigma (pda-sigma P)]
           [new-finals (list new-start)]
           [new-rules (cons (pda-rule new-start (pda-action EMP EMP (list new-gamma-sym)) (pda-start P))
                            (append (pda-rules P)
                                    (map (λ (final)
                                           (pda-rule final (pda-action EMP (list new-gamma-sym) EMP) new-start))
                                         (pda-finals P))))])
      (pda new-states new-sigma new-gamma new-start new-finals new-rules)))

  ;;(listof state) pda -> pda
  ;;Purpose: Renames the states of the second pda using the states of first pda
  (define (rename-pda old-states P2)
    ;;natnum -> (listof state)
    ;;Purpose: Generates natnum amount of states
    (define (gen-states num)
      (for/fold ([states '()])
                ([x (in-range num)])
        (cons (gen-state (append old-states states)) states)))
    (let* ([new-states (gen-states (length (pda-states P2)))]
           [associated-state (foldl (λ (new-state old-state acc)
                                      (hash-set acc old-state new-state))
                                    (hash)
                                    new-states
                                    (pda-states P2))])
      (pda new-states
           (pda-sigma P2)
           (pda-gamma P2)
           (hash-ref associated-state (pda-start P2))
           (map (λ (final)
                  (hash-ref associated-state final))
                (pda-finals P2))
           (map (λ (rule)
                  (struct-copy pda-rule rule
                               [source (hash-ref associated-state (pda-rule-source rule))]
                               [destin (hash-ref associated-state (pda-rule-destin rule))]))
                (pda-rules P2)))))

  ;;pda -> ndpda
  ;;Purpose: Converts a pda to an ndpda
  (define (pda->unchecked P)
    (make-unchecked-ndpda (pda-states P)
                          (pda-sigma P)
                          (pda-gamma P)
                          (pda-start P)
                          (pda-finals P)
                          (map (λ (rule)
                                 (list (list (pda-rule-source rule) (pda-action-read (pda-rule-action rule)) (pda-action-pop (pda-rule-action rule)))
                                       (list (pda-rule-destin rule) (pda-action-push (pda-rule-action rule)))))
                               (pda-rules P))))
     
  
  (let* ([extract-res (extract-var-and-singles-cfe cfe)]
         [alphabet (set->list (extraction-results-singles extract-res))]
         [sigma-pdas (foldl (λ (sigma acc)
                              (hash-set acc sigma (pda
                                                   '(S F)
                                                   (list sigma)
                                                   '()
                                                   'S
                                                   '(F)
                                                   (list (pda-rule 'S (pda-action sigma EMP EMP) 'F)))))
                            (hash)
                            alphabet)])
    (values (sm-graph (pda->unchecked (pda-union (hash-ref sigma-pdas 'a) (rename-pda (pda-states (hash-ref sigma-pdas 'a)) (hash-ref sigma-pdas 'b)))))
            (sm-graph (pda->unchecked (pda-concat (hash-ref sigma-pdas 'b) (rename-pda (pda-states (hash-ref sigma-pdas 'b)) (hash-ref sigma-pdas 'c)))))
            (sm-graph (pda->unchecked (pda-kleenestar (hash-ref sigma-pdas 'c)))))
                      
    #;(cfg->pda (cfe->cfg cfe))))