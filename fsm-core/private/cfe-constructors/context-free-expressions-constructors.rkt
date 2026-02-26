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
         racket/format)

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
    (if (null? lst-words) EMP (append-map append lst-words))))

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
(define/contract (gen-cfexp-word cfe . reps)
  gen-cfexp-word/c
  (define MAX-KLEENESTAR-REPS (if (null? reps) MAX-KLEENESTAR-LIMIT (car reps)))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-cfexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (list (string->symbol (mk-singleton-cfexp-char cfe)))]
        [(box? cfe) (gen-cfexp-word (unbox cfe) reps)]
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
  finals | The final states => (listof symbol)
  rules  | The transition relation for the given pda => (listof pda-rule)
  |#
  (struct pda (states sigma gamma start finals rules) #:transparent)

  #|
  pda-rule is a structural representation of a pda rule
  source | The state the rule is coming from => symbol
  action | The action the pda takes when using the rule => pda-action
  destin | The state the rule transitions to => symbol
  tag    | The cfe-template for the given rule => symbol / cfe-template
  |#
  (struct pda-rule (source action destin tag) #:transparent)

  #|
  a pda-action is a structural representation of a pda action
  read | The element that the pda reads => symbol
  pop  | The element(s) that the pda pops of the stack => symbol / (listof symbol)
  push | The element(s) that the pda pushes to the stack => symbol / (listof symbol)
  |#
  (struct pda-action (read pop push) #:transparent)

  (struct inverse-pair (push pop) #:transparent)

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

  ;;symbol los los -> pda-action
  ;;Purpose: Creates a pda action from the given input
  (define (make-pda-action read pop push)
    (pda-action read pop push))

  ;;(list (list state symbol los) (list state los)) -> pda-rule
  ;;Purpose: Converts the given pda rule into a pda-rule struct
  (define (rule->struct rule)
    (pda-rule (first (first rule))
              (make-pda-action (second (first rule)) (third (first rule)) (second (second rule)))
              (first (second rule))
              'none))

  ;;pda-rule -> (list (list state symbol los) (list state los))
  ;;Purpose: Converts the given pda-rule to a pda rule
  (define (struct->rules rule) 
    (list (list (pda-rule-source rule)
                (pda-action-read (pda-rule-action rule))
                (pda-action-pop (pda-rule-action rule)))
          (list (pda-rule-destin rule)
                (pda-action-push (pda-rule-action rule)))))

  ;;pda-action pda-action -> Boolean
  ;;Purpose: Determines if the given pda-actions have inverse stack operations
  (define (inverse-stack-operations? push-action pop-action)
    (let ([push (pda-action-push push-action)]
          [pop (pda-action-pop pop-action)])
      (and (equal? pop push)
           (not (eq? pop EMP))
           (not (eq? push EMP)))))

  ;;pda -> pda-struct
  ;;Purpose: Converts the given pda into a pda-struct
   (define (unchecked->pda P)
    (pda (pda-getstates P)
         (pda-getalphabet P)
         (pda-getgamma P)
         (pda-getstart P)
         (pda-getfinals P)
         (map rule->struct (pda-getrules P))))

  ;;pda-struct -> pda
  ;;Purpose: Converts the given pda-struct into a pda
  (define (pda->unchecked P)
    (make-unchecked-ndpda (pda-states P)
                          (pda-sigma P)
                          (pda-gamma P)
                          (pda-start P)
                          (pda-finals P)
                          (map struct->rules (pda-rules P))))

  ;;pda-rule -> Boolean
  ;;Purpose: Determines if the given pda-rule is an empty transition
  (define (e-transition? pda-rule)
    (let ([action (pda-rule-action pda-rule)])
      (and (eq? EMP (pda-action-read action))
           (eq? EMP (pda-action-pop action))
           (eq? EMP (pda-action-push action)))))

  ;;pda-rule -> Boolean
  ;;Purpose: Determines if the given pda-rule only reads an letter in sigma
  (define (read-only? pda-rule)
    (let ([action (pda-rule-action pda-rule)])
      (and (not (eq? EMP (pda-action-read action)))
           (eq? EMP (pda-action-pop action))
           (eq? EMP (pda-action-push action)))))

  ;;pda-rule -> Boolean
  ;;Purpose: Determines if the given pda-rule is a self loop
  (define (self-loop? pda-rule)
    (eq? (pda-rule-source pda-rule) (pda-rule-destin pda-rule))) 

  ;;pda-rule -> cfe-template
  ;;Purpose: Converts the given pda-rule to a cfe-template
  (define (rules->cfe pda-rule)
    (if (e-transition? pda-rule)
        (empty pda-rule) 
        (singleton pda-rule)))

  ;;(listof pda-rule) -> cfe-template
  ;;Purpose: Converts the given (listof pda-rule) to a cfe-template
  (define (rules->tag rules)
    (let ([t-rules (map rules->cfe rules)])
      (cond [(null? t-rules) rules]
            [(is-length-one? t-rules) (first t-rules)]
            [else (union t-rules)])))

  ;;(listof cfe-template) -> (listof cfe-template)
  ;;Purpose: Converts given (listof cfe-template) to a cfe-kleene-template
  (define (make-kleene-tag t-rule)
    (if (null? t-rule)
        t-rule
        (kleene t-rule)))

  (define (make-new-tag t-rules)
    (let ([rules (flatten (map template->rules t-rules))])
    (if (eq? (pda-rule-source (first rules)) (pda-rule-destin (last rules)))
        (kleene (concat t-rules))
        (concat t-rules))))

  ;;(listof pda-rule) -> pda-rule
  ;;Purpose: If the given list has many rules (e.i more than 1), merge their tags together and create a union.
  ;;         Otherwise return the given list
  (define (merge-rules rules)
    (if (or (null? rules)(is-length-one? rules))
        rules
        (list (struct-copy pda-rule (first rules)
                     [tag (union (map pda-rule-tag rules))]))))


  ;(define (update-rule (pda-rule-action rule)frm-ripped-state-rules)

  
  ;;pda-struct -> pda-struct
  ;;Purpose: Recursively rips nodes out from the given M and converts the ripped nodes to cfe-templates
  (define (rip-nodes M)
    (let ([states-to-rip-out (filter (λ (state) (and (not (eq? state (pda-start M)))
                                                     (not (eq? state (first (pda-finals M))))))
                                     (pda-states M))])
          (if (null? states-to-rip-out)
              (let ([start (pda-start M)]
                    [final (first (pda-finals M))]
                    [tag (simplify-templates (pda-rule-tag (first (merge-rules (pda-rules M)))))]
                    [space (string->symbol "")])
                
                (struct-copy pda M
                           [rules (list #;(first (merge-rules (pda-rules M)))
                                        (pda-rule start (pda-action (string->symbol (printable-tag tag)) space space) final tag))]))
              (let* ([state-to-rip (first states-to-rip-out)]
                     [rules-frm-ripped-state (filter (λ (rule) (or (eq? state-to-rip (pda-rule-source rule))
                                                                   (eq? state-to-rip (pda-rule-destin rule))))
                                                     (pda-rules M))]
                     [frm-ripped-state-rules (filter (λ (rule) (and (not (self-loop? rule))
                                                              (eq? state-to-rip (pda-rule-source rule))))
                                                     rules-frm-ripped-state)]
                     [to-ripped-state-rules (filter (λ (rule) (and (not (self-loop? rule))
                                                                   (not (and (eq? (pda-start M) (pda-rule-source rule))
                                                                        (eq? EMP (pda-action-read (pda-rule-action rule)))))
                                                              (eq? state-to-rip (pda-rule-destin rule))))
                                                     rules-frm-ripped-state)]
                     [self-loop-rules (filter (λ (rule) (self-loop? rule)) rules-frm-ripped-state)]
                     [extracted-tags (flatten (filter-not symbol? (map pda-rule-tag rules-frm-ripped-state)))]
                     [translated-frm-rules (list (rules->tag frm-ripped-state-rules))]
                     [translated-to-rules (cond [(concat? extracted-tags) (concat-rules (first extracted-tags))]
                                                [(kleene? extracted-tags) (kleene-rule (first extracted-tags))]
                                                [else (list (rules->tag to-ripped-state-rules))])]
                     [translated-self-loop-rules (list (make-kleene-tag (rules->tag self-loop-rules)))]
                     [new-tag (make-new-tag (filter-not null? (append translated-to-rules
                                                                      translated-self-loop-rules
                                                                      translated-frm-rules)))]
                     [new-states-for-P (filter (λ (state) (not (eq? state-to-rip state)))
                        (pda-states M))]
                     [destins-from-ripped-state (filter (λ (rule)
                                                          (and (not (self-loop? rule))
                                                               (eq? state-to-rip (pda-rule-source rule))))
                                                 rules-frm-ripped-state)]
                     [updated-destins (map (λ (rule)
                                             (struct-copy pda-rule rule
                                                          [source (pda-start M)]
                                                          [tag new-tag]))
                                           destins-from-ripped-state)]
                     #;[updated-to-rules (map (λ (rule)
                                                (struct-copy pda-rule rule
                                                             [destin (if (eq? (first (pda-finals M)) (pda-rule-source rule))
                                                                         (first (pda-finals M))
                                                                         (pda-rule-source rule))]
                                                             #;[tag new-tag]
                                                             [action (let ([action (pda-rule-action rule)])
                                                                       (struct-copy pda-action action
                                                                                  [read (apply (compose1 string->symbol string-append)
                                                                                               (map (λ (rule)
                                                                                                      ((compose1 symbol->string pda-action-read pda-rule-action) rule))
                                                                                                    (append to-ripped-state-rules
                                                                                                            self-loop-rules
                                                                                                            frm-ripped-state-rules)))
                                                                                   ]))]))
                                                             to-ripped-state-rules
                                                             #;(filter (λ (rule)
                                                                       (and (not (eq? (pda-start M) (pda-rule-source rule)))
                                                                        (not (eq? EMP (pda-action-read (pda-rule-action rule))))))
                                                                     to-ripped-state-rules))])
                ;(pretty-print new-tag)
               #| (pretty-print rules-frm-ripped-state)
                (pretty-print frm-ripped-state-rules)
                (pretty-print to-ripped-state-rules)
                (pretty-print self-loop-rules)
                #;(pretty-print updated-to-rules)
                (pretty-print translated-frm-rules)
                (pretty-print new-tag)
                (displayln "next")
                #;(pretty-print (append updated-destins
                                      updated-to-rules
                                      (filter (λ (rule) (and (not (eq? state-to-rip (pda-rule-source rule)))
                                                                                  (not (eq? state-to-rip (pda-rule-destin rule)))))
                                                                    (pda-rules M))))
                #;(pretty-print (apply (compose1 string->symbol string-append)
                                     (map (λ (rule)
                                     ((compose1 symbol->string pda-action-read pda-rule-action) rule))
                                   (append to-ripped-state-rules
                                           self-loop-rules
                                           frm-ripped-state-rules)))
                              #;(foldl (λ (acc rule)
                                     (string-append ((compose1 symbol->string pda-action-read pda-rule-action) rule)) acc)
                                    ""
                                   (append to-ripped-state-rules
                                           self-loop-rules
                                           frm-ripped-state-rules)))
                #;(displayln (sm-graph (pda->unchecked (struct-copy pda M
                             [states new-states-for-P]
                             [rules (append updated-destins
                                            updated-to-rules
                                            (filter (λ (rule) (and (not (eq? state-to-rip (pda-rule-source rule)))
                                                                                  (not (eq? state-to-rip (pda-rule-destin rule)))))
                                                                    (pda-rules M)))]))))
                #;(struct-copy pda M
                             [states new-states-for-P]
                             [rules (append updated-destins
                                            updated-to-rules
                                            (filter (λ (rule) (and (not (eq? state-to-rip (pda-rule-source rule)))
                                                                                  (not (eq? state-to-rip (pda-rule-destin rule)))))

                                                    (pda-rules M)))])
                (rip-nodes (struct-copy pda M
                             [states new-states-for-P]
                             [rules (filter (λ (rule) (and (not (eq? state-to-rip (pda-rule-source rule)))
                                                                                  (not (eq? state-to-rip (pda-rule-destin rule)))))
                                                                  (pda-rules M))]))
                |#
                (rip-nodes (struct-copy pda M
                             [states new-states-for-P]
                             [rules (append #;updated-to-rules
                                            updated-destins
                                            (filter (λ (rule) (and (not (eq? state-to-rip (pda-rule-source rule)))
                                                                                  (not (eq? state-to-rip (pda-rule-destin rule)))))
                                                                    (pda-rules M)))]))))))

  ;;(listof cfe-template) string -> string
  ;;Purpose: Converts a (listof cfe-template) to a string
  (define (printable-helper tags connector)
    (if (null? tags)
        connector
        (if (is-length-one? tags)
        (printable-tag (first tags))
        (let ([res (printable-helper (rest tags) connector)])
          (format "~a~a~a"(printable-tag (first tags)) connector res)))))

  ;;pda-action -> string
  ;;Purpose: Converts a pda-action to a string
  (define (action->string action)
    (format "[~a~a~a]" (pda-action-read action) (pda-action-pop action) (pda-action-push action)))

  ;;cfe-template -> string
  ;;Purpose: Prints the given tag as a string
  (define (printable-tag tag)
    (cond [(symbol? tag) ""]
          [(pda-rule? tag) (format "~a~a" (action->string (pda-rule-action tag)) (printable-tag (pda-rule-tag tag)))]
          [(empty? tag) (~a (printable-tag (empty-rule tag)))]
          [(singleton? tag) (~a (printable-tag (singleton-rule tag)))]
          [(union? tag) (format "(~a)" (printable-helper (union-rules tag) " U "))]
          [(concat? tag) (printable-helper (concat-rules tag) "")]
          [else (format "(~a)*" (printable-tag (kleene-rule tag)))]))

  ;;pda -> pda-struct
  ;;Purpose: Converts given pda into a pda-struct
  (define (make-new-machine P)
    (let* ([new-states (for/fold ([st (pda-getstates P)])
                                 ([x (in-range 2)])
                         (cons (gen-state st) st))]
           [new-start (first new-states)]
           [new-final (second new-states)]
           [new-rules-to-final (for/fold ([acc '()])
                                         ([final (pda-getfinals P)])
                                 (cons (list (list final EMP EMP) (list new-final EMP)) acc))]
           [new-rules-to-start (list (list new-start EMP EMP) (list (pda-getstart P) EMP))])
      (pda new-states
           (pda-getalphabet P)
           (pda-getgamma P)
           new-start
           (list new-final)
           (map rule->struct (append (cons new-rules-to-start new-rules-to-final) (pda-getrules P))))))

  ;;union-cfe-template -> cfe-template
  ;;Purpose: Simplifies the given union-cfe-template
  (define (simplify-empty temp)
    (if (andmap empty? (union-rules temp))
        (empty (first (union-rules temp)))
        (union (map simplify-templates (union-rules temp)))))

  ;;cfe-template -> cfe-template
  ;;Purpose: Simplifies the given cfe-template
  (define (simplify-templates temp)
    (cond [(concat? temp) (concat (filter-not empty? (map simplify-templates (concat-rules temp))))]
          [(union? temp) (simplify-empty temp)]
          [(kleene? temp) (kleene (simplify-templates (kleene-rule temp)))]
          [else temp]))
      
  ;;cfe-template -> pda-rule / (listof pda-rule)
  ;;Purpose: Extracts of the rule(s) out of the given cfe-template
  (define (template->rules temp)
    (cond [(symbol? temp) temp]
          [(empty? temp) (empty-rule temp)]
          [(singleton? temp) (singleton-rule temp)]
          [(union? temp) (map template->rules (union-rules temp))]
          [(concat? temp) (map template->rules (concat-rules temp))]
          [else (template->rules (kleene-rule temp))]))

  ;;(listof pda-rule) -> (listof pda-rule)
  ;;Purpose: Extracts all the pda-rules that ONLY push to the stack
  (define (get-push-only-rules rules)
    (filter (λ (rule)
              (and (list? (pda-action-push (pda-rule-action rule)))
                   (symbol? (pda-action-pop (pda-rule-action rule)))))
            rules))

  ;;(listof pda-rule) -> (listof pda-rule)
  ;;Purpose: Extracts all the pda-rules that ONLY pop off the stack
  (define (get-pop-only-rules rules)
    (filter (λ (rule)
              (and (symbol? (pda-action-push (pda-rule-action rule)))
                   (list? (pda-action-pop (pda-rule-action rule)))))
              
            rules))

  ;;(listof pda-rule) -> (listof pda-rule)
  ;;Purpose: Extracts all the pda-rules both push and pop off the stack
  (define (get-push&pop-rules rules)
    (filter (λ (rule)
              (and (list? (pda-action-push (pda-rule-action rule)))
                   (list? (pda-action-pop (pda-rule-action rule)))))
              
            rules))

  (define (get-read-only-rules rules)
    (filter read-only? rules))

  (define (find-largest accessor rules acc)
    (if (null? rules)
        acc
        (let ([oper-amount (length (accessor (first rules)))])
          (if (> oper-amount acc)
              (find-largest accessor (rest rules) oper-amount)
              (find-largest accessor (rest rules) acc)))))
        
  
  (define (make-inverses-equal push-rules pop-rules)
    (cond [(or (null? push-rules) (null? pop-rules)) '()]
          [(> (length push-rules) (length pop-rules)) '()]))
  
  (define (make-inverse-pairs push-rules pop-rules)
    (for/list ([push (in-list push-rules)]
               [pop (in-list pop-rules)]
               #:when (inverse-stack-operations? (pda-rule-action push) (pda-rule-action pop))
               [pair (list (inverse-pair push pop))])
      pair))
  
  (let* ([new-P (make-new-machine P)]
         [shrunken-P (rip-nodes new-P)]
         #;[cfe-templates (pda-rule-tag (first (pda-rules shrunken-P)))]
         #;[extracted-rules (flatten (template->rules cfe-templates))]
         #;[inverse-pairs (make-inverse-pairs (get-push-only-rules extracted-rules) (get-pop-only-rules extracted-rules))])
    #;(printable-tag cfe-templates)
    (sm-graph (pda->unchecked shrunken-P))
      #;(values #;(sm-graph (pda->unchecked new-P))
              #;(sm-graph (pda->unchecked shrunken-P))
              cfe-templates
              (printable-tag cfe-templates)
              
              #;extracted-rules
              (get-push-only-rules extracted-rules)
              (get-pop-only-rules extracted-rules)
              (get-push&pop-rules extracted-rules)
              (get-read-only-rules extracted-rules))
      #;(values (find-largest (compose1 pda-action-push pda-rule-action) (get-push-only-rules extracted-rules) 0)
              (find-largest (compose1 pda-action-pop pda-rule-action) (get-pop-only-rules extracted-rules) 0))
      #;(list
             (get-push-only-rules extracted-rules)
             (get-pop-only-rules extracted-rules))
      #;(values 
              (sm-graph (pda->unchecked new-P))
              cfe-templates
              (printable-tag cfe-templates)
              (template->rules cfe-templates)
              (sm-graph (pda->unchecked shrunken-P))
              #;(displayln (format "temp:\n~a\nprintable:\n~a\n" cfe-templates (printable-tag cfe-templates)))
              #;(displayln (format "temp:\n~a\nprintable:\n~a\n" simp-template (printable-tag simp-template)))
              #;(sm-graph (pda->unchecked shrunken-P)))))

#;(define #;define/contract (pda->cfe pda)
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
      (if (null? old-nts)
          acc
          (let* ([translated-nt (gen-nt new-nts)]
                 [new-acc (hash-set acc (car old-nts) translated-nt)]
                 [new-nts (cons translated-nt new-nts)])
            (rename-nts-helper (cdr old-nts) new-nts new-acc))))
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
              (and (andmap (λ (rule) (member rules2 rule)) rules1)
                   (andmap (λ (rule) (member rules1 rule)) rules2)))
            (if (and (>= (length acc) 2)
                     (same-rules? (car acc) (cadr acc)))
                (car acc)
                (let* ([new-rules (filter (λ (rule)
                                            (andmap (λ (rhs)
                                                      (or (member sigma rhs)
                                                          (member nts rhs)))
                                                    (cfg-rule-rhs rule)))
                                          rules)]
                       [new-nts (map cfg-rule-lhs new-rules)])
                  (remove-rules-without-nts new-nts new-rules sigma (cons new-rules acc)))))
  
          ;;(listof cfg-rules) (listof cfg-rules) -> (listof cfg-rules)
          ;;Purpose: Removes rules whose LHS nt only produces self-loops 
          (define (remove-only-self-loops rules new-rules)
            (if (null? rules)
                new-rules
                (let* ([rule-lhs (cfg-rule-lhs (car rules))]
                       [related-rules (filter (λ (rule) (eq? (cfg-rule-lhs rule) rule-lhs)) rules)]
                       [only-self-loop? (or (and (= (length related-rules) 1)
                                                 (not (member (cfg-rule-rhs (car related-rules)) rule-lhs)))
                                            (and (> (length related-rules) 1)
                                                 (ormap (λ (rule) (not (member (cfg-rule-rhs rule) rule-lhs))) related-rules)))])
                  (if (not only-self-loop?)
                      (remove-only-self-loops (cdr rules) new-rules)
                      (remove-only-self-loops (cdr rules) (cons (car rules) new-rules))))))
  
          (let* ([rhs-nts (remove-duplicates (append-map cfg-rule-rhs rules))]
                 [rules-that-can-be-generated (filter (λ (rule)
                                                        (or (eq? (cfg-rule-lhs rule) start)
                                                            (member  rhs-nts (cfg-rule-lhs rule))))
                                                      rules)]
                 [lhs-nts (map cfg-rule-lhs rules-that-can-be-generated)])
            (remove-only-self-loops (remove-rules-without-nts lhs-nts rules-that-can-be-generated sigma '()) '())))

        ;;(listof cfg-rule) (listof nt) (listof nt) -> (listof nt)
        ;;Purpose: Extrascts rules who rhs contain needed nts
        (define (extract-needed-nts rules needed seen)
          (let* ([new-needed-nts (filter (λ (nt)
                                           (not (member seen nt)))
                                         (remove-duplicates (filter-map (λ (rule)
                                                                          (and (ormap (λ (rhs) (member needed rhs))
                                                                                      (cfg-rule-rhs rule))
                                                                               (cfg-rule-lhs rule)))
                                                                        rules)))])
            (if (null? new-needed-nts)
                seen 
                (extract-needed-nts rules (append needed new-needed-nts) (append seen new-needed-nts)))))
  
        (let* ([sigma (cons EMP (CFG-sigma G))]
               [needed-nts (extract-needed-nts (remove-useless-rules (CFG-start G) (CFG-nts G) (CFG-rules G) sigma) sigma '())]
               [rules-that-contain-needed-nts (filter (λ (rule)
                                                        (member needed-nts (cfg-rule-lhs rule)))
                                                      (CFG-rules G))])
          (CFG needed-nts
               (CFG-sigma G)
               (filter (λ (rule)
                         (andmap (λ (rhs) (or (member sigma rhs)
                                              (member needed-nts rhs))) (cfg-rule-rhs rule)))
                       rules-that-contain-needed-nts)
               (CFG-start G))))
      (if (and (>= (length acc) 2)
               (equal? (car acc) (cadr acc)))
          (cfg->unchecked (car acc))
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
             (equal? (car acc-rules) (cadr acc-rules)))
        (hash-filter (car acc-rules)
                     (λ (k v)
                       (andmap (λ (r) (not (or (null? r)
                                               (equal? (list EMP) r)))) v)))
        (let* ([only-empty-rules
                (hash-filter curr-rules (λ (k v)
                                          (andmap (λ (r) (or (null? r)
                                                             (equal? (list EMP) r))) v)))]
               [e-nts (hash-keys only-empty-rules)]
               [new-rules (hash-map/copy curr-rules (λ (k v)
                                                      (if (ormap (λ (rule)
                                                                   (ormap (λ (r)
                                                                            (hash-has-key? only-empty-rules r)) rule)) v)
                                                          (values k (map (λ (val) (filter (λ (r) (not (member r e-nts))) val)) v))
                                                          (values k v))))])
          (simplify-rules new-rules (cons new-rules acc-rules)))))
  
  (let* ([G (unchecked->cfg (pda2cfg pda))]
         [renamed-nts-mapping (rename-nts G)]
         [renamed-cfg (unchecked->cfg (rebuild-cfg G renamed-nts-mapping))]
         [proper-cfg  (minimize-cfg renamed-cfg)]
         [new-rules (make-hash-table (cfg-get-v proper-cfg) (λ (nt) (filter-map (λ (rule)
                                                                                  (and (eq? (car rule) nt)
                                                                                       (symbol->fsmlos (caddr rule))))
                                                                                (cfg-get-rules proper-cfg))))]
         [only-empty-rules (hash-filter new-rules (λ (k v)
                                                    (or (null? v)
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
                                                                                         (car (flatten (hash-ref sub-only-rules r)))
                                                                                         r)) val)) v))
                                                      (values k v))))]
         [startt (car (filter (λ (x) (eq? x (cfg-get-start proper-cfg))) (hash-keys simp-rules2)))]
         [final-rules (hash-filter simp-rules2 (λ (k v)
                                                 (or (eq? k startt)
                                                     (not (member k sub-nts)))))]
         [usable-rules (append-map (λ (lhs)
                                     (map (λ (rhs)
                                            (cfg-rule lhs rhs))
                                          (hash-ref final-rules lhs)))
                                   (hash-keys final-rules))])
    final-rules))

;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define (cfe->pda cfe)
  (cfg->pda (cfe->cfg cfe)))