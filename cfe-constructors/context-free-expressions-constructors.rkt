#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/pda.rkt"
         "../fsm-core/private/misc.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
         racket/treelist
         ;racket/hash
         )

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
         printable-cfexp ;;temp
         cfexp ;;temp
         unchecked->cfg ;;temp
         )

(define MAX-KLEENESTAR-LIMIT 50)

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

;; . cfexp -> concat-cfexp/empty-cfexp
;;Purpose: A wrapper to create a concat-cfexp unless all the given cfexps are empty-cfexp
(define/contract (concat-cfexp . cfexps)
  concat-cfexp/c
  (if (all-empty? cfexps)
        (empty-cfexp)
        (mk-concat-cfexp (list->vector cfexps))))

;; . cfexp -> union-cfexp/empty-cfexp
;;Purpose: A wrapper to create a union-cfexp unless all the given cfexps are empty-cfexp
(define/contract (union-cfexp . cfexps)
  union-cfexp/c
  (if (all-empty? cfexps)
        (empty-cfexp)
        (mk-union-cfexp (list->vector cfexps))))

;;cfexp -> Kleene-cfexp/empty-cfexpmessage
;;Purpose: A wrapper to create a Kleene-cfexp
(define/contract (kleene-cfexp cfe)
  kleene-cfexp/c
  (if (mk-empty-cfexp? cfe)
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
(define (convert-singleton singleton-cfexp)
 (symbol->string (mk-singleton-cfexp-char singleton-cfexp)) #;(vector (mk-singleton-cfexp-char singleton-cfexp)))


(define (contains-empty? Vocfe)
  (for/or ([cfe (in-vector Vocfe)])
    (or (mk-empty-cfexp? cfe)
        (and (mk-var-cfexp? cfe)
             (mk-var-cfexp-only-empty? cfe)))))

;; Vocfexp --> cfexp
;; Purpose: Return a randomly chosen sub-cfexp from the given union-cfexp weigthed towards a non-empty-cfexp
(define (pick-cfexp cfexps)
  (if (contains-empty? cfexps)
        (let ([filtered-empties (vector-filter-not mk-empty-cfexp? cfexps)])
          (if (or (vector-empty? filtered-empties)
                  (< (random) 0.1))
            (empty-cfexp)
            (vector-ref filtered-empties (random (vector-length filtered-empties)))))
        (vector-ref cfexps (random (vector-length cfexps)))))

;;var-cfexp --> word
;;Purpose: Substitutes the given var-cfexp with it's environment bindings 
(define (substitute-var var-cfexp reps)
  (let* ([bindings (hash-ref (mk-var-cfexp-env var-cfexp) (mk-var-cfexp-cfe var-cfexp))])
    (gen-cfexp-word-helper (pick-cfexp bindings) #;(vector-ref bindings (random (vector-length bindings))) reps)))

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
    (if (empty? lst-words) EMP lst-words)))

(define (string-empty? str)
  (not (non-empty-string? str)))

(define (string->word str)
  (define (string->word-helper idx end-idx acc)
    (if (= idx end-idx)
        (reverse acc)
        (string->word-helper (add1 idx) end-idx (cons (string->symbol (substring str idx (add1 idx))) acc))))
  (string->word-helper 0 (string-length str) '()))

;; cfe [natnum] -> word
;; Purpose: Generates a word using 
(define/contract (gen-cfexp-word cfe . reps)
  gen-cfexp-word/c
  (define MAX-KLEENESTAR-REPS (if (empty? reps) MAX-KLEENESTAR-LIMIT (first reps)))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (list (mk-singleton-cfexp-char cfe))]
        [else (let ([res (gen-cfexp-word-helper cfe MAX-KLEENESTAR-REPS)])
                (if (string-empty? res)
                    EMP
                    (string->word res)))]))


(define (gen-cfexp-word-helper cfe reps)
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) ""]
        [(mk-singleton-cfexp? cfe) (convert-singleton cfe)]
        [(mk-var-cfexp? cfe) (substitute-var cfe reps)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word-helper reps)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word-helper (pick-cfexp (mk-union-cfexp-locfe cfe)) reps)]
        [else (gen-cfe-kleene-word cfe reps gen-cfexp-word-helper)]))


(struct CFG (nts sigma rules start) #:transparent)

(define (unchecked->cfg G)
  (CFG (cfg-get-v G) (cfg-get-alphabet G) (cfg-get-the-rules G) (cfg-get-start G)))

(define (cfg->unchecked G)
  (cfg (CFG-nts G) (CFG-sigma G) (CFG-rules G) (CFG-start G)))

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
  
  ;;(hash nts . (listof symbol)) (hash symbol . singleton-cfe)) (hash nts . variable-cfe)) -> (hash nts . cfe))
  ;;Purpose: Converts the RHS of cfg rules into cfes
  (define (make-cfexps-frm-rules rules singletons variables)
    ;;symbol -> cfe
    ;;Purpose: Matches the given symbol with the corresponding cfe
    (define (convert-to-expression RHS-of-rule)
      (cond [(eq? RHS-of-rule EMP) (empty-cfexp)]
            [(hash-has-key? singletons RHS-of-rule) (hash-ref singletons RHS-of-rule)]
            [(hash-has-key? variables RHS-of-rule)
             (let* ([future-var-binding (hash-ref rules RHS-of-rule)]
                    [only-empty? (and (= (length future-var-binding) 1)
                                      (equal? (first future-var-binding) (list EMP)))])
               (if only-empty?
                   (empty-cfexp)
                   (hash-ref variables RHS-of-rule)))]
            [else (error (format "unreadable RHS: ~a" RHS-of-rule))]))
    ;;(listof symbol) -> cfe
    ;;Purpose: Translates the given (listof symbol) into its corresponding cfe
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
         [singletons (make-hash-table (cfg-get-alphabet G) singleton-cfexp)]
         [variables (make-hash-table nts var-cfexp)]
         [rules->cfexp (make-cfexps-frm-rules rules singletons variables)]
         [updated-bindings (hash-map/copy rules->cfexp (λ (key value)
                                                         (begin (update-binding! (hash-ref variables key) key value)
                                                                (values key (hash-ref variables key)))))])
     (hash-ref updated-bindings start)))
      
;;cfe -> string
;;Purpose: Converts the given cfe into a string to make it readable
(define (printable-cfexp cfe)
  ;;(listof cfe) string -> string
  ;;Purpose: Converts and appends all of the cfes in the given (listof cfe) 
  (define (printable-helper locfe connector)
    (if (= (length locfe) 1)
        (printable-cfexp (first locfe))
        (string-append (printable-cfexp (first locfe))
                       connector
                       (printable-helper (rest locfe) connector))))
  (define NULL-REGEXP-STRING "∅")
  (define EMPTY-REGEXP-STRING (symbol->string EMP))
  (cond [(mk-null-cfexp? cfe) NULL-REGEXP-STRING]
        [(mk-empty-cfexp? cfe) EMPTY-REGEXP-STRING]
        [(mk-singleton-cfexp? cfe) (symbol->string (mk-singleton-cfexp-char cfe))]
        [(mk-var-cfexp? cfe) (symbol->string (mk-var-cfexp-cfe cfe))]
        [(mk-concat-cfexp? cfe) (string-append "(" (printable-helper (mk-concat-cfexp-locfe cfe) "") ")")]
        [(mk-union-cfexp? cfe) (string-append "(" (printable-helper (mk-union-cfexp-locfe cfe) " ∪ ") ")")]
        [else (gen-cfe-kleene-word cfe gen-cfexp-word)]))
  

;;cfe -> cfg
;;Purpose: Converts the given cfe into its corresponding cfg
(define/contract (cfe->cfg cfe)
  cfe->cfg/c
  ;;vars    | the accumulated variables found from traversing the given cfe  | (listof var-cfexp)
  ;;singles | the accumulated singletons found from traversing the given cfe | (listof singleton-cfexp)
  (struct extraction-results (vars singles) #:transparent)

  (define e-queue '())

  (define qfirst first)

  (define qempty? empty?)

  ;;(queueof X) -> (queueof X)
  ;;Purpose: Removes the first element from the queue
  (define (dequeue qox)
    (rest qox))

  ;;(queueof X) X -> (queueof X)
  ;;Purpose: Adds the given X to the back of the queue
  (define (enqueue qox x)
    (let ([x (if (list? x) x (list x))])
      (append qox x)))

  ;;cfe -> extraction-results
  ;;Purpose: Extracts all var-cfexp and singleton-cfexp from the given cfe
  (define (extract-var-and-singles-cfe cfe)
    (let ([init-queue (foldl (λ (env acc)
                               (enqueue acc env))
                             e-queue
                             (map vector->list (hash-values (mk-var-cfexp-env #;cfexp-env cfe))))])
      (extract-var-and-singles init-queue
                               (update-extraction-results cfe (extraction-results '() '()))
                               (list cfe))))

  ;;cfe extraction-results -> extraction-results
  ;;Purpose: Updates the given extraction-results to add the given cfe if it is a singleton or variable
  (define (update-extraction-results cfe extract-res)
    (cond [(mk-var-cfexp? cfe) (struct-copy extraction-results
                                            extract-res
                                            [vars (cons cfe (extraction-results-vars extract-res))])]
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
                                   (filter (λ (cfe) (not (member cfe visited))) cfes-to-add))]
               [new-acc (update-extraction-results cfe extract-res)]
               [new-visited (cons cfe visited)])
          (extract-var-and-singles new-queue new-acc new-visited))))

  ;;cfe -> (listof cfe)
  ;;Purpose: Extracts the sub-expressions from the given cfe
  (define (extract-cfe-data cfe)
    (cond [(mk-concat-cfexp? cfe) (vector->list (mk-concat-cfexp-locfe cfe))]
          [(mk-union-cfexp? cfe) (vector->list (mk-union-cfexp-locfe cfe))]
          [(mk-kleene-cfexp? cfe) (list (mk-kleene-cfexp-cfe cfe))]
          [(mk-var-cfexp? cfe) (foldl (λ (env acc)
                                        (enqueue acc env))
                                      e-queue
                                      (map vector->list hash-values (mk-var-cfexp-env #;cfexp-env cfe)))]
          [else '()]))

  ;;(listof var-cfexp) (listof rule) -> (listof rule)
  ;;Purpose: Converts every var-cfexp into the corresponding grammar rule
  (define (variables->rules lovcfe results)
    (foldl (λ (vcfe res)
             (append (remake-rules (mk-var-cfexp-cfe vcfe) (vector->list (first (hash-values (mk-var-cfexp-env #;cfexp-env vcfe)))) '()) res))
           '()
           lovcfe))

  ;;nonterminal (queueof cfe) (listof rule) -> (listof rule)
  ;;Purpose: Converts each cf in the (queueof cfe) into the proper grammar rules
  (define (remake-rules nt rules-to-convert finished-rules)
    (if (qempty? rules-to-convert)
        finished-rules
        (let ([cfe (qfirst rules-to-convert)])
          (if (mk-union-cfexp? cfe)
              (remake-rules nt (enqueue (dequeue rules-to-convert) (vector->list (mk-union-cfexp-locfe cfe))) finished-rules)
              (remake-rules nt (dequeue rules-to-convert) (cons (cfe->rule nt cfe) finished-rules))))))

  ;;non-terminal cfe -> rule
  ;;Purpose: Converts the cfe into a grammar rule using the given non-terminal
  (define (cfe->rule nt cfe)
    ;;if union found in concat split union and make concat using every branch
    (let ([RHS (cond [(mk-empty-cfexp? cfe) EMP]
                     [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
                     [(mk-var-cfexp? cfe) (mk-var-cfexp-cfe cfe)]
                     [(mk-concat-cfexp? cfe) (string->symbol (foldr (λ (cfe acc)
                                                                      (string-append
                                                                       (symbol->string
                                                                        (if (mk-singleton-cfexp? cfe)
                                                                            (mk-singleton-cfexp-char cfe)
                                                                            (mk-var-cfexp-cfe cfe)))
                                                                       acc))
                                                                    ""
                                                                    (vector->list (mk-concat-cfexp-locfe cfe))))]
                     [else (error (format "unsuitable cfe ~a" cfe))])])
      (list nt ARROW RHS)))
  (let* ([extracted-components (extract-var-and-singles-cfe cfe)]
         [variables (extraction-results-vars extracted-components)]
         [singletons (extraction-results-singles extracted-components)]
         [alphabet (remove-duplicates (map mk-singleton-cfexp-char singletons))]
         [rules (variables->rules variables '())]
         [nts (remove-duplicates (map mk-var-cfexp-cfe variables))]
         [starting-nt (cond [(mk-var-cfexp? cfe) (mk-var-cfexp-cfe cfe)]
                         [(= (length nts) 1) (first nts)]
                         [else (gen-nt extracted-components)])])
    (make-unchecked-cfg nts alphabet rules starting-nt)))

;;(listof nt) (listof nt) (hash nt . nt)
;;Purpose: Renames the given (listof nt) and pairs the each nt with a new name in a hash
(define (rename-nts-helper old-nts new-nts acc)
  (if (empty? old-nts)
      acc
      (let* ([translated-nt (gen-nt new-nts)]
             [new-acc (hash-set acc (first old-nts) translated-nt)]
             [new-nts (cons translated-nt new-nts)])
        (rename-nts-helper (rest old-nts) new-nts new-acc))))


;;CFG -> (hash nt . nt)
;;Purpose: Creates a hash table with all the nts and new respective name  
(define (rename-nts G)
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

;;nts | the nts considered to be needed by the cfg | (listof nt)
;;rules | the rules considered to be needed by the cfg | (listof cfg-rule)
(struct minimize-res (nts rules) #:transparent)

;;X (listof Y) -> boolean
;;Purpose: Determines if the given X is in the (listof Y)
(define (member? X lst)
  (ormap (λ (y) (eq? X y)) lst))

;;cfg -> cfg
;;Purpose: minimizes the given cfg by removing nts and rules that aren't needed
(define (minimize-cfg G)
  (let ([minimized-nts+rules (minimize-nts-and-rules (CFG-nts G) (CFG-rules G) (cons EMP (CFG-sigma G)) '())])
    (cfg (minimize-res-nts minimized-nts+rules) (CFG-sigma G) (minimize-res-rules minimized-nts+rules) (CFG-start G))))


;;minimize-res minimize-res -> boolean
;;Purpose: Determines if the two given minimize-res are the same 
(define (same-rules-and-nts? res1 res2)
  (let ([nts1 (CFG-nts #;minimize-res-nts res1)]
        [rules1 (CFG-rules #;minimize-res-rules res1)]
        [nts2 (CFG-nts #;minimize-res-nts res2)]
        [rules2 (CFG-rules #;minimize-res-rules res2)])
    (and (andmap (λ (nt) (member? nt nts2)) nts1)
         (andmap (λ (nt) (member? nt nts1)) nts2)
         (andmap (λ (rule) (member? rule rules2)) rules1)
         (andmap (λ (rule) (member? rule rules1)) rules2))))

;;(listof nt) (listof cfg-rule) (listof alphabet) (listof minimize-res) -> minimize-res
;;Purpose: Minimizes the given nts and the given rules 
(define (minimize-nts-and-rules nts rules sigma acc)
  (if (and (>= (length acc) 2)
           (same-rules-and-nts? (first acc) (second acc)))
      (first acc)
      (let* ([new-nts (filter (λ (nt) (and (member? nt (map cfg-rule-lhs rules))
                                           (ormap (λ (rule)
                                                    (andmap
                                                     (λ (rhs)
                                                       (or (eq? nt rhs)
                                                           (member? rhs sigma)))
                                                     (cfg-rule-rhs rule)))
                                                  rules)))
                              nts)]
             [new-rules (filter (λ (rule)
                                  (and (ormap (λ (nt) (eq? (cfg-rule-lhs rule) nt)) new-nts)
                                       (andmap (λ (rhs) (or (member? rhs new-nts)
                                                            (member? rhs sigma)))
                                               (cfg-rule-rhs rule))))
                                rules)]
             [res (minimize-res new-nts new-rules)])
        (minimize-nts-and-rules new-nts new-rules sigma (cons res acc)))))


(define (clean-up-cfg cfg acc)
  #;(let* ([nts (CFG-nts cfg)]
         [rules (CFG-rules cfg)]
         [all-nts (remove-duplicates (filter-not (λ (nt) (member? nt (cons EMP (CFG-sigma cfg))))
                                                 (append nts
                                                         (append-map (λ (rl) (cons (cfg-rule-lhs rl) (cfg-rule-rhs rl))) rules))))]
         [nt-hash (foldl (λ (nt ht)
                           (hash-set ht nt (filter (λ (rl) (eq? nt (cfg-rule-lhs rl))) rules)))
                         (hash)
                         all-nts)]
         [needed-nts (filter-not (λ (nt) (empty? (hash-ref nt-hash nt)))
                                 (remove-duplicates (filter-not (λ (nt) (member? nt (cons EMP (CFG-sigma cfg))))
                                                    (apply append
                                                           (hash-map nt-hash (λ (key value)
                                                                               (append-map (λ (rl)
                                                                                             (cons (cfg-rule-lhs rl) (cfg-rule-rhs rl))) value)))))))]
         [need-rules (filter (λ (rule) (and (member? (cfg-rule-lhs rule) needed-nts)
                                            (andmap (λ (rhs)
                                                      (or (member? rhs (cons EMP (CFG-sigma cfg)))
                                                          (member? rhs needed-nts))) (cfg-rule-rhs rule))))
                               rules)]
         [new-cfg (CFG needed-nts (CFG-sigma cfg) need-rules (CFG-start cfg))])
    ;(values (length nts) (length all-nts))
    ;(filter-not (λ (nt) (member? nt all-nts)) nts)
    (values rules nt-hash needed-nts (length needed-nts) need-rules)
    #;(clean-up-cfg new-cfg (cons new-cfg acc)))
  (if (and (>= (length acc) 2)
           (same-rules-and-nts? (first acc) (second acc)))
      (cfg->unchecked (first acc))
  (let* ([nts (CFG-nts cfg)]
         [rules (CFG-rules cfg)]
         [all-nts (remove-duplicates (filter-not (λ (nt) (member? nt (cons EMP (CFG-sigma cfg))))
                                                 (append nts
                                                         (append-map (λ (rl) (cons (cfg-rule-lhs rl) (cfg-rule-rhs rl))) rules))))]
         [nt-hash (foldl (λ (nt ht)
                           (hash-set ht nt (filter (λ (rl) (eq? nt (cfg-rule-lhs rl))) rules)))
                         (hash)
                         all-nts)]
         [needed-nts (filter-not (λ (nt) (empty? (hash-ref nt-hash nt)))
                                 (remove-duplicates (filter-not (λ (nt) (member? nt (cons EMP (CFG-sigma cfg))))
                                                    (apply append
                                                           (hash-map nt-hash (λ (key value)
                                                                               (append-map (λ (rl)
                                                                                             (cons (cfg-rule-lhs rl) (cfg-rule-rhs rl))) value)))))))]
         [need-rules (filter (λ (rule) (and (member? (cfg-rule-lhs rule) needed-nts)
                                            (andmap (λ (rhs)
                                                      (or (member? rhs (cons EMP (CFG-sigma cfg)))
                                                          (member? rhs needed-nts))) (cfg-rule-rhs rule))))
                               rules)]
         [new-cfg (CFG needed-nts (CFG-sigma cfg) need-rules (CFG-start cfg))])
    ;(values (length nts) (length all-nts))
    ;(filter-not (λ (nt) (member? nt all-nts)) nts)
    #;(values rules nt-hash needed-nts (length needed-nts) need-rules)
    (clean-up-cfg new-cfg (cons new-cfg acc)))))


(define (clean-up1 G)
  (let* ([sigma (cons EMP (CFG-sigma G))]
         [needed-nts (clean-up2 (remove-useless-rules (CFG-start G) (CFG-nts G) (CFG-rules G) sigma) sigma '())]
         [rules-that-contain-needed-nts (filter (λ (rule)
                                                  (member? (cfg-rule-lhs rule) needed-nts))
                                                (CFG-rules G))]
         )
    (CFG needed-nts
         (CFG-sigma G)
         (filter (λ (rule)
                   (andmap (λ (rhs) (or (member? rhs sigma)
                                        (member? rhs needed-nts))) (cfg-rule-rhs rule)))
                 rules-that-contain-needed-nts)
         (CFG-start G))))
  


(define (clean-up2 rules needed seen)
  (let* ([new-needed-nts (filter-not (λ (nt)
                                       (member? nt seen))
                                     (remove-duplicates (map cfg-rule-lhs (filter (λ (rule)
                                                                 (ormap (λ (rhs) (member? rhs needed))
                                                                        (cfg-rule-rhs rule)))
                                                               rules))))])
        (if (empty? new-needed-nts)
            #;needed seen #;(values needed seen)
            (clean-up2 rules (append needed new-needed-nts) (append seen new-needed-nts)))))

(define (same-rules? rules1 rules2)
  (and (andmap (λ (rule) (member? rule rules2)) rules1)
       (andmap (λ (rule) (member? rule rules1)) rules2)))

(define (remove-useless-rules start nts rules sigma)
  (let* ([rhs-nts (remove-duplicates (append-map cfg-rule-rhs rules))]
         [rules-that-can-be-generated (filter (λ (rule) (or (eq? (cfg-rule-lhs rule) start)
                          (member? (cfg-rule-lhs rule) rhs-nts))) rules)]
         [lhs-nts (map cfg-rule-lhs rules-that-can-be-generated)]
         )
       (remove-only-self-loops (remove-rules-without-nts lhs-nts rules-that-can-be-generated sigma '()) '())))

(define (remove-rules-without-nts nts rules sigma acc)
  (if (and (>= (length acc) 2)
           (same-rules? (first acc) (second acc)))
      (first acc)
      (let* ([new-rules (filter (λ (rule)
                                  (andmap (λ (rhs)
                                            ;(displayln rhs)
                                            (or (member? rhs sigma)
                                                (member? rhs nts)))
                                          (cfg-rule-rhs rule)))
                                rules)]
             [new-nts (map cfg-rule-lhs new-rules)])
        (remove-rules-without-nts new-nts new-rules sigma (cons new-rules acc)))))

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


(struct rule-path (queue next-nts queued-nts seen) #:transparent)


(define e-queue '())

(define qfirst first)

(define qempty? empty?)

;;(queueof X) -> (queueof X)
;;Purpose: Removes the first element from the queue
(define (dequeue qox)
  (rest qox))

;;(queueof X) X -> (queueof X)
;;Purpose: Adds the given X to the back of the queue
(define (enqueue qox x)
  (let ([x (if (list? x) x (list x))])
    (append qox x)))


(define (clean-up-rules cfg)
  (let* ([rules-from-start (filter (λ (rule) (eq? (cfg-rule-lhs rule) (CFG-start cfg)))
                                   (CFG-rules cfg))]
         [starting-queue (map (λ (rule) (rule-path (list rule) (cfg-rule-rhs rule) '() (set rule))) rules-from-start)])
    (clean-up-rules-bfs (enqueue e-queue starting-queue) '() (CFG-rules cfg) (list->set (cons EMP (CFG-sigma cfg))))))


#;(define (clean-up-rules-bfs queue finished rules sigma)
  (if (qempty? queue)
      finished
      (let* ([next-rule-path (qfirst queue)]
             [next-nts-to-find (rule-path-next-nts next-rule-path)]
             [applicable-rules-from-nts (filter-not empty?
                                                    (map (λ (nt)
                                                           (filter (λ (rule)
                                                                     (eq? nt (cfg-rule-lhs rule)))
                                                                   rules))
                                                         next-nts-to-find))]
             #;[next-rules (if (= (length next-nts-to-find) 1)
                             applicable-rules-from-nts
                             applicable-rules-from-nts)]
             [next-paths-to-search (filter-not
                                    (λ (next-path)
                                      (set-member? (rule-path-seen next-rule-path) (qfirst (rule-path-queue next-path))))
                                    (append-map (λ (rules)
                                                  (map (λ (rule)
                                                         (struct-copy rule-path
                                                                      next-rule-path
                                                                      [queue (cons rule (rule-path-queue next-rule-path))]
                                                                      [next-nts (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                            (cfg-rule-rhs rule))]
                                                                      [seen (set-add (rule-path-seen next-rule-path)
                                                                                     rule
                                                                                     #;(qfirst (rule-path-queue next-rule-path)))]))
                                                       rules))
                                                applicable-rules-from-nts))])
        ;;need cartensian product from rules frm nt
        (if (empty? next-paths-to-search)
            (clean-up-rules-bfs2 (dequeue queue) (cons next-rule-path finished) rules sigma)
            (clean-up-rules-bfs2 (enqueue (dequeue queue) next-paths-to-search) finished rules sigma))
            #;(values next-rule-path applicable-rules next-paths-to-search))))


#;(define (clean-up-rules-bfs2 queue finished rules sigma)
  (if (qempty? queue)
      finished
      (let* ([next-rule-path (qfirst queue)]
             [next-nts-to-find (rule-path-next-nts next-rule-path)]
             [applicable-rules-from-nts (filter-not empty?
                                                    (map (λ (nt)
                                      (filter (λ (rule)
                                                (eq? nt (cfg-rule-lhs rule)))
                                              rules))
                                    next-nts-to-find))]
             [next-rules (if (= (length next-nts-to-find) 1)
                             applicable-rules-from-nts
                               (if (= (length next-nts-to-find) 1)
                                   '()
                                   applicable-rules-from-nts))]
             [next-paths-to-search (filter-not
                                    (λ (next-path)
                                      (set-member? (rule-path-seen next-rule-path) (qfirst (rule-path-queue next-path))))
                                    (append-map (λ (rules)
                                                  (map (λ (rule)
                                                         (struct-copy rule-path
                                                                      next-rule-path
                                                                      [queue (cons rule (rule-path-queue next-rule-path))]
                                                                      [next-nts (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                            (cfg-rule-rhs rule))]
                                                                      [seen (set-add (rule-path-seen next-rule-path)
                                                                                     rule
                                                                                     #;(qfirst (rule-path-queue next-rule-path)))]))
                                                       rules))
                                                applicable-rules-from-nts))])
        ;;need cartensian product from rules frm nt
        (if (empty? next-paths-to-search)
            (clean-up-rules-bfs3 (dequeue queue) (cons next-rule-path finished) rules sigma)
            (clean-up-rules-bfs3 (enqueue (dequeue queue) next-paths-to-search) finished rules sigma))
            #;(values next-rule-path applicable-rules-from-nts next-paths-to-search))))

(define (clean-up-rules-bfs queue finished rules sigma)
  (if (qempty? queue)
      finished
      (let* ([next-rule-path (qfirst queue)]
             [next-nts-to-find (flatten (rule-path-next-nts next-rule-path))]
             [applicable-rules-from-nts (map (λ (nt)
                                               (filter (λ (rule)
                                                         (eq? nt (cfg-rule-lhs rule)))
                                                       rules))
                                             next-nts-to-find)
                                        #;(filter-not empty?
                                                    (map (λ (nt)
                                                           (filter (λ (rule)
                                                                     (eq? nt (cfg-rule-lhs rule)))
                                                                   rules))
                                                         next-nts-to-find))]
             [next-rules (cond [(empty? applicable-rules-from-nts) applicable-rules-from-nts]
                               [(= (length next-nts-to-find) 1) applicable-rules-from-nts]
                               [(= (length next-nts-to-find) 2)
                                (let* ([paired-rules (list (first applicable-rules-from-nts)
                                                          (second applicable-rules-from-nts))]
                                       [pair-contains-empty? (or (empty? (first paired-rules))
                                                                 (empty? (second paired-rules)))])
                                  (if pair-contains-empty?
                                   '()
                                   (cartesian-product (first paired-rules) (second paired-rules))))]
                               [(= (length next-nts-to-find) 3)
                                (let* ([is-first-nts-a-pair? (= (length (first (rule-path-next-nts next-rule-path))) 2)]
                                       [paired-rules (if is-first-nts-a-pair?
                                                         (list (first applicable-rules-from-nts)
                                                               (second applicable-rules-from-nts))
                                                         (list (second applicable-rules-from-nts)
                                                               (third applicable-rules-from-nts)))]
                                       [singleton-rule (if is-first-nts-a-pair?
                                                           (third applicable-rules-from-nts)
                                                           (first applicable-rules-from-nts))]
                                       [pair-contains-empty? (or (empty? (first paired-rules))
                                                                 (empty? (second paired-rules)))]
                                       )
                                  (if pair-contains-empty?
                                   '()
                                   (cartesian-product singleton-rule
                                                      (cartesian-product (first paired-rules) (second paired-rules)))))]
                               [else (let* ([paired-rules1 (list (first applicable-rules-from-nts)
                                                                 (second applicable-rules-from-nts))]
                                            [paired-rules2 (list (third applicable-rules-from-nts)
                                                                 (fourth applicable-rules-from-nts))]
                                            [pairs-contains-empty? (or (or (empty? (first paired-rules1))
                                                                           (empty? (second paired-rules1)))
                                                                       (or (empty? (first paired-rules2))
                                                                           (empty? (second paired-rules2))))])
                                       (if pairs-contains-empty?
                                           '()
                                           (cartesian-product (cartesian-product (first paired-rules1) (second paired-rules1))
                                                              (cartesian-product (first paired-rules2) (second paired-rules2)))))])]
             [next-paths-to-search (filter-not
                                    (λ (next-path)
                                      (set-member? (rule-path-seen next-rule-path) (qfirst (rule-path-queue next-path))))
                                    (append-map (λ (rules)
                                                  
                                                  (map (λ (rule)
                                                         (let ([future-nts (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                       (if (= (length next-nts-to-find) 1)
                                                                                           (cfg-rule-rhs rule)
                                                                                           (append (cfg-rule-rhs (first rules))
                                                                                                   (cfg-rule-rhs (second rules)))))])
                                                           (struct-copy rule-path
                                                                        next-rule-path
                                                                        [queue (cons rule (rule-path-queue next-rule-path))]
                                                                        [next-nts (cond [(and (<= (length future-nts) 2)
                                                                                              (empty? (rule-path-queued-nts next-rule-path)))
                                                                                         future-nts]
                                                                                        [(empty? (rule-path-queued-nts next-rule-path)) (take future-nts 2)]
                                                                                        [else (if (>= (length (rule-path-queued-nts next-rule-path)) 2)
                                                                                                  (list (first (rule-path-queued-nts next-rule-path))
                                                                                                        (second (rule-path-queued-nts next-rule-path)))
                                                                                                  (list (first (rule-path-queued-nts next-rule-path))))])]
                                                                        [queued-nts (cond [(and (<= (length future-nts) 2)
                                                                                                (empty? (rule-path-queued-nts next-rule-path)))
                                                                                           (rule-path-queued-nts next-rule-path)]
                                                                                          [(empty? (rule-path-queued-nts next-rule-path))
                                                                                           (drop future-nts 2)]
                                                                                          [else (if (empty? future-nts)
                                                                                                    (if (>= (length (rule-path-queued-nts next-rule-path)) 2)
                                                                                                        (dequeue (dequeue (rule-path-queued-nts next-rule-path)))
                                                                                                        (dequeue (rule-path-queued-nts next-rule-path)))
                                                                                                    (enqueue (if (>= (length (rule-path-queued-nts next-rule-path)) 2)
                                                                                                             (dequeue (dequeue (rule-path-queued-nts next-rule-path)))
                                                                                                             (dequeue (rule-path-queued-nts next-rule-path)))
                                                                                                         (drop future-nts 2)))])]
                                                                        [seen (foldl (λ (rule st)
                                                                                       (set-add st rule))
                                                                                     (rule-path-seen next-rule-path)
                                                                                     rules
                                                                                     #;(qfirst (rule-path-queue next-rule-path)))])))
                                                         rules))
                                                  next-rules))])
        ;;need cartensian product from rules frm nt
        (if (empty? next-paths-to-search)
            (clean-up-rules-bfs (dequeue queue) (cons next-rule-path finished) rules sigma)
            (clean-up-rules-bfs (enqueue (dequeue queue) next-paths-to-search) finished rules sigma))
            #;(values next-rule-path applicable-rules-from-nts next-rules next-paths-to-search))))

(struct nt-path (last-rule-used next-nts seen) #:transparent)
    
(define (gather-nts cfg)
  (let* ([rules-from-start (filter (λ (rule) (eq? (cfg-rule-lhs rule) (CFG-start cfg)))
                                   (CFG-rules cfg))]
         [starting-queue (map (λ (rule)
                                (nt-path rule (cfg-rule-rhs rule) (list->set (cons (cfg-rule-lhs rule) (cfg-rule-rhs rule)))))
                                rules-from-start)]
         [sigma (list->set (cons EMP (CFG-sigma cfg)))]
         [needed-nts (nt-path-seen (gather-nts-bfs (enqueue e-queue starting-queue)
                                                   '()
                                                   (CFG-rules cfg)
                                                   sigma))]
         [rules (filter (λ (rule)
                          (and (set-member? needed-nts (cfg-rule-lhs rule))
                               (andmap (λ (rhs)
                                         (or (set-member? sigma rhs)
                                             (set-member? needed-nts rhs)))
                                          (cfg-rule-rhs rule))))
                        (CFG-rules cfg))]
         )
    #;(gather-nts-bfs (enqueue e-queue starting-queue) '() (CFG-rules cfg) (list->set (cons EMP (CFG-sigma cfg))))
    (values needed-nts rules)))


(define (gather-nts-bfs queue finished rules sigma)
  (if (qempty? queue)
      finished
      (let* ([next-rule-path (qfirst queue)]
             [next-nts-to-find (nt-path-next-nts next-rule-path)]
             [applicable-rules-from-nts (append-map (λ (nt)
                                                      (append-map cfg-rule-rhs (filter (λ (rule)
                                                                                    (eq? nt (cfg-rule-lhs rule)))
                                                                                  rules)))
                                                    next-nts-to-find)]
             #;[next-rules (if (= (length next-nts-to-find) 1)
                             applicable-rules-from-nts
                             applicable-rules-from-nts)]
             [next-nts-to-search (filter-not
                                   (λ (nt)
                                    (set-member? (nt-path-seen next-rule-path) nt))
                                   (filter-not (λ (nt) (set-member? sigma nt)) applicable-rules-from-nts))]
             [next-path (struct-copy nt-path
                                     next-rule-path
                                     [next-nts next-nts-to-search]
                                     [seen (foldl (λ (nt st)
                                                    (set-add st nt))
                                                    (nt-path-seen next-rule-path)
                                                    next-nts-to-search)])]
             #;[next-paths-to-search (filter-not
                                    (λ (next-path)
                                      (set-member? (nt-path-seen next-rule-path) (nt-path-last-rule-used next-path)))
                                    (map #;append-map (λ (rule)
                                                  (struct-copy nt-path
                                                               next-rule-path
                                                               [last-rule-used rule]
                                                               [next-nts (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                     rule)]
                                                               [seen (set-add (nt-path-seen next-rule-path)
                                                                              rule)])
                                                  #;(map (λ (rule)
                                                         (displayln (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                            (cfg-rule-rhs rule)))
                                                         (struct-copy nt-path
                                                                      next-rule-path
                                                                      [last-rule-used rule]
                                                                      [next-nts (filter-not (λ (rhs) (set-member? sigma rhs))
                                                                                            (cfg-rule-rhs rule))]
                                                                      [seen (set-add (nt-path-seen next-rule-path)
                                                                                     rule)]))
                                                       rules))
                                                applicable-rules-from-nts))])
        ;;need cartensian product from rules frm nt
        ;#;
        
        ;(displayln applicable-rules-from-nts)
        
        (if (empty? next-nts-to-search #;next-paths-to-search)
            (gather-nts-bfs (dequeue queue) next-path rules sigma)
            (gather-nts-bfs (enqueue (dequeue queue) next-path #;next-paths-to-search) finished rules sigma))
           #;
        (values next-rule-path applicable-rules-from-nts next-nts-to-search next-path #;next-paths-to-search))))
  
  
            

;; pda -> cfe
;;Purpose: Converts the given pda into a cfe
(define #;define/contract (pda->cfe pda)
  ;pda->cfe/c
  (let* ([G (unchecked->cfg (pda2cfg pda))]
         [renamed-nts-mapping (rename-nts G)]
         [renamed-cfg (unchecked->cfg (rebuild-cfg G renamed-nts-mapping))]
         #;[proper-cfg  (clean-up-cfg renamed-cfg '()) #;(minimize-cfg renamed-cfg)])
   renamed-cfg #;(cfg->cfe proper-cfg)))

;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define/contract (cfe->pda cfe)
  cfe->pda/c
  (cfg->pda (cfe->cfg cfe)))


(define (pda->cfg m)

  (define (cfgrules-pda->cfg sm S Z)
    ; (listof pdarule) (listof state) --> (listof pdarule)
    ; ASSUMPTION: The rules push < 2 elems
    (define (gen-type2-rules rls K Gamma)
        
      ; pdarule -> (listof cfgrule)
      (define (mk-type2-rules r)
          
        ; state --> (listof cfgrule)
        (define (t2-maker s) 
          (map (lambda (g) (cfg-rule (let* ((new-nt (los->symbol 
                                                     (list (pdarule-fromstate r) 
                                                           (if (eq? (pdarule-pop r) EMP) 
                                                               EMP 
                                                               (car (pdarule-pop r))) 
                                                           s))))
                                       new-nt)
                                     (if (eq? (pdarule-readsymb r) EMP)
                                         (list (los->symbol (list (pdarule-tostate r) g s)))
                                         (list (pdarule-readsymb r) (los->symbol (list (pdarule-tostate r) g s))))))
               Gamma))
          
        (append-map (lambda (s) (t2-maker s)) K))
        
      (append-map (lambda (r) (mk-type2-rules r)) rls))


    ; (listof pdarule) (listof state) --> (listof cfg-rule)
    ; ASSUMPTION: The rules push 2 elems
    (define (gen-type3-rules rls states)
        
      ; pdarule --> (listof cfg-rule)
      (define (t3-maker r)
          
        ; state --> (listof cfg-rule)
        (define (gen-t3 p)
          (map (lambda (p1)
                 (cfg-rule (los->symbol (list (pdarule-fromstate r) (if (eq? (pdarule-pop r) EMP) EMP (car (pdarule-pop r))) p))
                           (if (eq? (pdarule-readsymb r) EMP)
                               (list (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                     (los->symbol (list p1 (cadr (pdarule-push r)) p)))
                               (list (pdarule-readsymb r)
                                     (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                     (los->symbol (list p1 (cadr (pdarule-push r)) p))))))
               states))
          
        (append-map (lambda (p) (gen-t3 p)) states))
        
      (append-map (lambda (r) (t3-maker r)) rls))

    ; (listof state) --> (listof cfg-rule)
    (define (gen-type4-rules states) 
      (map (lambda (s) (cfg-rule (los->symbol (list s EMP s)) (list EMP))) states))

    (define (push-length pushstuff)
      (if (eq? pushstuff EMP) 0 (length pushstuff)))
  
    (let* ((R1 (list (cfg-rule S (list (los->symbol (list (pda-getstart m) Z (car (pda-getfinals sm))))))))
           (R2 (gen-type2-rules (filter (lambda (r) (< (push-length (pdarule-push r)) 2))
                                        (pda-getrules sm))
                                (pda-getstates sm)
                                (cons EMP (pda-getgamma sm))))
           (R3 (gen-type3-rules (filter (lambda (r) (= (push-length (pdarule-push r)) 2))
                                        (pda-getrules sm))
                                (cons EMP (pda-getstates sm))))
           (R4 (gen-type4-rules (pda-getstates sm)))
           )
      (append R1 R2 R3 R4)))
  
    ; (simple)ndpda symbol --> (listof cfg-rule)
  (define (extract-nts-from-rule rule)
  (cons (cfg-rule-lhs rule) (cfg-rule-rhs rule)))
    
    (let* ((sm (pda->spda m))
           (S (let ([states (remove-duplicates (append (pda-getstates m) (pda-getstates sm)))])
                (if (member 'S states)
                    (gen-nt states)
                    'S)))
           (Z (first (pda-getgamma sm)))
           (SIGMA (pda-getalphabet sm))
           (R (cfgrules-pda->cfg sm S Z))
           (V (remove-duplicates (append-map extract-nts-from-rule R)))
           )
       (cfg V SIGMA R S)))


(define (mk-cfg-rl a-lhs a-rhs)
  (list a-lhs ARROW a-rhs))

(define (pda2cfg P)
  (define (keep-rule? r len)
    (and (not (eq? (get-push r) EMP))
         (= (length (get-push r)) len)))

  (define (get-nt ntl tbl)
    (second (assoc ntl tbl)))
  
  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=0 pda rules
  (define (gen-theta=0-prs rls sts)
    (for*/list ([r rls] [s sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s)
                 (list (get-read r) (list (get-to r) EMP s)))))

  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=1 pda rules
  (define (gen-theta=1-prs rls sts)
    (for*/list ([r rls] [s sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s)
                 (list (get-read r) (list (get-to r) (first (get-push r)) s)))))

  ;; (listof pda-rule) (listof state) --> (listof cfg-rl)
  ;; Purpose: Return production rules for the given |theta|=2 pda rules
  (define (gen-theta=2-prs rls sts)
    (for*/list ([r rls] [s1 sts] [s2 sts])
      (mk-cfg-rl (list (get-from r) (first (get-pop r)) s2)
                   (list (get-read r)
                         (list (get-to r) (first (get-push r)) s1)
                         (list s1 (second (get-push r)) s2)))))
  
  ;; (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) --> (listof lnt)
  ;; Purpose: Extract the lnts in the given cfg-rl
  (define (extract-lnts theta=0-prs theta=1-prs theta=2-prs self-prs)
    (remove-duplicates
     (append 
      (append-map (λ (pr) (list (first pr) (second (third pr))))
                  (append theta=0-prs theta=1-prs))
      (append-map (λ (pr) (list (first pr)
                                (second (third pr))
                                (third (third pr))))
                  theta=2-prs)
      (map first self-prs))))

  ;; cfg-rl (listof (list lnt nt)) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) (listof cfg-rl) --> (listof cfg-rule)
  ;; Purpose: Convert all given cfg-rl to a cfg-rule using the given association table
  (define (make-cfg-rules startr st-tbl theta=0-prs theta=1-prs theta=2-prs self-prs)
    (cons (mk-cfg-rl (first startr) (get-nt (third startr) st-tbl))
          (append
           (map (λ (rl) (mk-cfg-rl (get-nt (first rl) st-tbl) (third rl)))
                self-prs)
           (map (λ (rl)
                  (mk-cfg-rl (get-nt (first rl) st-tbl)
                             (if (not (eq? (first (third rl)) EMP))
                                 (los->symbol  (list (first (third rl))
                                                     (get-nt (second (third rl)) st-tbl)))
                                 (get-nt (second (third rl)) st-tbl))))
                (append theta=0-prs theta=1-prs))
           (map (λ (rl) (mk-cfg-rl (get-nt (first rl) st-tbl)
                                   (if (not (eq? (first (third rl)) EMP))
                                       (los->symbol (list
                                                     (first (third rl))
                                                     (get-nt (second (third rl)) st-tbl)
                                                     (get-nt (third (third rl)) st-tbl)))
                                       (los->symbol (list
                                                     (get-nt (second (third rl)) st-tbl)
                                                     (get-nt (third (third rl)) st-tbl))))))
                theta=2-prs))))
  (let* [(p (pda->spda P))
         (pstates (pda-getstates p))
         (psigma (pda-getalphabet p))
         (pgamma (pda-getgamma p))
         (pstart (pda-getstart p))
         (pfinals (pda-getfinals p))
         (prules (pda-getrules p))
         (start (generate-symbol 'S '(S)))
         (bottom (first (filter (λ (s) (not (member s (pda-getgamma P)))) pgamma)))
         (startr (mk-cfg-rl start (list (pda-getstart P) bottom (first pfinals))))
         (pstates-nostart (remove pstart pstates))
         (prules-nostartrls (filter (λ (r) (not (eq? (first (first r)) pstart)))
                                    prules))
         (theta=0-prs (gen-theta=0-prs (filter (λ (r) (eq? (get-push r) EMP))
                                               prules-nostartrls)
                                       pstates-nostart))
         (theta=1-prs (gen-theta=1-prs (filter (λ (r) (keep-rule? r 1)) prules-nostartrls)
                                       pstates-nostart))
         (theta=2-prs (gen-theta=2-prs (filter (λ (r) (keep-rule? r 2)) prules-nostartrls)
                                       pstates-nostart))
         (self-prs (map (λ (s) (mk-cfg-rl (list s EMP s)  EMP)) pstates))
         (st-list (cons (third startr)
                        (extract-lnts theta=0-prs theta=1-prs theta=2-prs self-prs))) 
         (st-tbl (map (λ (lnt) (list lnt (generate-symbol 'G '(G)))) st-list))
         (new-rls (make-cfg-rules startr st-tbl theta=0-prs theta=1-prs theta=2-prs self-prs))]
    (make-unchecked-cfg (cons start (map second st-tbl)) psigma new-rls start)))



; ndpda -> ndpda
  ; convert the given pda into a simple pda
;; state symbol stacke state stacke --> pda-rule
;; Purpose: Build a pda-rule
(define (mk-pda-rule from a pop to push)
  (list (list from a pop) (list to push)))

;; pda-rule --> state
;; Purpose: Extract from state
(define (get-from r) (first (first r)))

;; pda-rule --> symbol
;; Purpose: Extract read symbol
(define (get-read r) (second (first r)))

;; pda-rule --> stacke
;; Purpose: Extract pop elements
(define (get-pop r) (third (first r)))

;; pda-rule --> state
;; Purpose: Extract to state
(define (get-to r) (first (second r)))

;; pda-rule --> stacke
;; Purpose: Extract push elements
(define (get-push r) (second (second r)))

;; (listof pda-rule) (listof state) --> (listof pda-rule)
;; Purpose: Eliminate rules that pop more than two elements
(define (generate-beta<2-rules rules states)
  ;; pda-rule (listof state) --> (listof pda-rule)
  ;; Purpose: Create |beta| = 1 rules for given rule
  (define (convert-beta=1 r states)
    ;; (listof symbol) (listof state) --> (listof pda-rule)
    ;; Purpose: Generate pda rules for given pop list using given states
    (define (gen-intermediate-rules beta sts)
      (if (empty? (rest sts))
          '()
          (cons (mk-pda-rule (first sts) EMP (list (first beta)) (first (rest sts)) EMP)
                (gen-intermediate-rules (rest beta) (rest sts)))))
    (let* [(from (get-from r))
           (read (get-read r))
           (beta (get-pop r))
           (to (get-to r))
           (push (get-push r))
           (new-states (build-list
                        (sub1 (length beta))
                        (λ (i) (generate-symbol 'B (cons 'B states)))))]
      (append (list (mk-pda-rule from EMP (list (first beta)) (first new-states) EMP)
                    (mk-pda-rule (last new-states) read (list (last beta)) to push))
              (gen-intermediate-rules (rest beta) new-states))))
  (let* [(beta>=2-rules (filter (λ (r) (and (not (eq? (get-pop r) EMP))
                                            (>= (length (get-pop r)) 2)))
                                rules))
         (beta<2-rules (filter (λ (r) (not (member r beta>=2-rules)))
                               rules))]
    (append beta<2-rules (append-map
                          (λ (r) (convert-beta=1 r states))
                          beta>=2-rules))))

;; Tests for generate-beta<1-rules




;; (listof pda-rule) (listof symbols) --> (listof pda-rules)
;; Purpose: Substitute pop nothing rules with pop 1 rules
(define (generate-beta=1-rules rls gamma)
  (let* [(beta=0-rls (filter (λ (r) (eq? (get-pop r) EMP)) rls))
         (beta>0-rls (filter (λ (r) (not (member r beta=0-rls))) rls))]
    (append beta>0-rls
            (for*/list ([r beta=0-rls]
                        [g gamma])
              (list (list (get-from r) (get-read r) (list g))
                    (list (get-to r)
                          (if (eq? (get-push r) EMP)
                              (list g)
                              (append (get-push r) (list g)))))))))


;; (listof pda-rule) (listof states) --> (listof pda-rule)
;; Purpose: Substitute rules that push more than 2 elements
(define (generate-theta<=2-rules rls sts)
  ;; (listof pda-rule) (listof state) --> (listof pda-rule)
  ;; Purpose: Generate rules with |theta|<=2 for given rules
  (define (gen-theta<=2-rules theta>2-rules sts)
    ;; pda-rule --> (listof pda-rule)
    ;; Purpose: Generate rules with |theta|<=2 for given rule
    (define (gen-rules r)
      ;; (listof state) (listof symbol) (listof symbol) symbol --> (listof pda-rule)
      ;; Purpose: Generate rules with |theta|<=2 for given push list and state list
      (define (process-sts sts push pop read) 
        (if (= (length sts) 2)
            (list (mk-pda-rule (first sts) read pop (second sts) push))
            (cons (mk-pda-rule (first sts) EMP pop (second sts) (append pop (list (first push))))
                  (process-sts (rest sts) (rest push) pop read))))
      (let* [(from (get-from r))
             (read (get-read r))
             (pop (get-pop r))
             (to (get-to r))
             (push (get-push r))
             (new-states (build-list (sub1 (length push))
                                     (λ (i) (generate-symbol 'T (cons 'T sts)))))
             (rev-push (reverse push))]
        (cons (mk-pda-rule from EMP pop (first new-states) (append pop (list (first rev-push))))
              (process-sts (append new-states (list to)) (rest rev-push) pop read))))
    (append-map gen-rules theta>2-rules))
  (let* [(theta>2-rules (filter
                         (λ (r) (and (not (eq? (second (second r)) EMP))
                                     (> (length (second (second r))) 2)))
                         rls))
         (theta<=2-rules (filter
                          (λ (r) (not (member r theta>2-rules)))
                          rls))]
    (append theta<=2-rules (gen-theta<=2-rules theta>2-rules sts))))


;; (listof pda-rule) --> (listof state)
;; Purpose: Extract the list of states in the given rules
(define (extract-states rls)
  (remove-duplicates
   (append-map (λ (r) (list (first (first r))
                            (first (second r))))
               rls)))

;; pda --> pda
;; Purpose: Convert given pda to a simple pda
(define (pda->spda p)
  (let* [(pstates (pda-getstates p))
         (psigma (pda-getalphabet p))
         (pgamma (pda-getgamma p))
         (pstart (pda-getstart p))
         (pfinals (pda-getfinals p))
         (prules (pda-getrules p))
         (new-start (generate-symbol 'S pstates))
         (bottom (generate-symbol 'Z pgamma))
         (initr (mk-pda-rule new-start EMP EMP pstart (list bottom)))
         (new-final (generate-symbol 'F pstates))
         (frules (map (λ (s) (mk-pda-rule s EMP (list bottom) new-final EMP))
                      pfinals))
         (beta<2-rules (generate-beta<2-rules prules pstates))
         (beta=1-rules (generate-beta=1-rules beta<2-rules (cons bottom pgamma)))
         (theta<=2-rules (generate-theta<=2-rules beta=1-rules
                                                  (extract-states beta=1-rules)))]
    (make-unchecked-ndpda (append (list  new-final new-start)
                                  (remove-duplicates
                                   (cons pstart (extract-states theta<=2-rules))))
                        
                          psigma
                          (cons bottom pgamma)
                          new-start
                          (list new-final)
                          (cons initr (append theta<=2-rules frules)))))

(struct pda (states alpha gamma start finals rules) #:transparent)

(define (unchecked->pda m)
  (pda (pda-getstates m)
       (pda-getalphabet m)
       (pda-getgamma m)
       (pda-getstart m)
       (pda-getfinals m)
       (pda-getrules m)))
#|

(define EMPTY (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

(define C (singleton-cfexp 'c))

(define S1
  (let* ([ANBN (var-cfexp 'S)]
         [ASB (concat-cfexp A ANBN B)])
    (begin
      (update-binding! ANBN 'S (union-cfexp EMPTY ASB))
      ANBN)))

;(unchecked->pda (pda->spda (cfe->pda S1)))

;;(apply-pda (cfe->pda S1) '(a a a a b b b b))

;;(apply-pda (pda->spda (cfe->pda S1)) '(a a a a b b b b))


(define cfe (pda->cfe (cfe->pda S1)))


;(pda->cfe (cfe->pda S1))



(define P (make-unchecked-ndpda '(S)
                                '(a b)
                                '(a)
                                'S
                                '(S)
                                `(((S a ,EMP) (S ,EMP)))))



;(pda->cfe (cfe->pda S1))
;cfe 
;G
;(cfg-derive G '(a b))
;(gen-cfexp-word cfe)
;(fsa-test-equivalence (cfe->pda S1) (pda->spda (cfe->pda S1)))

;(cfg->cfe (minimize-cfg (pda->cfe P)))
(define p-word (gen-cfexp-word (cfg->cfe (minimize-cfg (pda->cfe P)))))

;(fsa-test-equivalence P (cfe->pda (pda->cfe P)))
|#

(define EMPTY (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

(define C (singleton-cfexp 'c))

(define S1
  (let* ([ANBN (var-cfexp 'S)]
         [ASB (concat-cfexp A ANBN B)])
    (begin
      (update-binding! ANBN 'S (union-cfexp EMPTY ASB))
      ANBN)))

(define s1
  (pda->cfe (cfe->pda S1))
  )

(define ANBN-cfg (make-unchecked-cfg '(S)
                                     '(a b)
                                     `((S ,ARROW ,EMP) (S ,ARROW aSb))
                                     'S))


(define P (make-unchecked-ndpda '(S)
                                '(a b)
                                '(a)
                                'S
                                '(S)
                                `(((S a ,EMP) (S ,EMP)))))

(define p-cfe (pda->cfe P))


(cfg-derive (cfg (list 'B 'A  'W 'G 'T 'I-0 'H 'X 'R-1 'O)
                 '(a b)
                 (list
                  (cfg-rule 'A '(B))
                  (cfg-rule 'X '(ε))
                  (cfg-rule 'H '(ε))
                  (cfg-rule 'G '(H))
                  (cfg-rule 'O '(b H))
                  (cfg-rule 'T '(a H))
                  (cfg-rule 'W '(X))
                  (cfg-rule 'I-0 '(T))
                  (cfg-rule 'B '(G W))
                  (cfg-rule 'G '(R-1 O))
                  (cfg-rule 'R-1 '(I-0 G)))
                 'A)
            '(a a b b))


(cfg-derive (cfg
             '(T O H X G I-0 W R-1 B A)
             '(a b)
             (list
              (cfg-rule 'A '(B))
              (cfg-rule 'X '(ε))
              (cfg-rule 'H '(ε))
              (cfg-rule 'G '(H))
              (cfg-rule 'O '(b H))
              (cfg-rule 'T '(a H))
              (cfg-rule 'W '(X))
              (cfg-rule 'I-0 '(T))
              (cfg-rule 'B '(G W))
              (cfg-rule 'G '(R-1 O))
              (cfg-rule 'R-1 '(I-0 G)))
             'A)
            '(a a b b))


(gen-cfexp-word (cfg->cfe (cfg (list 'B 'A 'W 'G 'T 'I-0 'H 'X 'R-1 'O)
                 '(a b)
                 (list
                  (cfg-rule 'A '(B))
                  (cfg-rule 'X '(ε))
                  (cfg-rule 'H '(ε))
                  (cfg-rule 'G '(H))
                  (cfg-rule 'O '(b H))
                  (cfg-rule 'T '(a H))
                  (cfg-rule 'W '(X))
                  (cfg-rule 'I-0 '(T))
                  (cfg-rule 'B '(G W))
                  (cfg-rule 'G '(R-1 O))
                  (cfg-rule 'R-1 '(I-0 G)))
                 'A)))

(gen-cfexp-word (cfg->cfe (cfg
             '(T O H X G I-0 W R-1 B A)
             '(a b)
             (list
              (cfg-rule 'A '(B))
              (cfg-rule 'X '(ε))
              (cfg-rule 'H '(ε))
              (cfg-rule 'G '(H))
              (cfg-rule 'O '(b H))
              (cfg-rule 'T '(a H))
              (cfg-rule 'W '(X))
              (cfg-rule 'I-0 '(T))
              (cfg-rule 'B '(G W))
              (cfg-rule 'G '(R-1 O))
              (cfg-rule 'R-1 '(I-0 G)))
             'A)))
    