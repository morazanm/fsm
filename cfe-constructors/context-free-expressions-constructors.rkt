#lang racket

(require "../fsm-core/private/constants.rkt"
         "../fsm-core/private/cfg.rkt"
         "../fsm-core/private/pda.rkt"
         "../fsm-core/private/misc.rkt"
         "../fsm-core/private/word.rkt"
         "cfexp-contracts.rkt"
         "cfexp-structs.rkt"
         racket/hash)

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
  (mk-null-cfexp (empty-cfexp-env)))

;; -> empty-cfexp
;;Purpose: A wrapper to create a empty-cfexp
(define (empty-cfexp)
  (mk-empty-cfexp (empty-cfexp-env)))

;; symbol -> singleton-cfexp
;;Purpose: A wrapper to create a singleton-cfexp
(define/contract (singleton-cfexp a-char)
  singleton-cfexp/c 
  (mk-singleton-cfexp (empty-cfexp-env) a-char))

;;symbol -> variable-cfexp
;;Purpose: A wrapper to create a variable-cfexp
(define/contract (var-cfexp symbol)
  var-cfexp/c 
  (mk-var-cfexp (empty-cfexp-env) symbol))

;;(listof cfexp) -> env
;;Purpose: Merges all of the environments from the given (listof cfexp) in to one environment
(define (merge-env locfexp)
  (foldl (λ (cfe env)
           (let ([new-cfe-env (hash-map/copy (cfexp-env cfe)
                                             (λ (k v) (if (hash-has-key? env k)
                                                          (values (gen-nt (hash-keys env)) v)
                                                          (values k v))))])
             (hash-union env new-cfe-env)))
         (hash)
         locfexp))

;; . cfexp -> concat-cfexp
;;Purpose: A wrapper to create a concat-cfexp
(define/contract (concat-cfexp . cfexp)
  concat-cfexp/c
  (let ([cfexp (flatten cfexp)])
    (mk-concat-cfexp (merge-env cfexp) cfexp)))

;; . cfexp -> union-cfexp
;;Purpose: A wrapper to create a union-cfexp
(define/contract (union-cfexp . cfexp)
  union-cfexp/c
  (let ([cfexp (flatten cfexp)])
    (mk-union-cfexp (merge-env cfexp) cfexp)))

;;cfexp -> Kleene-cfexp
;;Purpose: A wrapper to create a Kleene-cfexp
(define/contract (kleene-cfexp cfe)
  kleene-cfexp/c
  (mk-kleene-cfexp (cfexp-env cfe) cfe))

;;cfe-id cfe -> env
;;Purpose: Creates an environment where the given cfe-id is the key and cfe is the value
(define (env-cfexp cfe-id binding)
  (let ([binding (if (list? binding) binding (list binding))])
    (hash cfe-id binding)))

;;var-cfexp symbol cfe -> var-cfexp
;;Purpose: Creates a binding where the cfe is bound to the given var-cfexp's environment
(define/contract (update-binding! cfe bindee-id binding)
  update-binding!/c
  (let ([env (cfexp-env cfe)])
    (begin
      (set! env (env-cfexp bindee-id (if (hash-has-key? env bindee-id)
                                         (cons binding (hash-ref env bindee-id))
                                         binding)))
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
(define/contract (gen-cfexp-word cfe . reps)
  gen-cfexp-word/c
  (define MAX-KLEENESTAR-REPS (if (empty? reps) 20 (first reps)))
  (cond [(mk-null-cfexp? cfe) (error "A word cannot be generated using the null-regexp.")]
        [(mk-empty-cfexp? cfe) EMP]
        [(mk-singleton-cfexp? cfe) (convert-singleton cfe)]
        [(mk-var-cfexp? cfe) (substitute-var cfe)]
        [(mk-concat-cfexp? cfe) (gen-concat-word cfe gen-cfexp-word MAX-KLEENESTAR-REPS)]
        [(mk-union-cfexp? cfe) (gen-cfexp-word (pick-cfexp cfe) MAX-KLEENESTAR-REPS)]
        [else (gen-cfe-kleene-word cfe MAX-KLEENESTAR-REPS gen-cfexp-word)]))

(struct CFG (nts sigma rules start) #:transparent)

(define (unchecked->cfg G)
  (CFG (cfg-get-v G) (cfg-get-alphabet G) (cfg-get-the-rules G) (cfg-get-start G)))

;;context-free grammar -> cfe
;;Purpose: Converts the given cfg its equivalent cfe
(define/contract (cfg->cfe G)
 cfg->cfe/c
  ;;(listof X) (X -> Y) -> (hash X . Y)
  (define (make-hash-table lox f)
    (foldl (λ (x h)
             (hash-set h x (f x))
             #;(let ([res (f x)])
               (if (empty? res)
                   h
                   (hash-set h x res))))
           (hash)
           lox))

  ;;string -> (listof symbol)
  ;;Purpose: Seperates every character in the given string and puts them into a list
  (define (explode string nts)
    ;;natnum (listof symbol) -> (listof symbol)
    ;;Purpose: Seperates every character in the given string and puts them into a list
    ;;acc = the character in the string from [idx..(string-length string)]
    (define (explode-helper idx acc)
      (if (= idx 0)
          acc
          (let* ([next-sym (substring string (sub1 idx) idx)]
                 [res (cond [(member (string->symbol string) nts) (list (string->symbol string))]
                            ;;exploiting fact that the RHS of cfg rules converted from simple pda
                            ;;rules will structure terminal followed by nonterminal
                            [(string<=? "0" next-sym "9")
                             (list (string->symbol (substring string 0 1)) (string->symbol (substring string 1)))] 
                            [else (string->symbol next-sym)])]
                 [new-acc (if (list? res) res (cons res acc))]
                 [new-idx (if (list? res) 0 (sub1 idx))])
            ;(displayln string)
            ;(displayln (substring string 0 1))
            ;(displayln (substring string 1))
            (explode-helper new-idx new-acc))))
    (explode-helper (string-length string) '()))

  ;;(hash nts . (listof symbol)) (hash symbol . singleton-cfe)) (hash nts . variable-cfe)) -> (hash nts . cfe))
  ;;Purpose: Converts the RHS of cfg rules into cfes
  (define (make-cfexps-frm-rules rules singletons variables)
    ;;symbol -> cfe
    ;;Purpose: Matches the given symbol with the corresponding cfe
    (define (convert-to-expression RHS-of-rule)
      (cond [(eq? RHS-of-rule EMP) (empty-cfexp)]
            [(hash-has-key? singletons RHS-of-rule) (hash-ref singletons RHS-of-rule)]
            [(hash-has-key? variables RHS-of-rule) (hash-ref variables RHS-of-rule)]
            [else (error (format "unreadable RHS: ~a" RHS-of-rule))]))
    ;;(listof symbol) -> cfe
    ;;Purpose: Translates the given (listof symbol) into its corresponding cfe
    (define (rule->expression RHS-of-rule)
      (if (= (length RHS-of-rule) 1)
          (convert-to-expression (first RHS-of-rule))
          (concat-cfexp (map (λ (sym) (convert-to-expression sym)) RHS-of-rule))))
    (hash-map/copy rules (λ (nts RHS)
                           (values nts (cond [(empty? RHS) (null-cfexp)]
                                             [(= (length RHS) 1) (rule->expression (first RHS))]
                                             [else (union-cfexp (map (λ (rule) (rule->expression rule)) RHS))])))))
  ;; symbol -> boolean
  ;;Purpose: Determines if the given symbol is a valid state or alphabet symbol
  (define (valid-RHS? sym)
    (or (valid-alpha? sym)
        (valid-state? sym)))
  
  (let* ([nts (cfg-get-v G)]
         [rules (make-hash-table nts (λ (nt) (filter #;any/c list? (filter-map (λ (rule) (and (eq? (first rule) nt) 
                                                                                      (let* ([new-RHS (explode (symbol->string (third rule)) nts)]
                                                                                             [res (if (or (equal? new-RHS (list EMP))
                                                                                                          (andmap valid-RHS? new-RHS)) new-RHS void)])
                                                                                        #;(displayln nt)
                                                                                        #;(displayln res)
                                                                                        res
                                                                                        #;new-RHS)))
                                                                       (cfg-get-rules G)))))]
         [start (cfg-get-start G)]
         [singletons (make-hash-table (cfg-get-alphabet G) singleton-cfexp)]
         [variables (make-hash-table nts var-cfexp)]
         [rules->cfexp (make-cfexps-frm-rules rules singletons variables)]
         [updated-bindings (hash-map/copy rules->cfexp (λ (key value)
                                                         (begin
                                                           (update-binding! (hash-ref variables key) key value)
                                                           (values key (hash-ref variables key)))))])
    #;(displayln updated-bindings)
    #;(displayln start)
     (hash-ref updated-bindings start)
    ;rules
    ;nts
    #;(unchecked->cfg G)))
      
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
(define/contract (cfe->cfg cfe #:debug[debug #f])
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
                             (hash-values (cfexp-env cfe)))])
      (extract-var-and-singles init-queue
                               (update-extraction-results cfe (extraction-results '() '()))
                               (list cfe))))

  ;;cfe extraction-results -> extraction-results
  ;;Purpose: Updates the given extraction-results to add the given cfe if it is a singleton or variable
  (define (update-extraction-results cfe er)
    (cond [(mk-var-cfexp? cfe) (struct-copy extraction-results
                                            er
                                            [vars (cons cfe (extraction-results-vars er))])]
          [(mk-singleton-cfexp? cfe) (struct-copy extraction-results
                                                  er
                                                  [singles (cons cfe (extraction-results-singles er))])]
          [else er]))

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
    (cond [(mk-concat-cfexp? cfe) (mk-concat-cfexp-locfe cfe)]
          [(mk-union-cfexp? cfe) (mk-union-cfexp-locfe cfe)]
          [(mk-kleene-cfexp? cfe) (list (mk-kleene-cfexp-cfe cfe))]
          [(mk-var-cfexp? cfe) (foldl (λ (env acc)
                                        (enqueue acc env))
                                      e-queue
                                      (hash-values (cfexp-env cfe)))]
          [else '()]))

  ;;(listof var-cfexp) (listof rule) -> (listof rule)
  ;;Purpose: Converts every var-cfexp into the corresponding grammar rule
  (define (variables->rules lovcfe results)
    (foldl (λ (vcfe res)
             (append (remake-rules (mk-var-cfexp-cfe vcfe) (first (hash-values (cfexp-env vcfe))) '()) res))
           '()
           lovcfe))

  ;;nonterminal (queueof cfe) (listof rule) -> (listof rule)
  ;;Purpose: Converts each cf in the (queueof cfe) into the proper grammar rules
  (define (remake-rules nt rules-to-convert finished-rules)
    (if (empty? rules-to-convert)
        finished-rules
        (let ([cfe (first rules-to-convert)])
          (if (mk-union-cfexp? cfe)
              (remake-rules nt (enqueue (dequeue rules-to-convert) (mk-union-cfexp-locfe cfe)) finished-rules)
              (remake-rules nt (dequeue rules-to-convert) (cons (cfe->rule nt cfe) finished-rules))))))

  ;;non-terminal cfe -> rule
  ;;Purpose: Converts the cfe into a grammar rule using the given non-terminal
  (define (cfe->rule nt cfe)
    ;;if union found in concat split union and make concat using every branch
    (let ([RHS (cond [(mk-empty-cfexp? cfe) EMP]
                     [(mk-singleton-cfexp? cfe) (mk-singleton-cfexp-char cfe)]
                     [(mk-var-cfexp? cfe) (mk-var-cfexp-cfe cfe)]
                     [(mk-concat-cfexp? cfe) (string->symbol (foldr (λ (cfe acc)
                                                                      (string-append (symbol->string (if (mk-singleton-cfexp? cfe)
                                                                                                         (mk-singleton-cfexp-char cfe)
                                                                                                         (mk-var-cfexp-cfe cfe))) acc))
                                                                    ""
                                                                    (mk-concat-cfexp-locfe cfe)))]
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
    (if debug
        (CFG nts alphabet rules starting-nt)
        (make-unchecked-cfg nts alphabet rules starting-nt))))

;; pda -> cfe
;;Purpose: Converts the given pda into a cfe
#;(define/contract (pda->cfe pda)
  pda->cfe/c
 #;(pda->cfg pda) ;<-need to fix
  ;;rename nts before going into cfg-cfe, use hash to keep track of nts and sub 
  (cfg->cfe (pda->cfg pda)))


(define (rename-nts-helper old-nts new-nts acc)
  (if (empty? old-nts)
      acc
      (let* ([translated-nt (gen-nt new-nts)]
             [new-acc (hash-set acc (first old-nts) translated-nt)]
             [new-nts (cons translated-nt new-nts)])
        (rename-nts-helper (rest old-nts) new-nts new-acc))))


(define (rename-nts G)
  (rename-nts-helper (filter (λ (nt) (not (or (eq? nt EMP)
                                              (member nt (CFG-sigma G)))))
                             (CFG-nts G))
                     '()
                     (hash)))


(define (rebuild-cfg improper-cfg nts-mapping)
  (let ([sigma (CFG-sigma improper-cfg)])
  (make-unchecked-cfg (hash-values nts-mapping)
                      sigma
                      (map (λ (rule)
                             (list (hash-ref nts-mapping (cfg-rule-lhs rule))
                                   ARROW
                                   (let ([RHS (cfg-rule-rhs rule) #;(symbol->list (cfg-rule-rhs rule))])
                                     (los->symbol (map (λ (r) (if (or (member r sigma)
                                                                      (eq? r EMP))
                                                     r
                                                     (hash-ref nts-mapping r)))
                                          RHS)))))
                           (CFG-rules improper-cfg))
                      (hash-ref nts-mapping (CFG-start improper-cfg)))))
                      ;(hash-map (λ (old-nt new-nt)
                                  


(define (pda->cfe pda)
  (let* ([G #;(pda->cfg pda) (unchecked->cfg (pda->cfg pda))]
         [renamed-nts-mapping (rename-nts G)]
         [proper-cfg (rebuild-cfg G renamed-nts-mapping)])
    #;(cfg->cfe2 proper-cfg)
    (unchecked->cfg proper-cfg)

    #;(values
     G
     renamed-nts-mapping
     (unchecked->cfg proper-cfg)
     (cfg->cfe2 proper-cfg))
    #;renamed-nts-mapping  #;(values renamed-nts-mapping #;(cfg-rules G) (cfg-nts G) #;renamed-nts-mapping))
  #;(pda->cfg pda))

;;cfe -> pda
;;Purpose: Converts the given cfe into a pda
(define/contract (cfe->pda cfe)
  cfe->pda/c
  (cfg->pda (cfe->cfg cfe)))

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
            (map (lambda (p1) (cfg-rule (los->symbol (list (pdarule-fromstate r) (if (eq? (pdarule-pop r) EMP) EMP (car (pdarule-pop r))) p))
                                        (if (eq? (pdarule-readsymb r) EMP)
                                            (list (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                                  (los->symbol (list p1 (cadr (pdarule-push r)) p)))
                                            (list (pdarule-readsymb r)
                                                  (los->symbol (list (pdarule-tostate r) (car (pdarule-push r)) p1))
                                                  (los->symbol (list p1 (cadr (pdarule-push r)) p))))))
                 states))
          
          (append-map (lambda (p) (gen-t3 p)) states))
        
        (append-map (lambda (r) (t3-maker r)) rls))


(define (push-length pushstuff)
    (if (eq? pushstuff EMP) 0 (length pushstuff)))

; (listof state) --> (listof cfg-rule)
      (define (gen-type4-rules states) 
        (map (lambda (s) (cfg-rule (los->symbol (list s EMP s)) (list EMP))) states))

(define (cfgrules-pda->cfg sm m S Z)
      
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


(define (fsmlos->symbol l) 
    (define (lostr->string l)
      (cond [(null? l) ""]
            [else (string-append (car l) (lostr->string (cdr l)))]))
    (string->symbol (lostr->string (map symbol->string l))))

(define (extract-nts-from-rule rule)
  (cons (cfg-rule-lhs rule) (cfg-rule-rhs rule)))


(define (pda->cfg m)
    
    ; (simple)ndpda symbol --> (listof cfg-rule)
    
    
    (let* ((sm (pda->spda m))
           (S (let ([states (remove-duplicates (append (pda-getstates m) (pda-getstates sm)))])
                (if (member 'S states)
                    (gen-nt states)
                    'S)))
           (Z (first (pda-getgamma sm)))
           (SIGMA (pda-getalphabet sm))
           (R (cfgrules-pda->cfg sm m S Z))
           (V (remove-duplicates (append-map extract-nts-from-rule R)) #;(append (remove-duplicates (append-map extract-nts-from-rule R)) SIGMA))
           )
       (cfg V SIGMA R S)))



; ndpda -> ndpda
  ; convert the given pda into a simple pda
  (define (pda->spda m)
    
    ;(listof pdarule) --> (listof pdarule)
    (define (pdarules->simplepdarules rules newstart newgamma newstates)
      
      ; (listof symbol) state (listof pdarule) (listof state) --> (list (listof pdarule) (listof state))
      (define (create-push-rules pushlist froms tos rls states)
        (if (null? (cdr pushlist))
            (cons (cons (list (list (car states) EMP EMP) (list tos pushlist)) rls)
                  states)
            (let* ((ns (gen-state states) #;(gen-symbol froms states))
                   (newrule (list (list (car states) EMP EMP) (list ns (list (car pushlist))))))
              (create-push-rules (cdr pushlist) froms tos (cons newrule rls) (cons ns states)))))
      
      ; (listof symbol) state (listof state) -->
      ;pdarule --> (listof pdarule)
      ; ASSUMPTION: r pushes 2 or more elements
      (define (convert-push-rule r states)
        (let* ((poplist (pdarule-pop r))
               (pushstuff (reverse (pdarule-push r)))
               (readelem (pdarule-readsymb r))
               (froms (pdarule-fromstate r))
               (tos (pdarule-tostate r))
               (ns (gen-state states) #;(gen-symbol froms states))
               (res (create-push-rules (cdr pushstuff) froms tos (list (list (list froms readelem poplist) (list ns (list (car pushstuff))))) (cons ns states))))
          res))
      
      
      
      ; (listof pdarules) (listof pdarules) (listof state) --> (list (listof pdarule) (listof state))
      (define (convert-push-rules rls savedrls states)
        (if (null? rls)
            (cons savedrls states)
            (let* ((res (convert-push-rule (car rls) states))
                   (newrls (car res))
                   (newstates (cdr res)))
              (convert-push-rules (cdr rls) (append savedrls newrls) newstates))))
      
      ; (listof symbol) (listof symbol) symbol state state (listof state) --> (list (listof pdarule) (listof state))
      (define (create-pop-rules poplist pushstuff readelem froms tos rls states)
        (if (null? (cdr poplist))
            (cons (cons (list (list (car states) readelem poplist) (list tos pushstuff)) rls)
                  states)
            (let* ((ns (gen-state states) #;(gen-symbol froms states))
                   (newrule (list (list (car states) EMP (list (car poplist))) (list ns EMP))))
              (create-pop-rules (cdr poplist) pushstuff readelem froms tos (cons newrule rls) (cons ns states)))))
      
      ;pdarule  (listof state) --> (list (listof pdarule) state+))
      ; ASSUMPTION: r pops 2 or more elements
      (define (convert-pop-rule r states)
        (let* ((poplist (pdarule-pop r))
               (pushstuff (pdarule-push r))
               (readelem (pdarule-readsymb r))
               (froms (pdarule-fromstate r))
               (tos (pdarule-tostate r))
               (ns (gen-state states) #;(gen-symbol froms states))
               (res (create-pop-rules (cdr poplist) pushstuff readelem froms tos (list (list (list froms EMP (list (car poplist))) (list ns EMP))) (cons ns states))))
          res))
      
      ; (listof pdarules) (listof pdarules) (listof states) --> (list (listof pdarules) states+)
      (define (convert-pop-rules rls rules states)
        (if (null? rls)
            (cons rules states)
            (let* ((res (convert-pop-rule (car rls) states))
                   (newrls (car res))
                   (newstates (cdr res)))
              (convert-pop-rules (cdr rls) (append newrls rules) newstates))))
      
      ; pdarule (listof pdarules) (listof symbol) --> (list (listof pdarule) state+)
      (define (convert-emptypop-rule r rls gamma)
        (if (null? gamma)
            rls
            (let ((newrule (list (list (pdarule-fromstate r) (pdarule-readsymb r) (list (car gamma)))
                                 (list (pdarule-tostate r) 
                                       (if (eq? (pdarule-push r) EMP)
                                           (list (car gamma))
                                           (list (car (pdarule-push r)) (car gamma)))))))
              (convert-emptypop-rule r (cons newrule rls) (cdr gamma)))))
      
      ; (listof pdarules) (listof pdarules) (listof state) --> (listof pdarules)
      (define (convert-emptypop-rules rls savedrules)
        (if (null? rls)
            savedrules
            (let* ((res (convert-emptypop-rule (car rls) savedrules newgamma)))
              (convert-emptypop-rules (cdr rls) res))))
      
      
      (let* ((poprules (filter (lambda (r) (and (not (eq? (pdarule-pop r) EMP))
                                                (>= (length (pdarule-pop r)) 2)))
                               rules))
             (res1 (convert-pop-rules poprules 
                                      (filter (lambda (r) (not (member r poprules))) rules)
                                      newstates))
             (newpdarules1 (car res1))
             (newpdastates1 (cdr res1))
             (pushrules (filter (lambda (r) (and (not (eq? (pdarule-push r) EMP))
                                                 (>= (length (pdarule-push r)) 2))) 
                                newpdarules1))
             (res2 (convert-push-rules pushrules
                                       (filter (lambda (r) (not (member r pushrules))) newpdarules1)
                                       newpdastates1))
             (newpdarules2 (car res2))
             (newpdastates2 (cdr res2))
             (emptypoprules (filter (lambda (r) (and (not (eq? (pdarule-fromstate r) newstart))
                                                     (eq? (pdarule-pop r) EMP))) 
                                    newpdarules2))
             (newpdarules3 (convert-emptypop-rules emptypoprules
                                                   (filter (lambda (r) (not (member r emptypoprules))) newpdarules2))))
        (cons newpdarules3 newpdastates2)))
    
    (let* ((newS (gen-state (pda-getstates m)))
           (newF (gen-state (cons newS (pda-getstates m))))
           (K (cons newS (cons newF (pda-getstates m))))
           (sigma (pda-getalphabet m))
           (Z (if (member 'Z (pda-getgamma m)) ;;keeps Z as bottom stack symbol if possible
                  (gen-state (pda-getgamma m))
                  'Z))
           (gamma (cons Z (pda-getgamma m)))
           (res (pdarules->simplepdarules (append (cons (list (list newS EMP EMP) 
                                                              (list (pda-getstart m) (list Z)))
                                                        (map (lambda (f) 
                                                               (list (list f EMP (list Z)) (list newF EMP)))
                                                             (pda-getfinals m)))
                                                  (pda-getrules m)) 
                                          newS 
                                          gamma
                                          K))
           (delta (car res))
           (finalK (cdr res)))
      (make-unchecked-ndpda finalK sigma gamma newS (list newF) delta)))

(struct pda (states alpha gamma start finals rules) #:transparent)

(define (unchecked->pda m)
  (pda (pda-getstates m)
       (pda-getalphabet m)
       (pda-getgamma m)
       (pda-getstart m)
       (pda-getfinals m)
       (pda-getrules m)))


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

(define (cfg->cfe2 G)
 
  ;;(listof X) (X -> Y) -> (hash X . Y)
  (define (make-hash-table lox f)
    (foldl (λ (x h)
             (hash-set h x (f x))
             #;(let ([res (f x)])
               (if (empty? res)
                   h
                   (hash-set h x res))))
           (hash)
           lox))

  ;;string -> (listof symbol)
  ;;Purpose: Seperates every character in the given string and puts them into a list
  (define (explode string nts)
    ;;natnum (listof symbol) -> (listof symbol)
    ;;Purpose: Seperates every character in the given string and puts them into a list
    ;;acc = the character in the string from [idx..(string-length string)]
    (define (explode-helper idx acc)
      (if (= idx 0)
          acc
          (let* ([next-sym (substring string (sub1 idx) idx)]
                 [res (cond [(member (string->symbol string) nts) (list (string->symbol string))]
                            ;;exploiting fact that the RHS of cfg rules converted from simple pda
                            ;;rules will structure terminal followed by nonterminal
                            [(string<=? "0" next-sym "9")
                             (list (string->symbol (substring string 0 1)) (string->symbol (substring string 1)))] 
                            [else (string->symbol next-sym)])]
                 [new-acc (if (list? res) res (cons res acc))]
                 [new-idx (if (list? res) 0 (sub1 idx))])
            ;(displayln string)
            ;(displayln (substring string 0 1))
            ;(displayln (substring string 1))
            (explode-helper new-idx new-acc))))
    (explode-helper (string-length string) '()))

  ;;(hash nts . (listof symbol)) (hash symbol . singleton-cfe)) (hash nts . variable-cfe)) -> (hash nts . cfe))
  ;;Purpose: Converts the RHS of cfg rules into cfes
  (define (make-cfexps-frm-rules rules singletons variables)
    ;;symbol -> cfe
    ;;Purpose: Matches the given symbol with the corresponding cfe
    (define (convert-to-expression RHS-of-rule)
      (cond [(eq? RHS-of-rule EMP) (empty-cfexp)]
            [(hash-has-key? singletons RHS-of-rule) (hash-ref singletons RHS-of-rule)]
            [(hash-has-key? variables RHS-of-rule) (hash-ref variables RHS-of-rule)]
            [else (error (format "unreadable RHS: ~a" RHS-of-rule))]))
    ;;(listof symbol) -> cfe
    ;;Purpose: Translates the given (listof symbol) into its corresponding cfe
    (define (rule->expression RHS-of-rule)
      (if (= (length RHS-of-rule) 1)
          (convert-to-expression (first RHS-of-rule))
          (concat-cfexp (map (λ (sym) (convert-to-expression sym)) RHS-of-rule))))
    (hash-map/copy rules (λ (nts RHS)
                           (values nts (cond [(empty? RHS) (empty-cfexp)]
                                             [(= (length RHS) 1) (rule->expression (first RHS))]
                                             [else (union-cfexp (map (λ (rule) (rule->expression rule)) RHS))])))))
  ;; symbol -> boolean
  ;;Purpose: Determines if the given symbol is a valid state or alphabet symbol
  (define (valid-RHS? sym)
    (or (valid-alpha? sym)
        (valid-state? sym)))
  
  (let* ([nts (cfg-get-v G)]
         [rules (make-hash-table nts (λ (nt) (filter-map (λ (rule) (and (eq? (first rule) nt)
                                                                        (symbol->fsmlos (third rule)) #;(explode (symbol->string (third rule)) nts)
                                                                                      #;(let* ([new-RHS (explode (symbol->string (third rule)) nts)]
                                                                                             [res (if (or (equal? new-RHS (list EMP))
                                                                                                          (andmap valid-RHS? new-RHS)) new-RHS void)])
                                                                                        #;(displayln nt)
                                                                                        #;(displayln res)
                                                                                        res
                                                                                        #;new-RHS)))
                                                                       (cfg-get-rules G))))]
         [start (cfg-get-start G)]
         [singletons (make-hash-table (cfg-get-alphabet G) singleton-cfexp)]
         [variables (make-hash-table nts var-cfexp)]
         [rules->cfexp (make-cfexps-frm-rules rules singletons variables)]
         [updated-bindings (hash-map/copy rules->cfexp (λ (key value)
                                                         (begin
                                                           (update-binding! (hash-ref variables key) key value)
                                                           (values key (hash-ref variables key)))))])
    #;(displayln updated-bindings)
    #;(displayln start)
     (hash-ref updated-bindings start)
    ;rules
    ;nts
    #;(unchecked->cfg G)))

(define (grammar-testequiv g1 g2 . l)
  (let* ((numtests (if (null? l) NUM-TESTS (car l)))
         (sigma1 (cfg-get-alphabet g1))
         (sigma2 (cfg-get-alphabet g2))
         (testlist (append (generate-words (floor (/ numtests 2)) sigma1 '())
                           (generate-words (ceiling (/ numtests 2)) sigma2 '())))
         (res1 (map (lambda (w) 
                      (let ((r (cfg-derive g1 w)))
                        (if (string? r) r (last r))))
                    testlist))
         (res2 (map (lambda (w) 
                      (let ((r (cfg-derive g2 w)))
                        (if (string? r) r (last r))))
                    testlist))
         (diffs (get-differences res1 res2 testlist)))
    (if (null? diffs) true diffs)))

(define cfe (pda->cfe (cfe->pda S1)))
;(pda->cfe (cfe->pda S1))
;cfe 
;G
;(cfg-derive G '(a b))
;(gen-cfexp-word cfe)
;(fsa-test-equivalence (cfe->pda S1) (pda->spda (cfe->pda S1)))