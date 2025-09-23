#lang typed/racket/base

(provide sm-test-invs-fsa)

(require racket/list
         racket/set
         racket/vector
         typed/racket/unsafe
         racket/undefined)

(unsafe-require/typed "../fsa.rkt"
                      [make-unchecked-dfa (-> (Listof Symbol) (Listof Symbol) Symbol (Listof Symbol) (Listof (List Symbol Symbol Symbol))
                                              Procedure)]
                      [complement-fsa (-> Procedure Procedure)])

(unsafe-require/typed "../sm-getters.rkt"
                      [sm-start (-> Procedure Symbol)]
                      [sm-finals (-> Procedure (Listof Symbol))]
                      [sm-sigma (-> Procedure (Listof Symbol))]
                      [sm-rules (-> Procedure (Listof (List Symbol Symbol Symbol)))])

(unsafe-require/typed racket/vector
                      [vector-set/copy (-> (Vectorof Byte) Byte Byte (Vectorof Byte))])

(struct path-with-hash ([path : gbvector] [hash : Bytes]) #:transparent)

(struct array-stack ([vec : (Vectorof (Option path-with-hash))]
                     [capacity : Index]
                     [size : Index])
  #:mutable)

(: make-stack (-> Positive-Index array-stack))
(define (make-stack capacity)
  (define new-vec : (Mutable-Vectorof (Option path-with-hash)) (make-vector capacity #f))
  (array-stack new-vec capacity 0))

(: ensure-free-space-stack! (-> array-stack Positive-Index Void))
(define (ensure-free-space-stack! a-q needed-free-space)
  (define cap (vector-length (array-stack-vec a-q)))
  (define needed-cap (+ (array-stack-size a-q) needed-free-space))
  (unless (<= needed-cap cap)
    (: loop (-> Index Index))
    (define (loop new-cap)
      (if (<= needed-cap new-cap)
          new-cap
          (loop (let ([res (* 2 new-cap)])
                  (if (index? res)
                      res
                      (error "Too large"))))))
    (define new-cap (loop (max DEFAULT-CAPACITY cap)))
    (: new-vec (Mutable-Vectorof (Option path-with-hash)))
    (define new-vec (make-vector new-cap #f))
    (vector-copy! new-vec 0 (array-stack-vec a-q))
    (set-array-stack-vec! a-q new-vec)
    (set-array-stack-capacity! a-q new-cap)))

(: push! (-> array-stack path-with-hash Void))
(define (push! stack val)
  (when (= (array-stack-size stack) (array-stack-capacity stack))
    (ensure-free-space-stack! stack 1))
  (vector-set! (array-stack-vec stack)
               (array-stack-size stack)
               val)
  (set-array-stack-size! stack (let ([res (add1 (array-stack-size stack))])
                                 (if (index? res)
                                     res
                                     (error "")))))

(: pop! (-> array-stack path-with-hash))
(define (pop! stack)
  (define temp (vector-ref (array-stack-vec stack) (sub1 (array-stack-size stack))))
  (set-array-stack-size! stack (let ([res (sub1 (array-stack-size stack))])
                                   (if (index? res)
                                       res
                                       (error ""))))
  (if temp
      temp
      (error "")))

(define DEFAULT-CAPACITY 10)

(struct gbvector ([vec : Bytes] [n : Positive-Index]) #:mutable)

(: make-gbvector (-> Positive-Index Byte gbvector))
(define (make-gbvector capacity init-value)
  (define vec (make-bytes capacity 0))
  (bytes-set! vec 0 init-value)
  (gbvector vec 1))

(: ensure-free-space-bytes! (-> gbvector Index Void))
(define (ensure-free-space-bytes! gbv needed-free-space)
  (define vec (gbvector-vec gbv))
  (define n (gbvector-n gbv))
  (define cap (bytes-length vec))
  (define needed-cap (+ n needed-free-space))
  (unless (<= needed-cap cap)
    (: loop (-> Index Index))
    (define (loop new-cap)
      (if (<= needed-cap new-cap)
          new-cap
          (loop (let ([res (* 2 new-cap)])
                  (if (index? res)
                      res
                      (error "Too large"))))))
    (define new-cap (loop (max DEFAULT-CAPACITY cap)))
    (define new-vec (make-bytes new-cap 0))
    (bytes-copy! new-vec 0 vec)
    (set-gbvector-vec! gbv new-vec)))

(: gbvector-add! (-> gbvector Byte Void))
(define (gbvector-add! gv item)
  (ensure-free-space-bytes! gv 1)
     (define n (gbvector-n gv))
     (define v (gbvector-vec gv))
     (bytes-set! v n item)
     (set-gbvector-n! gv (let ([res (add1 n)])
                          (if (index? res)
                              res
                              (error "Vector size is too large")))))

(: gbvector-ref (-> gbvector Index Byte))
(define (gbvector-ref gv index)
  (let ([res (bytes-ref (gbvector-vec gv) index)])
    (if res
        res
        (error "Index unallocated value in gvec"))))

(: gbvector-add/copy (-> gbvector Byte gbvector))
(define (gbvector-add/copy gv val)
  (define new-vec
    (gbvector (bytes-copy (gbvector-vec gv))
             (gbvector-n gv)))
  (gbvector-add! new-vec val)
  new-vec)

;                                                                                                     
;                                                                                                     
;                                                                                                     
;                             ;                                                                       
;                             ;                                                                       
;  ;;;    ;;;                                              ;;;;;                                      
;   ;;    ;;                                                  ;;                                      
;   ; ;  ; ;     ;;;;       ;;;      ;; ;;;                  ;  ;      ;;  ;;      ;;;;       ;;;;    
;   ; ;  ; ;    ;    ;        ;       ;;   ;                 ;  ;       ;;;  ;    ;    ;     ;    ;   
;   ; ;  ; ;         ;        ;       ;    ;                 ;  ;       ;        ;      ;         ;   
;   ;  ;;  ;    ;;;;;;        ;       ;    ;                ;    ;      ;        ;;;;;;;;    ;;;;;;   
;   ;  ;;  ;   ;     ;        ;       ;    ;                ;;;;;;      ;        ;          ;     ;   
;   ;      ;   ;     ;        ;       ;    ;                ;    ;      ;        ;          ;     ;   
;   ;      ;   ;    ;;        ;       ;    ;               ;      ;     ;         ;     ;   ;    ;;   
;  ;;;    ;;;   ;;;; ;;    ;;;;;;;   ;;;  ;;;             ;;;    ;;;   ;;;;;;      ;;;;;     ;;;; ;;  
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     
;                                                                                                     

(define (always-true x)
  #t)

(define (always-true-thunk)
  always-true)

(struct rule-struct ([start-state : Symbol] [read-elem : Symbol] [dest-state : Symbol] [idx : Byte]))

(: new-find-paths (-> Procedure Byte (Immutable-HashTable Symbol (-> (Listof Symbol) Boolean))
                      (Listof (List (Listof Symbol) Symbol))))
(define (new-find-paths a-machine rep-limit loi-hash)
  (: rules-len Byte)
  (define rules-len (let ([len (length (sm-rules a-machine))])
                      (if (< 255 len)
                          (error "Number of rules cannot be >255")
                          len)))
  (: idx-vec (Mutable-Vectorof Byte))
  (define idx-vec (make-vector rules-len 0))

  (: loop (-> Byte Void))
  (define (loop idx)
    (if (= idx rules-len)
        (void)
        (begin
          (vector-set! idx-vec idx idx)
          (let ([res (add1 idx)])
            (if (< 255 res)
                (error "This shouldn't happen")
                (loop res))))))
  (loop 0)
  
  (define rules (for/vector : (Mutable-Vectorof rule-struct) #:length rules-len
                  ([rule (in-list (sm-rules a-machine))]
                   [idx (in-vector idx-vec)])
                  (rule-struct (car rule) (cadr rule) (caddr rule) idx)))

  (: new-new-word-of-path (-> gbvector (Listof Symbol)))
  (define (new-new-word-of-path a-gv)
    (: word-of-path-helper (-> (Listof Symbol) Index (Listof Symbol)))
    (define (word-of-path-helper accum idx)
      (if (= idx 0)
          (if (eq? 'ε (rule-struct-read-elem (vector-ref rules (gbvector-ref a-gv 0))))
              accum
              (cons (rule-struct-read-elem (vector-ref rules (gbvector-ref a-gv 0))) accum))
          (if (eq? 'ε (rule-struct-read-elem (vector-ref rules (gbvector-ref a-gv idx))))
              (word-of-path-helper accum (sub1 idx))
              (word-of-path-helper (cons (rule-struct-read-elem (vector-ref rules (gbvector-ref a-gv idx))) accum)
                                   (sub1 idx)))))
    (word-of-path-helper '() (sub1 (gbvector-n a-gv))))
  
  (: new-word-of-path (-> (Listof Byte) (Listof Symbol)))
  (define (new-word-of-path a-lor)
    (: word-of-path-helper (-> (Listof Symbol) (Listof Byte) (Listof Symbol)))
    (define (word-of-path-helper accum a-lor)
      (if (null? a-lor)
          accum
          (if (eq? 'ε (rule-struct-read-elem (vector-ref rules (car a-lor))))
              (word-of-path-helper accum (cdr a-lor))
              (word-of-path-helper (cons (rule-struct-read-elem (vector-ref rules (car a-lor))) accum) (cdr a-lor)))))
    (word-of-path-helper '() a-lor))

  (define new-stack (make-stack 10))
  
  (: find-paths-helper (-> Natural (Listof (List (Listof Symbol) Symbol)) (Listof (List (Listof Symbol) Symbol))))
  (define (find-paths-helper count accum )
    (collect-garbage 'incremental)
    (when (= 0 (modulo count 1000000))
      (display "Current count: ")
      (displayln count))
    (if (= (array-stack-size new-stack) 0)
        accum
        (let* [(qfirst (pop! new-stack))
               (final-state (rule-struct-dest-state
                             (vector-ref rules
                                         (gbvector-ref (path-with-hash-path qfirst)
                                                       (sub1 (gbvector-n (path-with-hash-path qfirst)))))))]
          (for ([rule (in-vector rules)]
                #:do [(define curr-rep-count (bytes-ref (path-with-hash-hash qfirst)
                                           (rule-struct-idx rule)))]
                #:when (and (eq? final-state
                                 (rule-struct-start-state rule))
                            (< curr-rep-count
                               rep-limit)))
            (push! new-stack (path-with-hash (gbvector-add/copy (path-with-hash-path qfirst) (rule-struct-idx rule))
                                       (let ([new-vec (bytes-copy (path-with-hash-hash qfirst))])
                                         (bytes-set! new-vec
                                                     (rule-struct-idx rule)
                                                     (let ([res (add1 curr-rep-count)])
                                                       (if (< 255 res)
                                                           (error "Rep count cannot be above 255")
                                                           res)))
                                         new-vec))))
          (let ([word-path (new-new-word-of-path (path-with-hash-path qfirst))])
            (if ((hash-ref loi-hash
                           final-state
                           always-true-thunk)
                 word-path)
                (find-paths-helper (add1 count) accum)
                (find-paths-helper (add1 count) (cons (list word-path final-state) accum)))))))
  
  (for ([rule (in-vector rules)]
        #:when (eq? (rule-struct-start-state rule) (sm-start a-machine)))
    (let ([vec (make-bytes rules-len 0)])
      (bytes-set! vec (rule-struct-idx rule) 1)
      (push! new-stack (path-with-hash (make-gbvector 10 (rule-struct-idx rule))
                                vec))))
  
  (find-paths-helper 0 '()))

;; ndfa -> ndfa
;; Purpose: Takes in ndfa and remove states and rules that can't reach a final state
#;(define (remove-states-that-cannot-reach-finals a-ndfa)
  (define rules (sm-rules a-ndfa))
  
  ;; machine -> (listof rules)
  ;; Purpose: To return all the paths in the given machine 
  (define (explore-all-paths a-machine)
    ;; (queueof (listof rule)) (listof (listof rule)) -> (listof (listof rule))
    ;; Purpose: To return all the paths of the given machine
    ;; Accumulator invariant: paths = list of current paths
    (define (explore-all-paths-helper)
      (if (queue-empty?)
          '()
          (let [(firstofq (dequeue!))]
            (for ([rule (in-list rules)]
                  #:when (and (eq? (caddr (car firstofq)) (car rule))
                              (not (member rule firstofq))))
              (enqueue! (cons rule firstofq)))
            (cons firstofq (explore-all-paths-helper)))))
    (for ([rule (in-list rules)]
          #:when (eq? (car rule) (sm-start a-machine)))
      (enqueue! (list rule)))
    (explore-all-paths-helper))
  
  (define new-states-set (mutable-seteq))
  (define new-rules-set (mutable-set))
  (for* ([path (in-list (for/list ([path (in-list (explore-all-paths a-ndfa))]
                                   #:when (member (caddr (car path)) (sm-finals a-ndfa)))
                          (reverse path)))]
         [rule (in-list path)])
    (set-add! new-states-set (car rule))
    (set-add! new-states-set (caddr rule)))
  (for ([rule (in-list rules)]
        #:when (and (set-member? new-states-set (car rule))
                    (set-member? new-states-set (caddr rule))))
    (set-add! new-rules-set rule))
  (make-unchecked-ndfa (set->list new-states-set) (sm-sigma a-ndfa) (sm-start a-ndfa) (sm-finals a-ndfa) (set->list new-rules-set)))

;; (listof rules) -> word
;; Purpose: To return a word that is made from the given list of rules
#;(define (word-of-path a-lor)
  (define (word-of-path-helper accum a-lor)
    (if (null? a-lor)
        accum
        (if (eq? 'ε (cadr (car a-lor)))
            (word-of-path-helper accum (cdr a-lor))
            (word-of-path-helper (cons (cadr (car a-lor)) accum) (cdr a-lor)))))
  (word-of-path-helper '() a-lor))

;; machine (listof (list state (word -> boolean))) -> (listof (listof state (listof word)))
;; Purpose: To return a list of all posible words that can be at each state in a machine 
#;(define (sm-all-possible-words a-machine a-loi)
    ;; the given machine without the states and rules of states that cannot reach a final state
    (define new-machine (if (eq? (sm-type a-machine) 'dfa)
                            a-machine
                            (remove-states-that-cannot-reach-finals a-machine)))
    ;; list of invariants that are reachable from the starting configuration
    #;(define reachable-inv (filter (λ (x) (member? (car x) (sm-states new-machine))) a-loi))
    ;; all paths of new-machine
    (define all-paths-new-machine (find-paths new-machine))

    ;; (listof (listof rule)) (listof (listof symbol)) -> (listof (listof symbol))
    ;; Purpose:  To return a list of all posible words that can be at each state in a machine 
    ;; Accumulator Invarient: accum = list of lists of words with the states that the can possibly be at
    (define (sm-all-possible-words-helper all-paths accum)
      (if (null? all-paths)
          accum
          (sm-all-possible-words-helper (cdr all-paths)
                                        (cons (list (word-of-path (car all-paths))
                                                    (caddr (last (car all-paths)))) accum))))

    ;; (listof symbol) -> (listof (symbol (listof word)))
    ;; Purpose: To generate a list of lists of a state and an empty list for each given states
    (define (build-list-of-states-&-empty-low states)
      ;; (listof symbol) (listof word)-> (listof (symbol '()))
      ;; Purpose: To generate a list of lists of a state and an empty list for each given states
      ;; Accumulator Invariant: accum = list containing a list with the state and an empty list of words
      (define (build-list-of-states-&-empty-low-helper states accum)
        (if (null? states) accum
            (build-list-of-states-&-empty-low-helper (cdr states) (cons (list (car states) '()) accum))))
      (build-list-of-states-&-empty-low-helper states '()))
          
    (define states-&-empty-low (build-list-of-states-&-empty-low (sm-states new-machine)))

    ;; (listof (listof word state)) -> (listof (listof state (listof word)))
    ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
    (define (sort-words listof-all-words-&-states)
      ;; (listof (word state)) -> (listof word)
      ;; Purpose: To make the given (listof (word state)) into a list of words
      (define (make-low list-of-words-&-states)
        ;; (listof (word state)) -> (listof word)
        ;; Purpose: To make the given (listof (word state)) into a list of words
        ;; Accumulator Invariant: accum = current list of words for the state
        (define (make-low-helper list-of-words-&-states accum)
          (if (null? list-of-words-&-states) accum
              (make-low-helper (cdr list-of-words-&-states) (cons (car (car list-of-words-&-states)) accum))))
        (make-low-helper list-of-words-&-states '()))
      ;; (listof (listof word state)) -> (listof (listof state (listof word)))
      ;; Purpose: To sort the words to be a list of the state and the words that can possibly reach that state
      ;; Accumulator Invairant: accum = list of states with all the possible words in them 
      (define (sort-words-helper states-&-empty-low accum)
        (if (null? states-&-empty-low) accum
            (sort-words-helper (cdr states-&-empty-low)
                               (cons (list (car (car states-&-empty-low))
                                           (make-low (filter (λ (x) (eq? (car (car states-&-empty-low)) (cadr x)))
                                                             listof-all-words-&-states))) accum))))
            
      (sort-words-helper states-&-empty-low '()))
    (sort-words (sm-all-possible-words-helper all-paths-new-machine
                                              (list (list '() (sm-start a-machine))))))

;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(: sm-test-invs-fsa (->* (Procedure) (#:ds-remove Boolean #:rep-limit Byte) #:rest (List Symbol (-> (Listof Symbol) Boolean))
                         (Listof (List (Listof Symbol) Symbol))))
(define (sm-test-invs-fsa a-machine #:ds-remove [ds-remove #f] #:rep-limit [rep-limit 1] . a-loi)
  (: a-loi-hash (Immutable-HashTable Symbol (-> (Listof Symbol) Boolean)))
  (define a-loi-hash
    (for/hash : (Immutable-HashTable Symbol (-> (Listof Symbol) Boolean))
      ([inv : (List Symbol (-> (Listof Symbol) Boolean)) (in-list a-loi)])
      (values (car inv) (cadr inv))))
  ;; the given machine without the states and rules of states that cannot reach a final state
  (define new-machine a-machine #;(if ds-remove
                          (remove-states-that-cannot-reach-finals a-machine)
                          a-machine))
  
  (if (null? a-loi)
      '()
      (let [(start-pair (hash-ref a-loi-hash (sm-start a-machine) #f))]
        (if (or (not (pair? start-pair))
                ((cadr start-pair) '()))
            (new-find-paths new-machine rep-limit a-loi-hash)
            (cons (list (list '() (sm-start a-machine)))
                  (new-find-paths new-machine rep-limit a-loi-hash))))))

(define NO-AA
  (make-unchecked-dfa
   '(S A B R)
   '(a b)
   'S
   '(S A B)
   '((S a A) (S b B)
             (B a A) (B b B)
             (A a R) (A b B)
             (R a R) (R b R))))

(define DNA-SEQUENCE (make-unchecked-dfa '(K H F M I D B S R)
                                         '(a t c g)
                                         'K
                                         '(K F I B R)
                                         `((K a H) (H t F) (F a H) (F t M) (F c D) (F g S)  
                                                   (K t M) (M a I) (I a H) (I t M) (I c D) (I g S)
                                                   (K c D) (D g B) (B a H) (B t M) (B c D) (B g S)
                                                   (K g S) (S c R) (R a H) (R t M) (R c D) (R g S))))

(define EVIL-dna-sequence (complement-fsa DNA-SEQUENCE))


;;word -> boolean
;;Purpose: Determines if the given word is empty
(: DNA-K-INV (-> (Listof Symbol) Boolean))
(define (DNA-K-INV a-word)
  (not (empty? a-word)))

;;word -> boolean
;;Purpose: Determines if the given word has more a's than t's
(: DNA-H-INV (-> (Listof Symbol) Boolean))
(define (DNA-H-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (< num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of a's and t's
(: DNA-F-INV (-> (Listof Symbol) Boolean))
(define (DNA-F-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more t's than a's
(: DNA-M-INV (-> (Listof Symbol) Boolean))
(define (DNA-M-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-t num-a)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of t's and a's
(: DNA-I-INV (-> (Listof Symbol) Boolean))
(define (DNA-I-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more c's than g's
(: DNA-D-INV (-> (Listof Symbol) Boolean))
(define (DNA-D-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-c num-g)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of c's and g's
(: DNA-B-INV (-> (Listof Symbol) Boolean))
(define (DNA-B-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has more g's than c's
(: DNA-S-INV (-> (Listof Symbol) Boolean))
(define (DNA-S-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of g's and c's
(: DNA-R-INV (-> (Listof Symbol) Boolean))
(define (DNA-R-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

#;(time (length (sm-test-invs-fsa EVIL-dna-sequence
                  #:rep-limit 1
                  #:ds-remove #f
                  (list 'K DNA-K-INV)
                  (list 'H DNA-H-INV)
                  (list 'F DNA-F-INV)
                  (list 'M DNA-M-INV)
                  (list 'I DNA-I-INV)
                  (list 'D DNA-D-INV)
                  (list 'B DNA-B-INV)
                  (list 'S DNA-S-INV)
                  (list 'R DNA-R-INV))))

(length (sm-test-invs-fsa EVIL-dna-sequence
                  #:rep-limit 1
                  #:ds-remove #f
                  (list 'K always-true)
                  (list 'H always-true)
                  (list 'F always-true)
                  (list 'M always-true)
                  (list 'I always-true)
                  (list 'D always-true)
                  (list 'B always-true)
                  (list 'S always-true)
                  (list 'R always-true)))