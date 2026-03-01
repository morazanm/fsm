#lang racket/base

(provide sm-test-invs-fsa)

(require
  ;; These are required for the filtered-in
  (for-syntax racket/base
              racket/set)
  racket/require
  (filtered-in
   (λ (name)
     (define unsafe-methods-used
       (set "unsafe-vector*-ref"
            "unsafe-vector*-set!"
            "unsafe-vector*-length"
            "unsafe-car"
            "unsafe-cdr"
            "unsafe-bytes-ref"
            "unsafe-bytes-copy!"
            "unsafe-bytes-set!"))
     (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
           [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
           [else name]))
   racket/unsafe/ops)
  (only-in racket/fixnum for/fxvector in-fxvector)
  "../fsa.rkt"
  "../sm-getters.rkt"
  "work-stealing-deque.rkt"
  "packed-vector.rkt")

(#%declare #:unsafe)

(struct path-with-rep-count (path rep-counts))                                                                                           

(define (always-true x)
  #t)

(define (always-true-thunk)
  always-true)

(struct rule-struct (start-state read-elem dest-state))

(define (determine-num-bits-needed num-rules)
  
  (define (helper num)
    (if (>= (expt 2 num) num-rules)
        num
        (helper (add1 num))))
  (define res (helper 1))
  (if (byte? res)
      res
      (error "idk")))

;; machine . (list state (word -> boolean)) -> (listof (listof symbol))
;; Purpose: To return a list of the invarients that don't hold and the words that cause it not to hold
(define (sm-test-invs-fsa a-machine #:ds-remove [ds-remove #f] #:rep-limit [rep-limit 1] . a-loi)
  (define rules-len (length (sm-rules a-machine)))
  (define-values (make-gcfxvector
                  gcfxvector-ref
                  gcfxvector-add!
                  gcfxvector-add/copy)
    (create-gcfxv-functions (determine-num-bits-needed rules-len)))

  
  (define (new-find-paths a-machine rep-limit loi-hash)
    (cond [(fx> rules-len 0)
           (define rules
             (for/vector #:length rules-len
               ([rule (in-list (sm-rules a-machine))])
               (rule-struct (car rule) (cadr rule) (caddr rule))))
           
           (define rule-idx-from-final
             (for/hasheq ([rule (in-vector rules)])
               (values (rule-struct-dest-state rule)
                       (for/fxvector ([a-rule (in-vector rules)]
                                      [idx (in-naturals)]
                                      #:when (eq? (rule-struct-dest-state rule)
                                                  (rule-struct-start-state a-rule)))
                         idx))))
                         
           
           (define (word-of-path a-gv)
             (define (word-of-path-helper accum idx)
               (if (fx= idx 0)
                   (if (eq? 'ε (rule-struct-read-elem (vector*-ref rules (gcfxvector-ref a-gv 0))))
                       accum
                       (unsafe-cons-list (rule-struct-read-elem (vector*-ref rules (gcfxvector-ref a-gv 0))) accum))
                   (if (eq? 'ε (rule-struct-read-elem (vector*-ref rules (gcfxvector-ref a-gv idx))))
                       (word-of-path-helper accum (fx- idx 1))
                       (word-of-path-helper (unsafe-cons-list (rule-struct-read-elem (vector*-ref rules (gcfxvector-ref a-gv idx))) accum)
                                            (fx- idx 1)))))
             (word-of-path-helper '() (fx- (gcfxvector-n a-gv) 1)))

           
           (define (create-new-words stack qfirst parent-task)
             (define final-state
               (rule-struct-dest-state
                (vector*-ref rules
                             (gcfxvector-ref (path-with-rep-count-path qfirst)
                                             (fx- (gcfxvector-n (path-with-rep-count-path qfirst)) 1)))))
                    
             (for ([idx (in-fxvector (hash-ref rule-idx-from-final final-state))]
                   #:do [(define curr-rep-count (bytes-ref (path-with-rep-count-rep-counts qfirst)
                                                           idx))]
                   #:when (fx< curr-rep-count
                               rep-limit))
               (define new-vec (bytes-copy (path-with-rep-count-rep-counts qfirst)))
               (bytes-set! new-vec idx (fx+ curr-rep-count 1))
               (push! stack
                      (path-with-rep-count (gcfxvector-add/copy (path-with-rep-count-path qfirst) idx)
                                           new-vec)
                      parent-task))
             (define word-path (word-of-path (path-with-rep-count-path qfirst)))
             (if ((hash-ref loi-hash
                            final-state
                            always-true-thunk)
                  word-path)
                 '()
                 (list (list word-path final-state))))
           
           
           (define (accumulate-results-into-lst vec)
             (define (loop idx accum)
               (if (fx= idx (vector*-length vec))
                   accum
                   (loop (fx+ idx 1) (append (vector*-ref vec idx) accum))))
             (loop 0 '()))
           
           (accumulate-results-into-lst
            (run-parallel (for/list ([rule (in-vector rules)]
                                     [idx (in-naturals)]
                                     #:when (eq? (rule-struct-start-state rule) (sm-start a-machine)))
                            (define vec (make-bytes rules-len 0))
                            (bytes-set! vec idx 1)
                            (path-with-rep-count (make-gcfxvector 16 idx) vec))
                          create-new-words
                          8
                          2048))]
          [else (error "Need at least one rule to run this tool")]))
  
  
  (define a-loi-hash
    (for/hash 
        ([inv (in-list a-loi)])
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
            (unsafe-cons-list (list (list '() (sm-start a-machine)))
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

(define (DNA-K-INV a-word)
  (not (null? a-word)))

;;word -> boolean
;;Purpose: Determines if the given word has more a's than t's

(define (DNA-H-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (< num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of a's and t's

(define (DNA-F-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more t's than a's

(define (DNA-M-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (> num-t num-a)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of t's and a's

(define (DNA-I-INV a-word)
  (let ([num-a (length (filter (λ (w) (equal? w 'a)) a-word))]
        [num-t (length (filter (λ (w) (equal? w 't)) a-word))])
    (= num-a num-t)))

;;word -> boolean
;;Purpose: Determines if the given word has more c's than g's

(define (DNA-D-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-c num-g)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of c's and g's

(define (DNA-B-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (= num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has more g's than c's

(define (DNA-S-INV a-word)
  (let ([num-g (length (filter (λ (w) (equal? w 'g)) a-word))]
        [num-c (length (filter (λ (w) (equal? w 'c)) a-word))])
    (> num-g num-c)))

;;word -> boolean
;;Purpose: Determines if the given word has an even amount of g's and c's

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

#;(sm-test-invs-fsa EVIL-dna-sequence
                    #:rep-limit 2
                    #:ds-remove #f
                    (list 'K DNA-K-INV)
                    (list 'H DNA-H-INV)
                    (list 'F DNA-F-INV)
                    (list 'M DNA-M-INV)
                    (list 'I DNA-I-INV)
                    (list 'D DNA-D-INV)
                    (list 'B DNA-B-INV)
                    (list 'S DNA-S-INV)
                    (list 'R DNA-R-INV))

(define mini-monster-kaboom
  (make-unchecked-ndfa '(S A B J K L M)
                       '(k a
                           ;l
                           b
                           ;u g c h
                           o
                           ;i n
                           m)
                       'S
                       '(M)
                       `((S a S) (S b S)
                                 (S o S)
                                 (S m S) (S k A)
                                                                        
                                 (A a B) (A k S) (A b S)
                                 (A o S)
                                 (A m S)
                                                                        
                                 (B b J) (B k S) (B a S)
                                 (B o S)
                                 (B m S)


                                 (J k S) (J a S)
                                 (J b S)
                                 (J m S) (J o K)

                                 (K k S) (K a S) (K b S)
                                                          
                                 (K m S) (K o L)

                                 (L m M) (L k S) (L a S) (L b S)
                                 (L o S)
                                                          

                                 (M k M) (M a M) (M b M)
                                 (M o M)
                                 (M m M))))

;; contains-kaboom-kaching-kalabunga

(define monster-machine (make-unchecked-ndfa '(S A B C D E F G H I J K L M N O P Q R)
                                             '(k a l b u g c h o i n m)
                                             'S
                                             '(I M R)
                                             `((S a S) (S l S) (S b S) (S u S) (S g S)
                                                       (S c S) (S h S) (S o S) (S i S)
                                                       (S n S) (S m S) (S k A)
                                                                        
                                                       (A a B) (A k S) (A l S) (A b S) (A u S)
                                                       (A g S) (A c S) (A h S) (A o S)
                                                       (A w S) (A i S) (A n S) (A m S)
                                                                        
                                                       (B l C) (B b J) (B c N) (B k S) (B a S)
                                                       (B u S) (B g S) (B h S) (B o S)
                                                       (B i S) (B n S) (B m S)

                                                       (C k S) (C l S) (C b S) (C u S) (C g S)
                                                       (C c S) (C h S) (C o S) (C i S)
                                                       (C n S) (C m S) (C a D)

                                                       (D b E) (D k S) (D a S) (D l S) (D u S)
                                                       (D g S) (D c S) (D h S) (D o S)
                                                       (D i S) (D n S) (D m S)

                                                       (E u F) (E k S) (E a S) (E l S) (E b S)
                                                       (E g S) (E c S) (E h S) (E o S)
                                                       (E i S) (E n S) (E m S)

                                                       (F n G) (F k S) (F a S) (F l S) (F b S)
                                                       (F u S) (F g S) (F c S) (F h S) (F o S)
                                                       (F i S) (F m S)

                                                       (G g H) (G k S) (G a S) (G l S) (G b S)
                                                       (G u S) (G c S) (G h S) (G o S)
                                                       (G i S) (G m S) (G n S)

                                                       (H a I) (H k S) (H l S) (H b S) (H u S)
                                                       (H g S) (H c S) (H h S) (H o S)
                                                       (H i S) (H n S) (H m S)

                                                       (I k I) (I a I) (I l I) (I b I) (I u I)
                                                       (I g I) (I c I) (I h I) (I o I)
                                                       (I i I) (I n I) (I m I)

                                                       (J k S) (J a S) (J l S) (J u S) (J g S)
                                                       (J c S) (J h S) (J b S) (J i S)
                                                       (J n S) (J m S) (J o K)

                                                       (K o L) (K k S) (K a S) (K l S) (K b S)
                                                       (K u S) (K g S) (K c S) (K h S)
                                                       (K i S) (K n S) (K m S)

                                                       (L m M) (L k S) (L a S) (L l S) (L b S)
                                                       (L u S) (L g S) (L c S) (L h S) (L o S)
                                                       (L i S) (L n S)

                                                       (M k M) (M a M) (M l M) (M b M) (M u M)
                                                       (M g M) (M c M) (M h M) (M o M)
                                                       (M i M) (M n M) (M m M)

                                                       (N h O) (N k S) (N a S) (N l S) (N b S)
                                                       (N u S) (N g S) (N c S) (N o S)
                                                       (N i S) (N n S) (N m S)

                                                       (O i P) (O k S) (O a S) (O l S) (O b S)
                                                       (O u S) (O g S) (O c S) (O h S) (O o S)
                                                       (O n S) (O m S)

                                                       (P n Q) (P k S) (P a S) (P l S) (P b S)
                                                       (P u S) (P g S) (P c S) (P h S) (P o S)
                                                       (P i S) (P m S)

                                                       (Q g R) (Q k S) (Q a S) (Q l S) (Q b S)
                                                       (Q u S) (Q c S) (Q h S) (Q o S)
                                                       (Q i S) (Q n S) (Q m S)

                                                       (R k R) (R a R) (R l R) (R b R) (R u R)
                                                       (R g R) (R c R) (R h R) (R o R)
                                                       (R i R) (R n R) (R m R))))



(define LOI-monster-machine (list (list 'S always-true)
                                  (list 'A always-true)
                                  (list 'B always-true)
                                  (list 'C always-true)
                                  (list 'D always-true)
                                  (list 'E always-true)
                                  (list 'F always-true)
                                  (list 'G always-true)
                                  (list 'H always-true)
                                  (list 'I always-true)
                                  (list 'J always-true)
                                  (list 'K always-true)
                                  (list 'L always-true)
                                  (list 'M always-true)
                                  (list 'N always-true)
                                  (list 'O always-true)
                                  (list 'P always-true)
                                  (list 'Q always-true)
                                  (list 'R always-true)))



#;(time (sm-test-invs-fsa monster-machine
                          #:rep-limit 1
                          #:ds-remove #f
                          (list 'S always-true)
                          (list 'A always-true)
                          (list 'B always-true)
                          (list 'C always-true)
                          (list 'D always-true)
                          (list 'E always-true)
                          (list 'F always-true)
                          (list 'G always-true)
                          (list 'H always-true)
                          (list 'I always-true)
                          (list 'J always-true)
                          (list 'K always-true)
                          (list 'L always-true)
                          (list 'M always-true)
                          (list 'N always-true)
                          (list 'O always-true)
                          (list 'P always-true)
                          (list 'Q always-true)
                          (list 'R always-true)))

#;(time (sm-test-invs-fsa mini-monster-kaboom
                          #:rep-limit 1
                          #:ds-remove #f
                          (list 'S always-true)
                          (list 'A always-true)
                          (list 'B always-true)
                          (list 'J always-true)
                          (list 'K always-true)
                          (list 'L always-true)
                          (list 'M always-true)))


#;(time (sm-test-invs-fsa EVIL-dna-sequence
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

(define (always-false x)
  #f)


#;(length (sm-test-invs-fsa NO-AA
                            #:rep-limit 5
                            #:ds-remove #f
                            (list 'S always-true)
                            (list 'A always-true)
                            (list 'B always-false)
                            (list 'R always-true)))


#;(time (length (sm-test-invs-fsa NO-AA
                                  #:rep-limit 6
                                  #:ds-remove #f
                                  (list 'S always-true)
                                  (list 'A always-true)
                                  (list 'B always-false)
                                  (list 'R always-true))))
#;(visualize-futures-thunk
   (lambda () (length (sm-test-invs-fsa NO-AA
                                        #:rep-limit 4
                                        #:ds-remove #f
                                        (list 'S always-true)
                                        (list 'A always-true)
                                        (list 'B always-false)
                                        (list 'R always-true)))))
#;(profile-thunk (lambda () (length (sm-test-invs-fsa NO-AA
                                                      #:rep-limit 5
                                                      #:ds-remove #f
                                                      (list 'S always-true)
                                                      (list 'A always-true)
                                                      (list 'B always-false)
                                                      (list 'R always-true))))
                 #:svg-path "/home/aqua/Pictures/profile-results.svg"
                 #:preview? #t
                 #:threads #t
                 #:use-errortrace? #t)
#;(time (let ([res (sm-test-invs-fsa NO-AA
                                     #:rep-limit 6
                                     #:ds-remove #f
                                     (list 'S always-true)
                                     (list 'A always-true)
                                     (list 'B always-false)
                                     (list 'R always-true))])
          (displayln (length res))))