#lang racket/base
(require racket/list
         racket/treelist
         racket/set
         data/queue
         rackcheck
         racket/require
         "../regexp.rkt"
         "../fsa.rkt"
         "../sm-getters.rkt"
         "../../../sm-graph.rkt"
         "../rg-constructors.rkt"
         "../regular-grammar.rkt"
         "../constants.rkt"
         "../fsa.rkt"
         "../misc.rkt"
         "../sm-getters.rkt"
         "../grammar-getters.rkt"
         "../cfg-struct.rkt"
         "../cfg-constructors.rkt"
         "../pda.rkt"
         "../cfe-constructors/context-free-expressions-constructors.rkt"
         "../cfe-constructors/construct-cfe-macro.rkt"
         (for-syntax racket/base
                     racket/set)
         (filtered-in
          (λ (name)
            (define unsafe-methods-used
              (set "unsafe-vector*-ref"
                   "unsafe-vector*-set!"
                   "unsafe-vector*-length"
                   "unsafe-fxvector-ref"
                   "unsafe-fxvector-set!"
                   "unsafe-fxvector-length"
                   "unsafe-car"
                   "unsafe-cdr"))
            (cond [(regexp-match #rx"^unsafe-fx" name) (regexp-replace #rx"unsafe-" name "")]
                  [(regexp-match #rx"^unsafe-cons-list" name) "cons"]
                  [(set-member? unsafe-methods-used name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          racket/unsafe/ops)
         (filtered-in
          (λ (name)
            (cond [(regexp-match #rx"^unsafe-" name) (regexp-replace #rx"unsafe-" name "")]
                  [else name]))
          "../testing-utils/unsafe-struct-accessors.rkt")
         "../../../sm-graph.rkt")

;(provide quickcheck-invs-cfg)

;; cfg boolean (listof (listof nt inv)) -> hashtable
;; Purpose: To return a hashtable of all context-free expressions that
;;          can be made from the given cfg
(define (get-all-cfgexp a-cfg dead-state-removal? a-loi)
  ;; have to get all the nts that are being tested, then get the cfexp for those nts, then add everything to a ht
  (define nts-being-tested (map (λ (inv-pair) (first inv-pair)) a-loi)) ;<- gets all the nts that have to be tested

  (for/hash ([nt nts-being-tested])     ;<- ht of the nt and its cfexp 
    (values nt (cfg->cfexp a-cfg nt))))




;; cfg nonterminal -> cfexp
;; purpose: to return a context-free expression for the given nonterminal from the given grammar
(define (cfg->cfexp a-cfg new-start)
  (define old-nts (list->seteq (cons EMP (cfg-get-v a-cfg))))
  (define new-rules (mutable-set))
  (define new-nts (mutable-seteq))

  (define all-rules (cfg-get-rules a-cfg))
 
  ;; adds all the given nts rules and new nts to the set
  (define (add-nt-rules nt)
    (cond [(set-member? new-nts nt)
           (void)]
          [else (set-add! new-nts nt)
                (for ([rule (in-list all-rules)]
                      #:when (eq? nt (car rule))) ;<- means nt was just discovered, so add all nts rules
                  (set-add! new-rules rule)
                  (for ([symb (in-list (symbol->fsmlos (caddr rule)))]
                        #:when (set-member? old-nts symb))
                    (when (not (eq? symb EMP))
                      (add-nt-rules symb))))]))

  (define (found-new-rule rules)
    (cond [(null? rules)
           (find-new-rule (cfg-get-rules a-cfg))]
          [(and (set-member? new-nts (caar rules))
                (not (set-member? new-rules (car rules))))
           (set-add! new-rules (car rules))
           (set-add! new-nts (caar rules))
           (for ([symb (in-list (symbol->fsmlos (caddar rules)))]
                 #:when (set-member? old-nts symb))
             (when (not (eq? symb EMP))
               (set-add! new-nts symb)))
           (found-new-rule (cdr rules))]
          [else
           (found-new-rule (cdr rules))]))
  
  (define (find-new-rule rules)
    (cond [(null? rules)
           (void)]
          [(and (set-member? new-nts (caar rules))
                (not (set-member? new-rules (car rules))))
           (set-add! new-rules (car rules))
           (set-add! new-nts (caar rules))
           (for ([symb (in-list (symbol->fsmlos (caddar rules)))]
                 #:when (set-member? old-nts symb))
             (when (not (eq? symb EMP))
               (set-add! new-nts symb)))
           (found-new-rule (cdr rules))]
          [else
           (find-new-rule (cdr rules))]))
  #;(set-add! new-nts new-start)
  #;(find-new-rule (cfg-get-rules a-cfg))


  (add-nt-rules new-start) ;;;;;;;

  
  (define new-cfg
    (make-unchecked-cfg
     (set->list new-nts)
     (cfg-get-alphabet a-cfg)
     (set->list new-rules)
     new-start))
   
  (cfg->cfe new-cfg)) ;;<- added it converting it to a cfexp here



(define (pred a-stack next-rules)
  (or (eq? (PDA-rule-pop next-rules) EMP)
      (and (fx<= (PDA-rule-pop-length next-rules) (stack-len a-stack))
           (equal? (take (stack-elems a-stack)
                         (PDA-rule-pop-length next-rules))
                   (PDA-rule-pop next-rules)))))

(define (rule->PDA-rule rule)
  (PDA-rule (car (car rule))
            (cadr (car rule))
            (caddr (car rule))
            (car (cadr rule))
            (cadr  (cadr  rule))
            (if (eq? EMP (cadr (cadr rule)))
                0
                (length (cadr (cadr rule))))
            (if (eq? EMP (caddr (car rule)))
                0
                (length (caddr (car rule))))))

(define (make-new-stack new-rule curr-config-stack)
  (if (eq? (PDA-rule-push new-rule) EMP)
      (if (eq? (PDA-rule-pop new-rule) EMP)
          curr-config-stack
          (stack (drop (stack-elems curr-config-stack)
                       (PDA-rule-pop-length new-rule))
                 (fx- (stack-len curr-config-stack)
                      (PDA-rule-pop-length new-rule))))
      (if (eq? (PDA-rule-pop new-rule) EMP)
          (stack (append (PDA-rule-push new-rule)
                         (stack-elems curr-config-stack))
                 (fx+ (stack-len curr-config-stack)
                      (PDA-rule-push-len new-rule)))
          (stack (append (PDA-rule-push new-rule)
                         (drop (stack-elems curr-config-stack)
                               (PDA-rule-pop-length new-rule)))
                 (fx+ (stack-len curr-config-stack)
                      (fx- (PDA-rule-push-len new-rule)
                           (PDA-rule-pop-length new-rule)))))))

(define (make-new-word new-rule curr-config-word)
  (if (eq? EMP (PDA-rule-read new-rule))
      curr-config-word
      (cons (PDA-rule-read new-rule) curr-config-word)))

(define (make-new-config new-rule curr-config-stack curr-config-word)
  (config (PDA-rule-destination new-rule)
          (make-new-word new-rule curr-config-word)
          (make-new-stack new-rule curr-config-stack)))




(define MAX-KLEENE-LENGTH 20)

;; cfexp -> generator throw error
;; Purpose: To translate the given cfexp into a generator, that generates
;;          words that are in the language in the context-free expression
(define (translate-cfexp cfexp)
  (cond [(equal? null-cfexp cfexp)
         (error "cannot generate a word for the null-cfexp")]
        [(singleton-cfexp? cfexp)
         (gen:const (string->symbol (singleton-cfexp-a cfexp)))]
        [(concat-cfexp? cfexp)
         (apply gen:tuple (translate-cfexp (vector-ref (concat-cfexp-cfes cfexp) 0))
                    (translate-cfexp (vector-ref (concat-cfexp-cfes cfexp) 1)))] 
        [(union-cfexp? cfexp)
         (gen:choice (translate-cfexp (vector-ref (union-cfexp-cfes cfexp) 0))
                     (translate-cfexp (vector-ref (union-cfexp-cfes cfexp) 1)))]
        [(kleenestar-cfexp? cfexp)
         (gen:bind (gen:integer-in 0 MAX-KLEENE-LENGTH)
                   (λ (x) (gen:list (translate-cfexp (kleenestar-cfexp-c1 cfexp))
                                    #:max-length x)))]
        [(empty-cfexp? cfexp)
         (gen:const '())]))



;; nt regexp invariant natnum -> void throw error
;; Purpose: To test if a word generated from the given regexp holds for the invariant of the nt
(define (testing-function state cfexp inv tests)
  (check-property
   (make-config #:tests tests)
   (property #:name (format "~a-invariant" state)
             ([generated-word (gen:map (translate-cfexp cfexp) flatten)])
             (inv generated-word))))


;; cfg (listof (state invariant)) natnum -> void throw error 
;; Purpose: To quickcheck the invariants of the nts of the given context-free grammar
(define (quickcheck-invs-cfg a-cfg los&inv tests dead-state-removal?)
  (define los&regexp (get-all-cfgexp a-cfg dead-state-removal? los&inv))
  (define los&inv-only-states-that-reach-finals
    (filter (λ (s&inv) (hash-has-key? los&regexp (car s&inv))) los&inv))
  (for ([inv (in-list
              los&inv-only-states-that-reach-finals)])
    (testing-function (car inv) (hash-ref los&regexp (car inv)) (cadr inv) tests)))




;; syntactic categories
;; S: word is a palindrome
(define palindrome
  (make-cfg '(S)
            '(a b)
            `((S ,ARROW ,EMP)
              (S ,ARROW aSa)
              (S ,ARROW bSb)
              (S ,ARROW a)
              (S ,ARROW b))
            'S))

(define (S-INV w)
  (equal? w (reverse w)))


(make-cfe ([L (union-cfexp (concat-cfexp (singleton-cfexp "a") L (singleton-cfexp "b"))
                           (empty-cfexp))])
           L)