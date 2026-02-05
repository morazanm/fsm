#lang racket/base

(require racket/contract
         racket/list
         "../../../fsm-core/private/macros/validation/validation-predicates.rkt"
         "../../../fsm-core/private/macros/shared/shared-predicates.rkt"
         "../../../fsm-core/private/macros/error-formatting.rkt"
         "../../../fsm-core/private/sm-getters.rkt"
         "../../../fsm-core/private/constants.rkt")

(provide sm-viz/c
         ndfa-viz/c
         pda-viz/c
         tm-viz/c
         mttm-viz/c)
;;;;;;;;;;;;;;;;ERROR MESSAGE FORMATTERS


;;string string -> string
;;Purpose: Formats the error messages for the optional arguments for sm-viz
(define (keyword-arg-formatter arg-name arg-type)
  (format "The keyword argument, #:~a, expects a ~a as input, given" arg-name arg-type))

;;;;;;;;;;;;;;;;;;;;;PREDICATES

;;X -> boolean
;;Purpose: Determines if the given X is a finite-state machine
(define (machine? x)
  (and (procedure? x)
       (or (dfa? x)
           (ndfa? x)
           (ndpda? x)
           (tm? x)
           (mttm? x))))

;;X -> boolean
;;Purpose: Determines if the given X has an invariant-like structure e.i. (list symbol procedure)
(define (invariant? X)
  ;;list -> Boolean
  ;;Purpose: Determines if the given list is length 2
  (define (length-is-two? L)
    (= (length L) 2))
  (and (list? X)
       (length-is-two? X)
       (symbol? (car X))
       (procedure? (cadr X))))

;;(listof symbol) -> boolean
;;Purpose: Determines if the given (listof symbol) is a valid fsm word)
(define (valid-fsm-word? los)
  ;;X -> boolean
  ;;Purpose: Determines if X is a lowercase roman alphabet symbol, an arabic numeral, LM or BLANK
  (define (valid-alpha? x)
    (define regex-pattern (regexp "^[a-z0-9@_]$"))
    (and (or (symbol? x) (and (number? x) (<= 0 x 9)))
         (not (not (regexp-match regex-pattern
                                 (if (symbol? x)
                                     (symbol->string x)
                                     (number->string x)))))))
  (andmap valid-alpha? los))


;;X -> boolean
;;Purpose: Determines if the given X is one of the colorblind options
(define (valid-palette-input? x)
  (or (eq? x 'default)
      (eq? x 'prot)
      (eq? x 'deut)
      (eq? x 'trit)))

;; words sigma --> boolean
;purpose: to return if the given word is made of character from sigma
(define (word-in-sigma? word sigma)
  (andmap (lambda (letter) (member letter sigma)) word))

;;;;;;;;;;;;;;;;;;;;;;;CONTRACTS

;;contract
;;Purpose: Determines if the input is a finite state machine
(define is-machine/c
  (make-flat-contract
   #:name 'is-machine?
   #:projection (λ (blame)
                  (λ (val)
                    (or (machine? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "sm-viz expects a finite state machine as input, given"))))))))



;;contract
;;Purpose: Determines if the input is a valid FSM word
(define is-word/c
  (make-flat-contract
   #:name 'is-word?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (list? val)
                             (valid-fsm-word? val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            "sm-viz expects a list of fsm symbols [a-z] or [0-9] as input, given"))))))))

;;string string -> contract
;;Purpose: A contract to ensure valid add-dead input
(define (valid-add-dead-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-boolean-for-add-dead?
   #:projection (λ (blame)
                  (λ (val)
                    (or (boolean? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

;;string string -> contract
;;Purpose: A contract to ensure valid head-pos input
(define (valid-headpos-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-natnumber?
   #:projection (λ (blame)
                  (λ (val)
                    (or (natural-number/c val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

;;string string -> contract
;;Purpose: A contract to ensure valid cut-off input
(define (valid-cutoff-input/c arg-name arg-type)
  ;;Contract ;;Purpose: A contract to ensure that the input is geq to 1
  (define geq-to-one/c (>=/c 1))
  
  (make-flat-contract
   #:name 'is-natnumber?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (natural-number/c val)
                               (geq-to-one/c val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

;;string string -> contract
;;Purpose: A contract to ensure valid palette input
(define (valid-palette-input/c arg-name arg-type)
  (make-flat-contract
   #:name 'is-valid-symbol?
   #:projection (λ (blame)
                  (λ (val)
                    (or (valid-palette-input? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            val
                            (keyword-arg-formatter arg-name arg-type)))))))))

;;contract
;;Purpose: Determines if input for invaraints has the proper form
(define valid-invariant-input/c
  (make-flat-contract
   #:name 'is-valid-symbol?
   #:projection (λ (blame)
                  (λ (val)
                    (or (and (map list? val)
                             (andmap invariant? val))
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                            (if (list? val)
                                (filter-not invariant? val)
                                val)
                            "Expected an arbitrary amount of (list <state> <invariant>), given"))))))))

;;contract
;;Purpose: Determines if the input is a dfa/ndfa
(define ndfa/c
 (make-flat-contract
  #:name 'is-ndfa?
  #:projection (λ (blame)
                  (λ (val)
                    (or (and (procedure? val)
                             (or (dfa? val)
                                 (ndfa? val)))
                             ((λ ()
                                (current-blame-format format-error)
                                (raise-blame-error
                                 blame
                                 val
                                 (format 
                                  "~a-viz expects a ~a as input, given" (if (and (procedure? val) (dfa? val)) "dfa" "ndfa"))))))))))
;;contract
;;Purpose: Determines if the input is a pda
(define ndpda/c
 (make-flat-contract
  #:name 'is-ndpda?
  #:projection (λ (blame)
                  (λ (val)
                    (or (ndpda? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                           val
                            "pda-viz expects an pda as input, given"))))))))

;;contract
;;Purpose: Determines if the input is a tm
(define tm/c
 (make-flat-contract
  #:name 'is-tm?
  #:projection (λ (blame)
                  (λ (val)
                    (or (tm? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                           val
                            "tm-viz expects an tm as input, given"))))))))

;;contract
;;Purpose: Determines if the input is a mttm
(define mttm/c
 (make-flat-contract
  #:name 'is-mttm?
  #:projection (λ (blame)
                  (λ (val)
                    (or (mttm? val)
                        ((λ ()
                           (current-blame-format format-error)
                           (raise-blame-error
                            blame
                           val
                            "mttm-viz expects an mttm as input, given"))))))))

;;(listof symbol) symbol -> contract
;;Purpose: Determines if the input word only contains letters in sigma
(define (valid-fsa-word/c sigma m-type)
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (word-in-sigma? val sigma))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     val
                     (format "~a-viz expects a word with symbols in sigma, ~a, as input, given" m-type sigma))))))

;;(listof symbol) symbol -> contract
;;Purpose: Determines if the input word only contains letters in sigma
(define (valid-tm-word/c sigma m-type)
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (word-in-sigma? val (cons LM (cons BLANK sigma))))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     val
                     (format "~a-viz expects a word with symbols in sigma, ~a, as input, given"
                             (if (or (eq? 'tm-language-recognizer m-type)
                                     (eq? 'tm m-type))
                                 "tm"
                                 "mttm")
                             sigma))))))

;;(listof symbol) (listof symbol) symbol -> contract
;;Purpose: Determines if the invariants contain states in the FSA and are predicates
(define (valid-ndfa-invariant/c states word m-type)
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (and (andmap (λ (inv-state) (member inv-state states)) (map car val))
                               (andmap (λ (inv-func) (boolean? (inv-func word))) (map cadr val))))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     (filter-not (λ (inv)
                                   (and (boolean? ((cadr inv) word))
                                        (member (car inv) states))) val)
                     (format "~a-viz expects a list of state invariants predicates, given" m-type))))))

;;(listof symbol) (listof symbol) symbol -> contract
;;Purpose: Determines if the invariants contain states in the PDA and are predicates
(define (valid-pda-invariant/c states word m-type)
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (and (andmap (λ (inv-state) (member inv-state states)) (map car val))
                               (andmap (λ (inv-func) (boolean? (inv-func word empty))) (map cadr val))))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     (filter-not (λ (inv)
                                   (and (boolean? ((cadr inv) word empty))
                                        (member (car inv) states))) val)
                     (format "~a-viz expects a list of state invariants predicates, given" m-type))))))

;;(listof symbol) (listof symbol) symbol -> contract
;;Purpose: Determines if the invariants contain states in the tm and are predicates
(define (valid-tm-invariant/c states word m-type)
  (define sample-index (sub1 (length word))) ;;may cause errors
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (and (andmap (λ (inv-state) (member inv-state states)) (map car val))
                               (andmap (λ (inv-func) (boolean? (inv-func word sample-index))) (map cadr val))))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     (filter-not (λ (inv)
                                   (and (boolean? ((cadr inv) word sample-index))
                                        (member (car inv) states))) val)
                     (format "~a-viz expects a list of state invariants predicates, given"
                             (if (eq? 'tm m-type)
                                 "tm"
                                 "tm"
                                 )))))))

;;(listof symbol) natnum (listof symbol) symbol -> contract
;;Purpose: Determines if the invariants contain states in the mttm and are procedures
(define (valid-mttm-invariant/c states num-tapes word m-type)
  ;(define sample-tape (list 3 word))
  ;(define sample-tape-config (cons sample-tape (make-list (sub1 num-tapes) sample-tape)))
  (make-flat-contract
   #:name 'all-letters-in-sigma
   #:first-order (λ (val) (and (andmap (λ (inv-state) (member inv-state states)) (map car val))
                               (andmap (λ (inv-func) (procedure? inv-func)) (map cadr val))))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     (filter-not (λ (inv)
                                   (and (procedure? (cadr inv))
                                        (member (car inv) states))) val)
                     (format "~a-viz expects a list of state invariants predicates, given"
                             (if (eq? 'tm m-type)
                                 "mttm"
                                 "mttm"
                                 )))))))

;;(listof symbol)  symbol -> contract
;;Purpose: Determines if the head-pos is not larger than the input word
(define (valid-head-pos/c word m-type)
  (define leq-than-word-length (</c (length word)))
  (make-flat-contract
   #:name 'valid-headpos-for-viz
   #:first-order (λ (val) (leq-than-word-length val))
   #:projection (λ (blame)
                  (λ (val)
                    (current-blame-format format-error)
                    (raise-blame-error
                     blame
                     val
                     (format "~a-viz expects a head position less than the length of the input word, ~a, given"
                             (if (or (eq? 'tm-language-recognizer m-type)
                                     (eq? 'tm m-type))
                                 "tm"
                                 "mttm")
                             word))))))

;;;;;;;;;;;;;;;;;;;;;CONTRACT COMBINATOR



(define sm-viz/c
  (->* (is-machine/c is-word/c)
       (#:add-dead (valid-add-dead-input/c "add-dead" "boolean")
        #:cut-off (valid-cutoff-input/c "cut-off" "postive integer greater than 0")
        #:head-pos (valid-headpos-input/c "head-pos" "positive integer")
        #:palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]"))
       #:rest valid-invariant-input/c
       void?))


(define ndfa-viz/c
  (->i ([M ndfa/c]
        [word (M) (valid-fsa-word/c (sm-sigma M) (sm-type M))]
        [invariant (M word) (valid-ndfa-invariant/c (sm-states M) word (sm-type M))])
       (#:add-dead [add-dead (valid-add-dead-input/c "add-dead" "boolean")]
        #:palette [palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]")])
       [result void?]))

(define pda-viz/c
  (->i ([M ndpda/c]
        [word (M) (valid-fsa-word/c (sm-sigma M) (sm-type M))]
        [invariant (M word) (valid-pda-invariant/c (sm-states M) word (sm-type M))])
       (#:add-dead [add-dead (valid-add-dead-input/c "add-dead" "boolean")]
        #:cut-off [cut-off (valid-cutoff-input/c "cut-off" "postive integer greater than 0")]
        #:palette [palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]")])
       [result void?]))

(define tm-viz/c
  (->i ([M tm/c]
        [word (M) (valid-tm-word/c (cons BLANK (sm-sigma M)) (sm-type M))]
        [head-pos (word M) (valid-head-pos/c word (sm-type M))]
        [invariant (M word) (valid-tm-invariant/c (sm-states M) word (sm-type M))])
       (#:cut-off [cut-off (valid-cutoff-input/c "cut-off" "postive integer greater than 0")]
        #:palette [palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]")])
       [result void?]))


(define mttm-viz/c
  (->i ([M mttm/c]
        [word (M) (valid-tm-word/c (cons BLANK (sm-sigma M)) (sm-type M))]
        [head-pos (word M) (valid-head-pos/c word (sm-type M))]
        [invariant (M word) (valid-mttm-invariant/c (sm-states M) (sm-numtapes M) word (sm-type M))])
       (#:cut-off [cut-off (valid-cutoff-input/c "cut-off" "postive integer greater than 0")]
        #:palette [palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]")])
       [result void?]))



