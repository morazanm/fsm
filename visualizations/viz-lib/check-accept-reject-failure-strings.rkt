#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/list
         "fsm-type-predicates.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE ACCEPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-accept machine-name failed-words)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following words: ~a"
          machine-name
          (apply string-append (cons (format "\n~s" (first failed-words))
                                     (map (lambda (n) (format "\n~s" n))
                                          (rest failed-words))))))

(define (named-single-failure-machine-accept machine-name failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following word: ~a"
          machine-name
          failed-word))

(define (anonymous-multi-failure-machine-accept failed-words)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following words: ~a"
          (apply string-append (cons (format "\n~s" (first failed-words))
                                     (map (lambda (n) (format "\n~s" n))
                                          (rest failed-words))))))

(define (anonymous-single-failure-machine-accept failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following word: ~a"
          failed-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE REJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-reject machine-name failed-words)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following words: ~a"
          machine-name
          (apply string-append (cons (format "\n~s" (first failed-words))
                                     (map (lambda (n) (format "\n~s" n))
                                          (rest failed-words))))))

(define (named-single-failure-machine-reject machine-name failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following word: ~a"
          machine-name
          failed-word))

(define (anonymous-multi-failure-machine-reject failed-words)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following words: ~a"
          (apply string-append (cons (format "\n~s" (first failed-words))
                                     (map (lambda (n) (format "\n~s" n))
                                          (rest failed-words))))))

(define (anonymous-single-failure-machine-reject failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following word: ~a"
          failed-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID NONTERMINAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-machine-invalid-nonterminal machine-name invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine, ~s: ~a"
          machine-name
          invalid-word))

(define (named-multi-failure-machine-invalid-nonterminal machine-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine, ~s:"
                 machine-name)
         invalid-words))

(define (anonymous-single-failure-machine-invalid-nonterminal invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine: ~a"
          invalid-word))

(define (anonymous-multi-failure-machine-invalid-nonterminal invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine:"
         invalid-words))

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-expression machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "The constructed machine, ~s, cannot test the following expressions because they are not valid words:" machine-name)
         invalid-expressions))

(define (named-single-failure-machine-invalid-expression machine-name invalid-expression)
  (format "The constructed machine, ~s, cannot test the following expression because it is not a valid word:\n~a" machine-name invalid-expression))

(define (anonymous-multi-failure-machine-invalid-expression invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "The constructed machine cannot test the following expressions because they are not valid words:")
         invalid-expressions))

(define (anonymous-single-failure-machine-invalid-expression invalid-expression)
  (format "The constructed machine cannot test the following expression because it is not a valid word:\n~a" invalid-expression))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID ARITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-arity machine-name invalid-expressions)
  (void))

(define (named-single-failure-machine-invalid-arity machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has arity ~a but expected 2: ~a"
          machine-name
          (length invalid-expression)
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-arity invalid-expressions)
  (void))

(define (anonymous-single-failure-machine-invalid-arity invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case has arity ~a but expected 2: ~a"
          (length invalid-expression)
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID HEAD-POS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-head-pos machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a natural number:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-invalid-head-pos machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a natural number: ~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-head-pos invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a natural number:"
          )
         invalid-expressions))

(define (anonymous-single-failure-machine-invalid-head-pos invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a natural number: ~a"
          
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID HEAD-POS INDEX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-head-pos-index machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a valid index into their respective test words:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-invalid-head-pos-index machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a valid index into its respective test word: ~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-head-pos-index invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a valid index into their respective test words:"
          )
         invalid-expressions))

(define (anonymous-single-failure-machine-invalid-head-pos-index invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a valid index into its respective test word: ~a"
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE NO LEFT HAND MARKER WORDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-no-left-hand-marker machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, do not contain a left hand marker:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-no-left-hand-marker machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, does not contain a left hand marker: ~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-no-left-hand-marker invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases do not contain a left hand marker:")
         invalid-expressions))

(define (anonymous-single-failure-machine-no-left-hand-marker invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case does not contain a left hand marker: ~a"
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR ACCEPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-accept grammar-name invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following word: ~a"
          grammar-name
          invalid-word))

(define (named-multi-failure-grammar-accept grammar-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following words:"
                 grammar-name)
         invalid-words))

(define (anonymous-single-failure-grammar-accept invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following word: ~a"
          invalid-word))

(define (anonymous-multi-failure-grammar-accept invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following words:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR REJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-reject grammar-name invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, derives the following word: ~a"
          grammar-name
          invalid-word))

(define (named-multi-failure-grammar-reject grammar-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, derives the following words:"
                 grammar-name)
         invalid-words))

(define (anonymous-single-failure-grammar-reject invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine derives the following word: ~a"
          invalid-word))

(define (anonymous-multi-failure-grammar-reject invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed machine derives the following words:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR INVALID NONTERMINALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-invalid-nonterminal grammar-name invalid-nonterminal)
  (format "Step 4 of the design recipe has not been successfully completed. The following word contains nonterminals not in the language of the constructed grammar, ~s: ~a"
          grammar-name
          invalid-nonterminal))

(define (named-multi-failure-grammar-invalid-nonterminal grammar-name invalid-nonterminals)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 4 of the design recipe has not been successfully completed. The following word contains nonterminals not in the language of the constructed grammar, ~s:"
                 grammar-name)
         invalid-nonterminals))

(define (anonymous-single-failure-grammar-invalid-nonterminal invalid-nonterminal)
  (format "Step 4 of the design recipe has not been successfully completed. The following word contains nonterminals not in the language of the constructed grammar: ~a"
          invalid-nonterminal))

(define (anonymous-multi-failure-grammar-invalid-nonterminal invalid-nonterminals)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 4 of the design recipe has not been successfully completed. The following word contains nonterminals not in the language of the constructed grammar:"
         invalid-nonterminals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR INVALID EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-invalid-expression grammar-name invalid-expression)
  (format "The constructed grammar, ~s, cannot testing the following word because it is invalid: ~a"
          grammar-name
          invalid-expression))

(define (named-multi-failure-grammar-invalid-expression grammar-name invalid-expressions)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "The constructed grammar, ~s, cannot testing the following word because it is invalid:"
                 grammar-name)
         invalid-expressions))

(define (anonymous-single-failure-grammar-invalid-expression invalid-expression)
  (format "The constructed grammar cannot testing the following word because it is invalid: ~a"
          invalid-expression))

(define (anonymous-multi-failure-grammar-invalid-expression invalid-expressions)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "The constructed grammar cannot testing the following word because it is invalid:"
         invalid-expressions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NO TEST CASES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (anonymous-no-test-cases fsm-val-type)
  (format "The unit-test for the constructed ~a does not contain any cases to test"
          fsm-val-type))

(define (named-no-test-cases fsm-val fsm-val-type)
  (format "The unit-test for the constructed ~a, ~s, does not contain any cases to test"
          fsm-val-type
          fsm-val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FAILURE STRING GENERATOR MACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (create-failure-str stx)
  (syntax-parse stx
    [(_ func-name (~var machine-name) invalid-words)
     #:with named-single-failure (format-id #'func-name "named-single-failure-~a" (syntax-e #'func-name))
     #:with named-multi-failure (format-id #'func-name "named-multi-failure-~a" (syntax-e #'func-name))
     #:with anonymous-single-failure (format-id #'func-name "anonymous-single-failure-~a" (syntax-e #'func-name))
     #:with anonymous-multi-failure (format-id #'func-name "anonymous-multi-failure-~a" (syntax-e #'func-name))
     #'(if (identifier? machine-name)
           (if (= (length invalid-words) 1)
               (named-single-failure (syntax-e machine-name) (first invalid-words))
               (named-multi-failure (syntax-e machine-name) invalid-words))
           (if (= (length invalid-words) 1)
               (anonymous-single-failure (first invalid-words))
               (anonymous-multi-failure invalid-words))
           )
     ]
    [(_ func-name (~var fsm-val))
     #:with named (format-id #'func-name "named-~a" (syntax-e #'func-name))
     #:with anonymous (format-id #'func-name "anonymous-~a" (syntax-e #'func-name))
     #'(if (identifier? #'fsm-val)
           (named (syntax-e #'fsm-val)
                  (whatami? fsm-val))
           (anonymous (whatami? fsm-val)))]))