#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/list
         "fsm-type-predicates.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE ACCEPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-accept machine-name failed-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following words:"
                 machine-name)
         failed-words))

(define (named-single-failure-machine-accept machine-name failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following word:\n~a"
          machine-name
          failed-word))

(define (anonymous-multi-failure-machine-accept failed-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following words:"
         failed-words))

(define (anonymous-single-failure-machine-accept failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following word:\n~a"
          failed-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE REJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-reject machine-name failed-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following words:"
                 machine-name)
         failed-words))

(define (named-single-failure-machine-reject machine-name failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following word:\n~a"
          machine-name
          failed-word))

(define (anonymous-multi-failure-machine-reject failed-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following words:"
         failed-words))

(define (anonymous-single-failure-machine-reject failed-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following word:\n~a"
          failed-word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID NONTERMINAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-machine-invalid-nonterminal machine-name invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine, ~s:\n~a"
          machine-name
          invalid-word))

(define (named-multi-failure-machine-invalid-nonterminal machine-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine, ~s:"
                 machine-name)
         invalid-words))

(define (anonymous-single-failure-machine-invalid-nonterminal invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine:\n~a"
          invalid-word))

(define (anonymous-multi-failure-machine-invalid-nonterminal invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID WORD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-machine-invalid-word machine-name invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, is not a list of symbols in the machine's alphabet:\n~a"
          machine-name
          invalid-word))

(define (named-multi-failure-machine-invalid-word machine-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, are not a list of symbols in the machine's alphabet:"
                 machine-name)
         invalid-words))

(define (anonymous-single-failure-machine-invalid-word invalid-word)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case is not a list of symbols in the machine's alphabet:\n~a"
          invalid-word))

(define (anonymous-multi-failure-machine-invalid-word invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 2 of the design recipe has not been successfully completed. The following test cases are not a list of symbols in the machine's alphabet:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID ARITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-arity machine-name invalid-expressions)
  (void))

(define (named-single-failure-machine-invalid-arity machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, is not a pair consisting of a word and a head position:\n~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-arity invalid-expressions)
  (void))

(define (anonymous-single-failure-machine-invalid-arity invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case is not a pair consisting of a word and a head position:\n~a"
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID HEAD-POS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-head-pos machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a natural number:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-invalid-head-pos machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a natural number:\n~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-head-pos invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a natural number:"
          )
         invalid-expressions))

(define (anonymous-single-failure-machine-invalid-head-pos invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a natural number:\n~a"
          
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE INVALID HEAD-POS INDEX;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-invalid-head-pos-index machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a valid index into their respective test cases:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-invalid-head-pos-index machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a valid index into it's test case:\n~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-invalid-head-pos-index invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a valid index into their respective test cases:"
          )
         invalid-expressions))

(define (anonymous-single-failure-machine-invalid-head-pos-index invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a valid index into it's test case:\n~a"
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MACHINE NO LEFT HAND MARKER WORDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-multi-failure-machine-no-left-hand-marker machine-name invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, do not contain a left hand marker:"
          machine-name)
         invalid-expressions))

(define (named-single-failure-machine-no-left-hand-marker machine-name invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, does not contain a left hand marker:\n~a"
          machine-name
          invalid-expression))

(define (anonymous-multi-failure-machine-no-left-hand-marker invalid-expressions)
  (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
         (format "Step 2 of the design recipe has not been successfully completed. The following test cases do not contain a left hand marker:")
         invalid-expressions))

(define (anonymous-single-failure-machine-no-left-hand-marker invalid-expression)
  (format "Step 2 of the design recipe has not been successfully completed. The following test case does not contain a left hand marker:\n~a"
          invalid-expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR ACCEPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-accept grammar-name invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following word:\n~a"
          grammar-name
          invalid-word))

(define (named-multi-failure-grammar-accept grammar-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar, ~s, does not derive the following words:"
                 grammar-name)
         invalid-words))

(define (anonymous-single-failure-grammar-accept invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following word:\n~a"
          invalid-word))

(define (anonymous-multi-failure-grammar-accept invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed grammar does not derive the following words:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR REJECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-reject grammar-name invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, derives the following word:\n~a"
          grammar-name
          invalid-word))

(define (named-multi-failure-grammar-reject grammar-name invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, derives the following words:"
                 grammar-name)
         invalid-words))

(define (anonymous-single-failure-grammar-reject invalid-word)
  (format "Step 6 of the design recipe has not been successfully completed. The constructed machine derives the following word:\n~a"
          invalid-word))

(define (anonymous-multi-failure-grammar-reject invalid-words)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~s" val)))
         "Step 6 of the design recipe has not been successfully completed. The constructed machine derives the following words:"
         invalid-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR INVALID NONTERMINALS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-invalid-nonterminal grammar-name invalid-nonterminal)
  (format "Step 4 of the design recipe has not been successfully completed. The following word contains terminals not in the constructed grammar, ~s's alphabet :\n~a"
          grammar-name
          invalid-nonterminal))

(define (named-multi-failure-grammar-invalid-nonterminal grammar-name invalid-nonterminals)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 4 of the design recipe has not been successfully completed. The following words contain terminals not in the constructed grammar, ~s's alphabet:"
                 grammar-name)
         invalid-nonterminals))

(define (anonymous-single-failure-grammar-invalid-nonterminal invalid-nonterminal)
  (format "Step 4 of the design recipe has not been successfully completed. The following word contains terminals not in the constructed grammar's alphabet :\n~a"
          invalid-nonterminal))

(define (anonymous-multi-failure-grammar-invalid-nonterminal invalid-nonterminals)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 4 of the design recipe has not been successfully completed. The following words contain terminals not in the constructed grammar's alphabet:"
         invalid-nonterminals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GRAMMAR INVALID EXPRESSIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (named-single-failure-grammar-invalid-expression grammar-name invalid-expression)
  (format "Step 4 of the design recipe has not been successfully completed. The following test case for the constructed grammar, ~s, is not a word:\n~a"
          grammar-name
          invalid-expression))

(define (named-multi-failure-grammar-invalid-expression grammar-name invalid-expressions)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         (format "Step 4 of the design recipe has not been successfully completed. The following test cases for the constructed grammar, ~s, are not words:"
                 grammar-name)
         invalid-expressions))

(define (anonymous-single-failure-grammar-invalid-expression invalid-expression)
  (format "Step 4 of the design recipe has not been successfully completed. The following test case for the constructed grammar is not a word:\n~a"
          invalid-expression))

(define (anonymous-multi-failure-grammar-invalid-expression invalid-expressions)
  (foldl (lambda (val accum)
           (string-append accum (format "\n~a" val)))
         "Step 4 of the design recipe has not been successfully completed. The following test cases for the constructed grammar are not words:"
         invalid-expressions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NO TEST CASES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (anonymous-no-test-cases fsm-val-type)
  (if (equal? fsm-val-type 'grammar)
      (format "Step 4 of the design recipe has not been successfully completed. The unit-test for the constructed ~a does not contain any cases to test."
          fsm-val-type)
      (format "Step 2 of the design recipe has not been successfully completed. The unit-test for the constructed ~a does not contain any cases to test."
          fsm-val-type)))

(define (named-no-test-cases fsm-val fsm-val-type)
  (if (equal? fsm-val-type 'grammar)
      (format "Step 4 of the design recipe has not been successfuly completed. The unit-test for the constructed ~a, ~s, does not contain any cases to test."
          fsm-val-type
          fsm-val)
      (format "Step 2 of the design recipe has not been successfuly completed. The unit-test for the constructed ~a, ~s, does not contain any cases to test"
          fsm-val-type
          fsm-val)
      ))

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