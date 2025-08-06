#lang racket/base
(require "base-fsm-error-types.rkt"
         "../syntax-value-struct.rkt")

(provide warn:fsm:app:tm?
         (struct-out warn:fsm:app:tm:invalid-arity)
         (struct-out warn:fsm:app:tm:invalid-head-pos)
         (struct-out warn:fsm:app:tm:invalid-head-pos-index)
         (struct-out warn:fsm:app:tm:no-left-hand-marker)
         (struct-out warn:fsm:app:tm:invalid-word)
         (struct-out warn:fsm:app:tm:invalid-nt)
         (struct-out warn:fsm:app:tm:accept)
         (struct-out warn:fsm:app:tm:reject))

(struct warn:fsm:app:tm warn:fsm:app ())

(struct warn:fsm:app:tm:invalid-arity warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (named-single-failure err-struct))

   (define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, is not a pair consisting of a word and a head position:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (anonymous-single-failure err-struct))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case is not a pair consisting of a word and a head position:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:tm:invalid-head-pos warn:fsm:app:tm (str-fmt)
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a natural number:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            ((warn:fsm:app:tm:invalid-head-pos-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct))))

   (define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a natural number:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (car ((warn:fsm:app:tm:invalid-head-pos-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a natural number:")
            ((warn:fsm:app:tm:invalid-head-pos-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a natural number:\n~a"
             (car ((warn:fsm:app:tm:invalid-head-pos-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:tm:invalid-head-pos-index warn:fsm:app:tm (str-fmt)
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, have a starting head position which is not a valid index into their respective test cases:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            ((warn:fsm:app:tm:invalid-head-pos-index-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct))))

   (define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, has a starting head position which is not a valid index into it's test case:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (car ((warn:fsm:app:tm:invalid-head-pos-index-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases have a starting head position which is not a valid index into their respective test cases:")
            ((warn:fsm:app:tm:invalid-head-pos-index-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case has a starting head position which is not a valid index into it's test case:\n~a"
             (car ((warn:fsm:app:tm:invalid-head-pos-index-str-fmt err-struct) (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:tm:no-left-hand-marker warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases for the constructed machine, ~s, do not contain a left hand marker:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, does not contain a left hand marker:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum) (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following test cases do not contain a left hand marker:")
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case does not contain a left hand marker:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:tm:invalid-word warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case for the constructed machine, ~s, is not a list of symbols in the machine's alphabet:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (named-single-failure err-struct))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following test case is not a list of symbols in the machine's alphabet:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (anonymous-single-failure err-struct))])

(struct warn:fsm:app:tm:invalid-nt warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine, ~s:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine, ~s:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 2 of the design recipe has not been successfully completed. The following word contains elements not in the language of the constructed machine:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 2 of the design recipe has not been successfully completed. The following words contain elements not in the language of the constructed machine:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))])

(struct warn:fsm:app:tm:accept warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not accept the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))
   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))
   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not accept the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])

(struct warn:fsm:app:tm:reject warn:fsm:app:tm ()
  #:methods gen:printable-fsm-err-vals
  [(define (named-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following words:"
                    (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct))))
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (named-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine, ~s, does not reject the following word:\n~a"
             (syntax-e (val-stx-pair-stx (warn:fsm-fsm-expr err-struct)))
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))

   (define (anonymous-multi-failure err-struct)
     (foldl (lambda (val accum)
              (string-append accum (format "\n~a" val)))
            "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following words:"
            (map val-stx-pair-val (warn:fsm:app-err-exprs err-struct))))

   (define (anonymous-single-failure err-struct)
     (format "Step 6 of the design recipe has not been successfully completed. The constructed machine does not reject the following word:\n~a"
             (val-stx-pair-val (car (warn:fsm:app-err-exprs err-struct)))))])