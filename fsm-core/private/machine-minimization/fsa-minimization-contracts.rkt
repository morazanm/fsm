#lang racket/base

(require
  "../macros/shared/shared-predicates.rkt"
  "../macros/error-formatting.rkt"
  racket/contract/base
  racket/contract/combinator)

(provide minimization/c
         minimization-viz/c)


;;string string -> string
;;Purpose: Formats the error messages for the optional arguments for minimization-viz
(define (keyword-arg-formatter arg-name arg-type)
  (format "The keyword argument, #:~a, expects a ~a as input, given" arg-name arg-type))

;;X -> boolean
;;Purpose: Determines if the given X is one of the colorblind options
(define (valid-palette-input? x)
  (or (eq? x 'default)
      (eq? x 'prot)
      (eq? x 'deut)
      (eq? x 'trit)))


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

;;string -> contract
;;Purpose: A contract that checks if the input is a dfa or ndfa
(define (valid-minimization-input func-name)
  (make-flat-contract
   #:name  'valid-minimization-input
   #:first-order procedure?
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
                           (format "~a expects as input a dfa or ndfa, given" func-name)))))))))

;;Purpose: A contract to check that minimize-dfa only works on ndfas and dfas
(define minimization/c (-> (valid-minimization-input "minimize-dfa")
                            dfa?))

;;Purpose: A contract to check that minimization-viz only works on ndfas and dfas
(define minimization-viz/c (->* ((valid-minimization-input "minimization-viz"))
                                (#:palette (valid-palette-input/c "palette" "symbol [prot, deut or trit]"))
                                void?))
