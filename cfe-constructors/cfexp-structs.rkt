#lang racket/base

(provide (all-defined-out))

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. variable
;; 5. concat 
;; 6. union
;; 7. kleene

(struct cfexp ([env #:mutable]) #:transparent)

;;hash ;;Purpose: To represent an empty environment
(define (empty-cfexp-env) (hash))

;;null-cfexp ;;Purpose: Only contains an environment
(struct mk-null-cfexp cfexp () #:transparent)

;;empty-cfexp ;;Purpose: A cfexp to represent the empty word
(struct mk-empty-cfexp cfexp () #:transparent)

;;singleton-cfexp ;;Purpose: A cfexp to represnt a single word
(struct mk-singleton-cfexp cfexp (char) #:transparent)

;;concat-cfexp ;;Purpose: A cfexp to represent the concatentation of cfexps
(struct mk-concat-cfexp cfexp (locfe) #:transparent)

;;union-cfexp ;;Purpose: A cfexp to represent the union of cfexps
(struct mk-union-cfexp cfexp (locfe) #:transparent)

;;Kleene-cfexp ;;Purpose: A cfexp to represent the Kleen of a cfexp
(struct mk-kleene-cfexp cfexp (cfe) #:transparent)

;;variable-cfexp ;;Purpose: A cfexp to represent a variable that needs to be substituted
(struct mk-var-cfexp cfexp (cfe) #:transparent)
