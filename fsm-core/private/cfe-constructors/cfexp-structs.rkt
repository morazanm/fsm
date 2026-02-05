#lang racket/base

(provide (all-defined-out))

;;a context-free expression is either:
;; 1. null (base case)
;; 2. empty (base case)
;; 3. singleton (base case)
;; 4. box
;; 5. concat 
;; 6. union
;; 7. kleene

;;null-cfexp ;;Purpose: Only contains an environment
(struct mk-null-cfexp () #:transparent)

;;empty-cfexp ;;Purpose: A cfexp to represent the empty word
(struct mk-empty-cfexp () #:transparent)

;;singleton-cfexp ;;Purpose: A cfexp to represnt a single word
(struct mk-singleton-cfexp (char) #:transparent)

;;concat-cfexp ;;Purpose: A cfexp to represent the concatentation of cfexps
(struct mk-concat-cfexp (locfe) #:transparent)

;;union-cfexp ;;Purpose: A cfexp to represent the union of cfexps
(struct mk-union-cfexp (locfe) #:transparent)

;;Kleene-cfexp ;;Purpose: A cfexp to represent the Kleen of a cfexp
(struct mk-kleene-cfexp (cfe) #:transparent)

;;X -> boolean
;;Purpose: Determines if the the given x is a cfexp
(define (cfexp? x)
  (or (mk-null-cfexp? x)
      (mk-empty-cfexp? x) 
      (mk-singleton-cfexp? x)
      (box? x) 
      (mk-concat-cfexp? x) 
      (mk-union-cfexp? x) 
      (mk-kleene-cfexp? x)))
