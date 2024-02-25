(module regexp-tests racket
  (require "../../../main.rkt"
           racket/contract
           )
  (local-require test-engine/racket-tests)
  
  (provide 
   )

  ;; singleton-regexp tests
  (check-expect
   (singleton-regexp-a (singleton-regexp "a"))
   "a")
  (check-error
   (singleton-regexp "abc")
   "The argument to a singleton-regexp must be a single lowercase Roman alphabet character, but found: \"abc\"")
  (check-error
   (singleton-regexp 5)
   "The argument to a singleton-regexp must be a single lowercase Roman alphabet character, but found: 5")
  (check-error
   (singleton-regexp "A")
   "The argument to a singleton-regexp must be a single lowercase Roman alphabet character, but found: \"A\"")
  (check-error
   (singleton-regexp "*")
   "The argument to a singleton-regexp must be a single lowercase Roman alphabet character, but found: \"*\"")

  (define singleton-a (singleton-regexp "a"))
  (define singleton-b (singleton-regexp "b"))
  (define singleton-c (singleton-regexp "c"))

  ;;concat-regexp tests
  (check-expect
   (concat-regexp-r1 (concat-regexp singleton-a singleton-b))
   singleton-a)
  (check-expect
   (concat-regexp-r2 (concat-regexp singleton-a singleton-b))
   singleton-b)
  (check-error
   (concat-regexp "a" singleton-b)
   "The first argument to concat-regexp must be a regular expression, but found: \"a\"")
  (check-error
   (concat-regexp singleton-a 7)
   "The second argument to concat-regexp must be a regular expression, but found: 7")

  (define concat-ab (concat-regexp singleton-a singleton-b))

  ;; union-regexp tests
  (check-expect
   (union-regexp-r1 (union-regexp concat-ab singleton-c))
   concat-ab)
  (check-expect
   (union-regexp-r2 (union-regexp concat-ab singleton-c))
   singleton-c)
  (check-error
   (union-regexp "ab" singleton-c)
   "The first argument to union-regexp must be a regular expression, but found: \"ab\"")
  (check-error
   (union-regexp concat-ab 8)
   "The second argument to union-regexp must be a regular expression, but found: 8")

  ;; kleenestar-regexp tests
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp singleton-a))
   singleton-a)
  (check-error
   (kleenestar-regexp (kleenestar-regexp "a"))
   "The argument to a kleenestar-regexp must be a regular expression, but found: \"a\"")
  
  (test)

  )
