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
  (check-expect
   (singleton-regexp-a (singleton-regexp "A"))
   "A")
  (check-error
   (singleton-regexp "abc")
   "Step five of the design recipe for regular expressions has not been successfully completed.
The argument to singleton-regexp must be a single lowercase Roman alphabet string, but found: \"abc\"")
  (check-error
   (singleton-regexp 5)
   "Step five of the design recipe for regular expressions has not been successfully completed.
The argument to singleton-regexp must be a single lowercase Roman alphabet string, but found: 5")

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
   "Step five of the design recipe for regular expressions has not been successfully completed.
The first argument to concat-regexp must be a regular expression, but found: \"a\"")
  (check-error
   (concat-regexp singleton-a 7)
   "Step five of the design recipe for regular expressions has not been successfully completed.
The second argument to concat-regexp must be a regular expression, but found: 7")
  (check-error
   (concat-regexp singleton-a singleton-b #:in-lang '((a b) 5))
   "Step four of the design recipe for regular expressions has not been successfully completed.
The expected list of words generated by the regular expression is not a valid list of words: ((a b) 5)")
  (check-error
   (concat-regexp singleton-a singleton-b #:sigma 5)
   "Step one of the design recipe for regular expressions has not been successfully completed.
The given regexp alphabet must be a list: 5")
  (check-error
   (concat-regexp singleton-a singleton-b #:sigma '(a b b))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following values, (b), are duplicated in the given sigma: (a b b)")
  (check-error
   (concat-regexp singleton-a singleton-b #:sigma '(a BB))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following: (BB) are not valid alphanumeric symbols in the given input alphabet: (a BB)")
  (check-error
   (concat-regexp singleton-a singleton-b #:sigma '(a c))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following singletons: (b) are found in words generated by the regular expression, but not in the specified alphabet: (a c)")
  (check-expect
   (concat-regexp-r1 (concat-regexp singleton-a singleton-b #:sigma '(a b)))
   (singleton-regexp "a"))
  (check-error
   (concat-regexp singleton-a singleton-b #:in-lang '((a a) (a b)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words are expected to be generated by the constructed concat-regexp but are not generated: ((a a))")
  (check-error
   (concat-regexp singleton-a singleton-b #:not-in-lang '((a a) (a b)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words are expected to not be generated by the constructed concat-regexp but are generated: ((a b))")
  (check-error
   (concat-regexp singleton-a singleton-b #:in-lang '((a b) (a c)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words to be generated by the regular expression: ((a c)) contain characters not in the regular expression's alphabet: (a b)")
  (check-error
   (concat-regexp singleton-a singleton-b #:not-in-lang '((a a) (d e)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words not to be generated by the regular expression: ((d e)) contain characters not in the regular expression's alphabet: (a b)")
  (check-expect
   (concat-regexp-r1 (concat-regexp
                      singleton-a
                      singleton-b
                      #:pred (lambda (word) (equal? word '(a b)))
                      #:in-lang '((a b))
                      #:not-in-lang '((a) (b))
                      #:sigma '(a b)))
   singleton-a)
  (check-error
   (concat-regexp singleton-a singleton-b #:pred (lambda (x) #f))
   "Step three of the design recipe for regular expressions has not been successfully completed.
The given predicate does not hold for the following words generated using the regexp: ((a b))")
  (check-error
   (concat-regexp singleton-a singleton-b #:pred (lambda (x) 5))
   "Step three of the design recipe for regular expressions has not been successfully completed.
Instead of returning a Boolean, the function given as a predicate returned: 5")
  (check-error
   (concat-regexp singleton-a singleton-b #:gen-cases 'a #:pred (lambda (x) #true))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The number of generated test cases to check with the predicate must be a positive integer, but found: a")
  

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
   "Step five of the design recipe for regular expressions has not been successfully completed.
The first argument to union-regexp must be a regular expression, but found: \"ab\"")
  (check-error
   (union-regexp concat-ab 8)
   "Step five of the design recipe for regular expressions has not been successfully completed.
The second argument to union-regexp must be a regular expression, but found: 8")
  (check-expect
   (union-regexp-r1 (union-regexp singleton-a singleton-b #:pred (lambda (word) (or (equal? word '(a)) (equal? word '(b))))))
   singleton-a)
  (check-error
   (union-regexp singleton-a singleton-b #:pred (lambda (x) 5))
   "Step three of the design recipe for regular expressions has not been successfully completed.
Instead of returning a Boolean, the function given as a predicate returned: 5")
  (check-error
   (union-regexp singleton-a singleton-a #:pred (lambda (x) #f))
   "Step three of the design recipe for regular expressions has not been successfully completed.
The given predicate does not hold for the following words generated using the regexp: ((a))")
  (check-error
   (union-regexp singleton-a singleton-b #:in-lang '((a) (b) (a b)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words are expected to be generated by the constructed union-regexp but are not generated: ((a b))")
  (check-error
   (union-regexp singleton-a singleton-b #:not-in-lang '((a b) (b)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words are expected to not be generated by the constructed union-regexp but are generated: ((b))")
  (check-expect
   (union-regexp-r1 (union-regexp singleton-a singleton-b #:in-lang '((a) (b))))
   singleton-a)
  (check-expect
   (union-regexp-r1 (union-regexp singleton-a singleton-b #:not-in-lang '((a b) (b a))))
   singleton-a)
  (check-error
   (union-regexp singleton-a singleton-b #:in-lang '((a) (b) (d e) (b c)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words to be generated by the regular expression: ((d e) (b c)) contain characters not in the regular expression's alphabet: (a b)")
  (check-error
   (union-regexp singleton-a singleton-b #:not-in-lang '((a) (b) (d e) (b c)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words not to be generated by the regular expression: ((d e) (b c)) contain characters not in the regular expression's alphabet: (a b)")
  (check-error
   (union-regexp singleton-a singleton-b #:sigma 5)
   "Step one of the design recipe for regular expressions has not been successfully completed.
The given regexp alphabet must be a list: 5")
  (check-error
   (union-regexp concat-ab singleton-c #:sigma '(a BB))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following: (BB) are not valid alphanumeric symbols in the given input alphabet: (a BB)")
  (check-error
   (union-regexp concat-ab singleton-c #:sigma '(a b b))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following values, (b), are duplicated in the given sigma: (a b b)")
  (check-error
   (union-regexp concat-ab singleton-c #:sigma '(a b))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following singletons: (c) are found in words generated by the regular expression, but not in the specified alphabet: (a b)")
  (check-expect
   (union-regexp-r1 (union-regexp concat-ab singleton-c #:sigma '(a b c)))
   concat-ab)
  (check-error
   (union-regexp singleton-a singleton-b #:gen-cases 'a #:pred (lambda (x) #true))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The number of generated test cases to check with the predicate must be a positive integer, but found: a")

  ;; kleenestar-regexp tests
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp singleton-a))
   singleton-a)
  (check-error
   (kleenestar-regexp (kleenestar-regexp "a"))
   "Step five of the design recipe for regular expressions has not been successfully completed.
The argument to kleenestar-regexp must be a regular expression, but found: \"a\"")
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp
                          singleton-a
                          #:pred (lambda (word) (or (equal? word EMP)
                                                    (and (list? word)
                                                         (andmap (lambda (x) (equal? x 'a)) word))))))
   singleton-a)
  (check-error
   (kleenestar-regexp singleton-a #:pred (lambda (x) 5))
   "Step three of the design recipe for regular expressions has not been successfully completed.
Instead of returning a Boolean, the function given as a predicate returned: 5")
  (check-error
   (kleenestar-regexp (empty-regexp) #:pred (lambda (x) #f))
   "Step three of the design recipe for regular expressions has not been successfully completed.
The given predicate does not hold for the following words generated using the regexp: (ε)")
  (check-error
   (kleenestar-regexp singleton-a #:not-in-lang '((a) (a a)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words are expected to not be generated by the constructed kleenestar-regexp but are generated: ((a) (a a))")
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp singleton-a #:in-lang '((a) (a a a a))))
   singleton-a)
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp concat-ab #:not-in-lang '((b) (b a b))))
   concat-ab)
  (check-error
   (kleenestar-regexp singleton-a #:in-lang '((a) (b) (d e) (b c)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words to be generated by the regular expression: ((b) (d e) (b c)) contain characters not in the regular expression's alphabet: (a)")
  (check-error
   (kleenestar-regexp singleton-a #:not-in-lang '((a) (b) (d e) (b c)))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The following words not to be generated by the regular expression: ((b) (d e) (b c)) contain characters not in the regular expression's alphabet: (a)")
  (check-error
   (kleenestar-regexp singleton-a #:sigma 5)
   "Step one of the design recipe for regular expressions has not been successfully completed.
The given regexp alphabet must be a list: 5")
  (check-error
   (kleenestar-regexp concat-ab #:sigma '(a BB))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following: (BB) are not valid alphanumeric symbols in the given input alphabet: (a BB)")
  (check-error
   (kleenestar-regexp concat-ab #:sigma '(a b b))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following values, (b), are duplicated in the given sigma: (a b b)")
  (check-error
   (kleenestar-regexp singleton-c #:sigma '(a b))
   "Step one of the design recipe for regular expressions has not been successfully completed.
The following singletons: (c) are found in words generated by the regular expression, but not in the specified alphabet: (a b)")
  (check-expect
   (kleenestar-regexp-r1 (kleenestar-regexp concat-ab #:sigma '(a b)))
   concat-ab)
  (check-error
   (kleenestar-regexp singleton-a #:gen-cases 'a #:pred (lambda (x) #true))
   "Step six of the design recipe for regular expressions has not been successfully completed.
The number of generated test cases to check with the predicate must be a positive integer, but found: a")

  
  

  (define ab*Uba* 
    (let* [(A       (singleton-regexp "a"))
           (B       (singleton-regexp "b"))
           (ASTAR   (kleenestar-regexp A))
           (BSTAR   (kleenestar-regexp B))
           (A-BSTAR (concat-regexp A BSTAR))
           (B-ASTAR (concat-regexp B ASTAR))
           (in-ab*Uba* 
            (lambda (w)
              (and (not (empty? w))
                   (or (and (eq? (first w) 'a) 
                            (andmap (lambda (s) (eq? s 'b)) (rest w)))
                       (and (eq? (first w) 'b) 
                            (andmap (lambda (s) (eq? s 'a)) (rest w)))))))]
      (union-regexp 
       A-BSTAR
       B-ASTAR
       #:pred        in-ab*Uba*
       #:gen-cases   3
       #:sigma       '(a b)
       #:in-lang     '((a) (b) (a b) (a b b b) (b a a a a))
       #:not-in-lang '(() (a a a) (b b) (b a a a a b) (a b b b a a a b a)))))

  (test)

  )
