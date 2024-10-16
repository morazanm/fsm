(module constructors racket
  (require "../regexp.rkt"
           "error-formatting.rkt"
           racket/contract
           "../../private/sm-getters.rkt"
           "../../private/sm-apply.rkt"
           "../../private/fsa.rkt"
           "shared/shared-predicates.rkt"
           "shared/shared-flat-contracts.rkt"
           "validation/validation-flat-contracts.rkt"
           )
  (provide singleton-regexp/c
           concat-regexp/c
           union-regexp/c
           kleenestar-regexp/c)


  ;; valid-alpha-string?: any --> boolean
  ;; purpose: Returns true if the given input is a single alphabet character string,
  ;;          and false for every other input.
  (define (valid-alpha-string? x)
    (define regex-pattern (regexp "^[a-z]$"))
    (not (false? (and (string? x)
                      (regexp-match regex-pattern x)))))

  (define valid-singleton/c
    (make-flat-contract
     #:name 'valid-singleton-regexp?
     #:first-order valid-alpha-string?
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (if (valid-alpha-string? x)
                          x
                          (raise-blame-error
                           blame
                           x
                           (format "Step five of the design recipe for regular expressions has not been successfully completed.\nThe argument to singleton-regexp must be a single lowercase Roman alphabet character, but found")))))))

  ;; singleton-regexp/c
  (define singleton-regexp/c
    (->i ([a valid-singleton/c])
         [result singleton-regexp?]))

  ;; valid-regexp/c: string --> (any --> boolean or error)
  ;; Purpose: Constructs a flat contract for checking if an input value is a
  ;;          valid regular expression. Takes as input a string to provide
  ;;          details to the error message.
  (define (valid-regexp/c error-message)
    (make-flat-contract
     #:name 'valid-regexp?
     #:first-order regexp?
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (if (regexp? x)
                          x
                          (raise-blame-error
                           blame
                           x
                           (format "Step five of the design recipe for regular expressions has not been successfully completed.\n~a" error-message)))))))

  (define (regexp-matches-pred/c regexp)
    (make-flat-contract
     #:name 'regexp-matches-pred?
     #:projection (lambda (blame)
                    (lambda (predicate)
                      (current-blame-format format-error)
                      (if (predicate regexp)
                          predicate
                          (raise-blame-error blame predicate "Predicate did not match regexp"))))))

  ;; regexp (listof symbol) -> (listof symbol)
  ;; Returns the list of all singleton regexps in the input regexp (as symbols)
  ;; that are not in the input sigma.
  (define (find-extra-singleton-regexps regexp sigma)
    (let [(regexp-sigma (sm-sigma (regexp->fsa regexp)))]
      (filter (lambda (x) (not (member x sigma))) regexp-sigma)))

  (define (valid-regexp-sigma/c regexp)
    (make-flat-contract
     #:name 'valid-sigma?
     #:first-order (lambda (sigma) (empty? (find-extra-singleton-regexps regexp sigma)))
     #:projection (lambda (blame)
                    (lambda (sigma)
                      (current-blame-format format-error)
                      (let [(extra-singletons (find-extra-singleton-regexps regexp sigma))]
                        (if (empty? extra-singletons)
                            sigma
                            (raise-blame-error
                             blame
                             sigma
                             (format "Step one of the design recipe for regular expressions has not been successfully completed.\nThe following singletons: ~a are in the regexp, but not in the specified alphabet"
                                     extra-singletons))))
                      ))))

  ;; regexp (listof symbol) -> boolean
  ;; Returns true if all of the letters in the input word are part of the regular
  ;; expression's alphabet, and false otherwise.
  (define (word-in-regexp-sigma? regexp-sigma word)
    (empty? (filter (lambda (x) (not (member x regexp-sigma))) word)))

  (define (words-in-sigma/c type regexp)
    (let [(regexp-sigma (sm-sigma (regexp->fsa regexp)))]
      (make-flat-contract
       #:name 'words-in-sigma?
       #:first-order (lambda (words) (empty? (filter (lambda (word) (not (word-in-regexp-sigma? regexp-sigma word))) words)))
       #:projection (lambda (blame)
                      (lambda (words)
                        (current-blame-format format-error)
                        (let [(words-not-in-regexp-sigma (filter (lambda (word) (not (word-in-regexp-sigma? regexp-sigma word))) words))]
                          (if (empty? words-not-in-regexp-sigma)
                              words
                              (raise-blame-error
                               blame
                               regexp-sigma
                               (format "Step six of the design recipe for regular expressions has not been successfully completed.\nThe following words to ~a: ~a contain characters not in the regular expression's alphabet"
                                       type
                                       words-not-in-regexp-sigma)))))))))

  ; regexp (listof symbol) boolean -> boolean
  ; Checks if the given word matches the regexp. The boolean input determines
  ; if the word is expected to be accepted (#true) or rejected (#false) by
  ; the regexp.
  (define (check-word-accepts regexp word accepts?)
    (let [(result (sm-apply (regexp->fsa regexp) word))]
      (if accepts?
          (equal? result 'accept)
          (equal? result 'reject))))

  ; string boolean -> string
  ; Formats an error message for step six of the DR if the regexp incorrectly
  ; rejects/accepts a word.
  (define (accepts/rejects-formatter type accepts?)
    (format "Step six of the design recipe for regular expressions has not been successfully completed.\nThe constructed ~a does not ~a the following words"
            type
            (if accepts? "accept" "reject")))

  ; any -> boolean
  ; Determines if the given input is a word, which is a list of symbols.
  (define (valid-word? word)
    (and (list? word)
         (andmap (lambda (letter) (symbol? letter))
                 word)))

  (define valid-word/c
    (make-flat-contract
     #:name 'valid-word
     #:first-order valid-word?
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (if (valid-word? x)
                          x
                          (raise-blame-error
                           blame
                           x
                           (format "Step three of the design recipe for regular expressions has not been successfully completed.\nThe input to a regexp predicate should be a word, but found")))))))

  (define valid-regexp-predicate-result/c
    (make-flat-contract
     #:name 'valid-regexp-predicate-result
     #:first-order boolean?
     #:projection (lambda (blame)
                    (lambda (x)
                      (current-blame-format format-error)
                      (if (boolean? x)
                          x
                          (raise-blame-error
                           blame
                           x
                           (format "Step three of the design recipe for regular expressions has not been successfully completed.\nInstead of returning a Boolean, the function given as a predicate returned")))))))
  

  (define valid-regexp-predicate/c
    (->i ([input valid-word/c])
         [result valid-regexp-predicate-result/c]))

  (define (regexp-input/c regexp type accepts?)
    (make-flat-contract
     #:name 'regexp-accepting-correctly
     #:first-order (lambda (words) (empty? (filter (lambda (word) (not (check-word-accepts regexp word accepts?))) words)))
     #:projection (lambda (blame)
                    (lambda (words)
                      (current-blame-format format-error)
                      (let [(failing-words (filter (lambda (word) (not (check-word-accepts regexp word accepts?))) words))]
                        (if (empty? failing-words)
                            words
                            (raise-blame-error
                             blame
                             failing-words
                             (accepts/rejects-formatter type accepts?))))))))

  (define (predicate-passes/c regexp)
    (let [(words (remove-duplicates (build-list 25 (lambda (x) (gen-regexp-word regexp)))))]
      (make-flat-contract
       #:name 'predicate-passes
       #:first-order (lambda (predicate) (empty? (filter (lambda (word) (not (predicate word))) words)))
       #:projection (lambda (blame)
                      (lambda (predicate)
                        (current-blame-format format-error)
                        (let [(failing-words (filter (lambda (word) (not (predicate word))) words))]
                          (if (empty? failing-words)
                              predicate
                              (raise-blame-error
                               blame
                               failing-words
                               (format "Step three of the design recipe for regular expressions has not been successfully completed.\nThe given predicate does not hold for the following words generated using the regexp"))))
                        )))))

  

  ;; concat-regexp/c
  (define concat-regexp/c
    (->i ([r1 (valid-regexp/c "The first argument to concat-regexp must be a regular expression, but found")]
          [r2 (valid-regexp/c "The second argument to concat-regexp must be a regular expression, but found")])
         (#:pred [pred (r1 r2) (and/c
                                valid-regexp-predicate/c
                                (predicate-passes/c (make-unchecked-concat r1 r2)))]
          #:accepts [accepts (r1 r2) (and/c
                                      (listof-words-regexp/c "accepts" "four")
                                      (words-in-sigma/c "accept" (make-unchecked-concat r1 r2))
                                      (regexp-input/c (make-unchecked-concat r1 r2)
                                                      'concat-regexp
                                                      #t))]
          #:rejects [rejects (r1 r2) (and/c
                                      (listof-words-regexp/c "rejects" "four")
                                      (words-in-sigma/c "reject" (make-unchecked-concat r1 r2))
                                      (regexp-input/c (make-unchecked-concat r1 r2)
                                                      'concat-regexp
                                                      #f))]
          #:sigma [sigma (r1 r2) (and/c (is-a-list-regexp/c "regexp alphabet" "one")
                                        (valid-listof/c valid-alpha? "lowercase alphabet letter" "input alphabet" #:rule "one" #:for-regexp? #true)
                                        (no-duplicates/c "sigma" "one" #:for-regexp? #true)
                                        (valid-regexp-sigma/c (make-unchecked-concat r1 r2)))])
         [result concat-regexp?]))

  ;; union-regexp/c
  (define union-regexp/c
    (->i ([r1 (valid-regexp/c "The first argument to union-regexp must be a regular expression, but found")]
          [r2 (valid-regexp/c "The second argument to union-regexp must be a regular expression, but found")])
         [result union-regexp?]))

  ;; kleenestar-regexp/c
  (define kleenestar-regexp/c
    (->i ([r1 (valid-regexp/c "The argument to kleenestar-regexp must be a regular expression, but found")])
         [result kleenestar-regexp?]))
  )
