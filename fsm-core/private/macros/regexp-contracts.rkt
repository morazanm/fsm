(module constructors racket
  (require "../regexp.rkt"
           "error-formatting.rkt"
           racket/contract
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
    (and (string? x)
         (not (not (regexp-match regex-pattern x)))))

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
                           (format "A singleton-regexp must be a single lowercase Roman alphabet character, but found")))))))

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
                          (raise-blame-error blame x error-message))))))

  ;; concat-regexp/c
  (define concat-regexp/c
    (->i ([r1 (valid-regexp/c "The first input to a concat-regexp must be a valid regular expression, but found")]
          [r2 (valid-regexp/c "The second input to a concat-regexp must be a valid regular expression, but found")])
         [result concat-regexp?]))

  ;; union-regexp/c
  (define union-regexp/c
    (->i ([r1 (valid-regexp/c "The first input to a union-regexp must be a valid regular expression, but found")]
          [r2 (valid-regexp/c "The second input to a union-regexp must be a valid regular expression, but found")])
         [result union-regexp?]))

  ;; kleenestar-regexp/c
  (define kleenestar-regexp/c
    (->i ([r1 (valid-regexp/c "The input to a kleenestar-regexp must be a valid regular expression, but found")])
        [result kleenestar-regexp?]))
  )
