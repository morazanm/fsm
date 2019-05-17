(module regexp-predicate racket
  (require "regexp.rkt")
  (provide valid-regexp?)

  ;valid-regexp?: string --> boolean/listof errors
  (define (valid-regexp? re)
    (local [(define the-string (printable-regexp re))

            (define (format-error the-list)
              (if (null? the-list)
                  (local [(define new-list (flatten the-list))
                          (define x-list (filter (lambda (x) (not (boolean? x))) new-list))
                          (define (build-error a-list)
                            (if (null? a-list) " "
                                (string-append (car a-list)
                                               (string-append "
"
                                                              (build-error (cdr a-list))))))]
                    (build-error x-list))
                  #t))
            
            ;flatten: listof lists --> listof element
            ;purpose: returns list of symbols in a list
            (define (flatten lst)
              (cond [(empty? lst) '()]
                    [(list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst)))]
                    [else (cons (car lst) (flatten (cdr lst)))]))
          
            ;check-singleton: string --> boolean/string
            (define (check-singleton given-regexp)
              (local [(define given-char (string-ref given-regexp 0))
                      (define length-check (equal? (string-length given-regexp) 1))
                      (define char-check (or (char-numeric? given-char)
                                             (char-alphabetic? given-char)))
                    
                      (define exclusion-check (or (equal? #\U given-char)
                                                  (equal? #\e given-char)))]
                (cond [(and length-check
                            (and char-check
                                 (not exclusion-check))) #t]
                      [length-check (format "the input string ~s to the regexp ~s must be a letter or number exluding U and e" given-regexp re)]
                      [char-check (format "the length of the input string ~s to the regexp ~s must be of length 1" given-regexp re)]
                      [else (format "the input string ~s for the regexp ~s must be a single alphabetical or numeric string exluding U and e" given-regexp re)])
                )
              )
          
            ;check-regexp: regexp --> boolean/listof strings
            (define (check-regexp proposed)
              (cond [(empty-regexp? proposed) empty]
                    [(singleton-regexp? proposed) (filter-errors (list (check-singleton the-string)))]
                    [(union-regexp? proposed) (filter-errors (flatten (list (valid-regexp? (union-regexp-r1 proposed))
                                                                            (valid-regexp? (union-regexp-r2 proposed)))))]
                    [(concat-regexp? proposed) (filter-errors (flatten (list (valid-regexp? (concat-regexp-r1 proposed))
                                                                             (valid-regexp? (concat-regexp-r2 proposed)))))]
                    [(kleenestar-regexp? proposed) (filter-errors (flatten (list (valid-regexp? (kleenestar-regexp-r1 proposed)))))]))
          
            ;filter-errors: listof strings --> boolean/listof strings
            (define (filter-errors given-list)
              (local [(define error-list (filter (lambda (x) (not (equal? x #true))) given-list))]
                (cond [(empty? error-list) #true]
                      [else error-list])))
            ]
      (cond [(string? the-string) (format-error (check-regexp re))]
            [else (format "the input to the regexp ~s must be a string" re)
                  ])
      )
    )
  )
