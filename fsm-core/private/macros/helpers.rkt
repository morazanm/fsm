(module helpers racket
  (require
    "../constants.rkt"
    )
  (provide
   invalid-finals
   return-duplicates
   )

  (define (invalid-finals states finals)
    (filter (lambda (x) (not (member x states)))finals))

  (define (return-duplicates los)
    (cond [(empty? los) '()]
          [(member (car los) (cdr los)) (cons (car los) (return-duplicates (cdr los)))]
          [else (return-duplicates (cdr los))]))

  )
