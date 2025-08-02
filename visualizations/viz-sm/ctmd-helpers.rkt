#lang racket/base

(require (for-syntax syntax/parse racket/base
                     racket/list
                     )
         syntax/parse
         racket/list)
(provide combine-tms-prepass/1
         #;combine-tms-prepass/2)
(define-syntax (combine-tms-prepass/1 stx)
  (syntax-parse stx
    [((~literal list)
         e:expr ...)
     #``(,@(combine-tms-prepass/1 e) ...)]
    [((~literal cons)
         e ...)
     #'`(,@(combine-tms-prepass/1 e) ...)]
    [((~literal quote) (e ...))
        #'`(,@(combine-tms-prepass/1 e) ...)]
    
    [n:number
     #''(n)]
    [e:expr
     #''(#f)]))

(define-syntax (prepass stx)
  (syntax-parse stx
    [(_ elem)
     (let-values ([(res o-res) (syntax-local-expand-expression #'elem)])
       #`'(#,(combine-tms-prepass/1 res)))]))

#;(displayln (syntax-local-expand-expression res))
#;(displayln (syntax->datum (combine-tms-prepass/1 #'(list 1 '(2 a) 'b))))

#;(define (combine-tms-prepass/3 stx)
  (displayln stx)
  (let ([res (syntax-parse stx
    [(_ n:number)
     #''(n)]
    [(_ ((~datum cons)
         e ...))
     #'`((combine-tms-prepass/1 e) ...)
     
     ]
    [(_ ((~datum quote) (e ...)))
        #'`((combine-tms-prepass/1 e) ...)]
    [(_ ((~datum list)
         e:expr ...))
     #'`((combine-tms-prepass/1 e) ...)]
    
    [(_ e:expr)
     #''(#f)])])
    (displayln res)
    res))

#;(define-syntax (combine-tms-prepass/2 stx)
  (let ([res (datum->syntax stx (filter (lambda (elem) (begin
                                                         
                                                         (number? (syntax-e elem))
                                                         )) (syntax->list stx)))])
    
    res))