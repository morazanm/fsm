(module grammar-getters racket
  (require "cfg.rkt" "csg.rkt" "regular-grammar.rkt")
  (provide grammar-getnts
           grammar-getalphabet
           grammar-getrules
           grammar-getstart
           grammar-gettype)


   (define (grammar-getnts g)
    (cond [(rg? g) (rg-getnts g)]
          [(cfg? g) (cfg-get-v g)]
          [(csg? g) (csg-getv g)]))
  
  (define (grammar-getalphabet g)
    (cond [(rg? g) (rg-getalphabet g)]
          [(cfg? g) (cfg-get-alphabet g)]
          [(csg? g) (csg-getsigma g)]))
  
  (define (grammar-getrules g)
    (cond [(rg? g) (rg-getunparsedrules g)]
          [(cfg? g) (cfg-get-rules g)]
          [(csg? g) (csg-get-unparsed-rules g)]))
  
  (define (grammar-getstart g)
    (cond [(rg? g) (rg-getstart g)]
          [(cfg? g) (cfg-get-start g)]
          [(csg? g) (csg-getstart g)]))
  
  (define (grammar-gettype g)
    (cond [(rg? g) 'rg]
          [(cfg? g) 'cfg]
          [(csg? g) 'csg]
          [else (error (format "In grammar-gettype: Unknown grammar type."))]))
  )