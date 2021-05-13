#lang racket
(require
  (for-syntax racket/syntax syntax/stx syntax/parse racket/list racket/string syntax/to-string racket/match))
#|  Adapter pattern Macro. Written by Joshua Schappel 5/13/21
See Macro readme for exact transformation and basic usage.
|#

(provide adapter)
(begin-for-syntax
  ;; transform each variable that contains a ?
  (define (trans-case stx-list)
    (define (helper stx-list patts guards)
      (match stx-list
        ['() (list patts guards)]
        [`(,f ,r ...) #:when (symbol? (syntax->datum f))
                      (let [(last-letter (last ((compose string->list symbol->string syntax->datum) f)))
                            (sym (gensym "a-"))]
                        (if (equal? last-letter #\?)
                            (helper r (cons #`#,sym patts) (cons #`(#,f #,sym)  guards))
                            (helper r (cons f patts) guards)))]
        [`(,f ,r ...) (helper r (cons f patts) guards)]))
    (helper stx-list '() '()))

  ;; transform each row of the pattern
  (define (trans-row stx-patt stx-func)
    (syntax-parse stx-patt
      [((_ ...)...) (let* [(f (stx-map (λ (inner) (trans-case (syntax->list inner))) stx-patt))
                           (g (flatten (map (λ (x) (cadr x)) f)))
                           (t (map (λ (x) #`(list #,@(car x))) f))]
                      #`[(list #,@t) #:when (and #,@g) (map #,stx-func data)])]
      [(_ ...) (match-let [(`(,p ,g) (trans-case (syntax->list stx-patt)))]
                 #`[(list #,@p) #:when (and #,@g) (map #,stx-func data)])])))

(define-syntax (adapter stx)
  (define-syntax-class lowc
    (pattern (single:expr ...))
    (pattern ((nested:expr ...) ...)))
  (define-syntax-class branch
    #:datum-literals (<-)
    (pattern (left:lowc <- right:id)))
  (syntax-parse stx
    [(_ adapter-name:id a-case:branch ...)
     #:with adapt-fn-name (format-id #'adapter-name "~a-adapter" #'adapter-name)
     #:with (conds ...) (stx-map (lambda (l r) (trans-row l r))
                                 #`(a-case.left ...) #`(a-case.right ...))
     #`(define (adapt-fn-name data)
         (match (car data)
           conds ...
           [else (error (format "Invalid pattern supplied to function ~s"
                                (string adapt-fn-name)))]))]))
