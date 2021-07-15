#lang racket
(require
  (for-syntax racket/syntax syntax/stx syntax/parse racket/list racket/string syntax/to-string racket/match))
#|  Adapter pattern Macro. Written by Joshua Schappel 5/13/21
See Macro readme for exact transformation and basic usage.
|#
(provide adapter)

(begin-for-syntax
  ;; trans-case :: [syntax] -> [ [idents] [guards] ]
  ;; If the ident is a _ then, we leave it, otherwise it is a procedure, so
  ;; we transforme the procedure into an identifer for the match case and add the
  ;; procedure to the guard. For Example:
  ;;    input:  (_ symbol? _)
  ;;    output: (list (_ a-123123 _) (a-123123))
  (define (trans-case stx-list)
    (define (helper stx-list patts guards)
      (match stx-list
        ['() (list patts guards)]
        [`(,f ,r ...) #:when (symbol? (syntax->datum f))
                      (let [(sym (gensym "a-"))]
                        (if (eq? (syntax->datum f) '_)
                            (helper r (cons f patts) guards)
                            (helper r (cons #`#,sym patts) (cons #`(#,f #,sym)  guards))))]))
    (helper stx-list '() '()))

  ;; trans-row :: syntax -> syntax -> syntax-case
  ;; Converts one of the macro rows into the propper match case, where the 1st param is
  ;; the match case and the 2nd param is the procedure associated with the match case
  ;; For example:
  ;;   input: ((_ number? _) (_ symbol?))       pda-rule-to-string
  ;;   output: [(list (list _ a-1 _) (list a-2 _)) #:when (and (number? a-1) (symbol? a-2)) (map pda-rule-to-string data)]
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
    (pattern (~or (single:expr ...) (nested:lowc))))
  (define-syntax-class branch
    #:datum-literals (<-)
    (pattern (left:lowc <- right:id)))
  (syntax-parse stx
    [(_ adapter-name:id a-case:branch ...)
     #:with adapt-fn-name (format-id #'adapter-name "~a-adapter" #'adapter-name) ; function name for adapter
     #`(define (adapt-fn-name data)
         (match (car data)
           #,@(stx-map (lambda (l r) (trans-row l r)) ;; convert to racket match case
                       #`(a-case.left ...) #`(a-case.right ...))
           [else (error "Invalid pattern supplied to adapter")]))]))
