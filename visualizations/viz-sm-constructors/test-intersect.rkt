#lang racket
(require (for-syntax "../viz-lib/viz-constants.rkt"
                     syntax/parse
                     racket/base
                     racket/list
                     racket/struct-info
                     )
         "../viz-lib/viz-constants.rkt"
         "../viz-lib/zipper.rkt"
         racket/struct-info
         #; "../viz-lib/default-viz-function-generators.rkt")


(struct imsg (a c b))


(begin-for-syntax
  (define (make-pairs lst0 lst1) (if (or (empty? lst0) (empty? lst1))
                                   '()
                                   (cons (list (first lst0) (first lst1)) (make-pairs (rest lst0) (rest lst1)))
                                   )
  )
  )

(define-syntax (move-struct-fields-next-helper stx)
  (syntax-parse stx
    [(_ imsg-iden a-imsg (qoute ((fields accessors)...)))
     #'(struct-copy imsg-iden a-imsg
                    [fields (zipper-next (accessors a-imsg))]...
                    )
     ]
    )
  )



(define-syntax (move-struct-fields-next stx)
  (syntax-parse stx
    [(_ imsg-iden a-imsg)
     (define fields (struct-field-info-list (syntax-local-value #'imsg-iden)))
     (define accessors (fourth (extract-struct-info (syntax-local-value #'imsg-iden))))
     (define pairs #`'#,(make-pairs fields accessors))
     (with-syntax ([pairs #`'#,(make-pairs fields accessors)])
     #'(move-struct-fields-next-helper imsg-iden a-imsg pairs)
       )
     ]
    )
  )



(zipper-current (imsg-a(move-struct-fields-next imsg (imsg (list->zipper (list 'c 'd)) (list->zipper '(e f)) (list->zipper '(j k))))))
;(define test0 (imsg 'c 'd))

#;(extract-struct-info (syntax-local-value test0))