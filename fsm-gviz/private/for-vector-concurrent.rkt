#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/for-body
                     racket/syntax)
         racket/promise
         racket/vector)
(provide for/vector/concurrent)

(define-syntax (for/vector/concurrent stx)
  (syntax-parse stx
    [(_ #:length len (clauses ...) body ...)
     (with-syntax
         ([((pre-body ...)
            (post-body ...))
           (split-for-body stx #'(body ...))]
          [idx-id (generate-temporary 'idx)])
       #'(let ([promises (make-vector len)])
           (for (clauses ...
                 [idx-id (in-naturals)])
             pre-body ...
             (vector-set! promises
                          idx-id
                          (delay/thread
                           (let () post-body ...))))
           (vector-map! force promises)
           promises))]))

#;(define-syntax (for/vector/concurrent stx)
    (syntax-parse stx
      [(_ #:length len (clauses ...) body ...)
       (with-syntax
           ([((pre-body ...)
              (post-body ...))
             (split-for-body stx #'(body ...))]
            [idx-id (generate-temporary 'idx)])
         #'(for/fold/derived stx
             ([promises (make-vector len)]
              #:result (vector-map! force promises))
             (clauses ...
              [idx-id (in-naturals)])
             pre-body ...
             (begin
               (vector-set! promises
                            idx-id
                            (delay/thread
                             (let () post-body ...)))
               promises)))]))