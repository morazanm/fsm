#lang racket/base

(require syntax/parse
         (for-template racket/base)
         (for-syntax racket/base))

(provide quasiquoted-tm-word
         quoted-tm-word
         list-tm-word
         lst-builders)

(define-literal-set lst-builders
  #:for-syntax
  (quote quasiquote list))

(define-syntax-class quasiquoted-tm-word
  #:literal-sets (lst-builders)
  (pattern (quasiquote ((~var word0 expr) (~var head-pos0 expr)))
    #:with word (syntax/loc #'word0 (quasiquote word0))
    #:with head-pos #'head-pos0))
  
(define-syntax-class quoted-tm-word
  #:literal-sets (lst-builders)
  
  (pattern (quote ((~var word0 expr) (~var head-pos0 expr)))
    #:with word (syntax/loc #'word0 (quote word0))
    #:with head-pos #'head-pos0))
  
(define-syntax-class list-tm-word
  #:literal-sets (lst-builders)
  (pattern (list (~var word0 expr) (~var head-pos0 expr))
    #:with word (syntax/loc #'word0 word0)
    #:with head-pos #'head-pos0))