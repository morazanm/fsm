#lang racket/base

(require "regexp.rkt"
         "macros/regexp-contracts.rkt"
         racket/contract/region)

(provide singleton-regexp
         concat-regexp
         union-regexp
         kleenestar-regexp)
;; singleton-regexp: string --> singleton-regexp
;; purpose: Given a lowercase Roman alphabet character, constructs a singleton
;;          regular expression for that letter.
(define/contract (singleton-regexp a)
  singleton-regexp/c
  (make-unchecked-singleton a))

;; concat-regexp: regexp regexp --> concat-regexp
;; purpose: Constructs the regular expression whose language contains words
;;          that are the concatenation of one word from L(a) and a word from L(b).
(define/contract (concat-regexp
                  a
                  b
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #t)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  concat-regexp/c
  (make-unchecked-concat a b))

;; union-regexp: regexp regexp --> union-regexp
;; purpose: Constructs a regular expression whose language contains all the words
;;          from L(a) and L(b).
(define/contract (union-regexp
                  a
                  b
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #true)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  union-regexp/c
  (make-unchecked-union a b)
  )

;; kleenestar-regexp: regexp --> kleenestar-regexp
;; purpose: Constructs a regular expression who language contains any word
;;          that is constructed by concatenating zero or more words from L(a).
(define/contract (kleenestar-regexp
                  a
                  #:sigma [sigma '()]
                  #:pred [pred (lambda (x) #true)]
                  #:gen-cases [gen-cases 10]
                  #:in-lang [in-lang '()]
                  #:not-in-lang [not-in-lang '()]
                  )
  kleenestar-regexp/c
  (make-unchecked-kleenestar a))
