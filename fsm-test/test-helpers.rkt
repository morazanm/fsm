#lang racket/base
(require (for-syntax syntax/stx syntax/parse racket/base) rackunit/text-ui "test-machine.rkt")

(provide test-all)

;; groups all test calls into one neat function
;; test-all :: symbol -> [symbols] -> code 
(define-syntax (test-all stx)
  (syntax-parse stx
    [(_ verbosity (val:id ...))
     #:with (calls ...) (stx-map (lambda (test-name)
                                   #`(define #,(gensym '_) (run-tests #,test-name verbosity)))
                                 #`(val ...))
     #`(begin
         calls ...)]))
