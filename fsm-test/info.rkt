#lang info

(define compile-omit-paths 'all)

(define pkg-desc "FSM - Test Suite")

;; Add files you wish not to test here. More info at: https://docs.racket-lang.org/raco/test.html
(define test-omit-paths
  '("test-helpers.rkt"
    "test-machine.rkt"))

(define pkg-authors '(jschappel morazanm))