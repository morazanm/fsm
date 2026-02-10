#lang racket/base
(require "../../fsa.rkt"
         "../../tm.rkt"
         "../../pda.rkt"
         "../../sm-apply.rkt"
         "../../mtape-tm.rkt"
         "../shared/shared-predicates.rkt")
(provide listof-words?
         listof-words-tm?
         words-in-sigma?
         words-in-sigma-tm?
         invalid-words
         invalid-words-tm
         words-in-sigma-tm?
         invalid-words-tm
         acceptable-position?
         unacceptable-position
         check-input-dfa
         return-input-dfa
         check-input-ndfa
         return-input-ndfa
         check-input-ndpda
         return-input-ndpda
         check-input-tm
         return-input-tm
         check-input-mttm
         return-input-mttm
         check-input-grammar
         return-input-grammar
         )

;listof-words?: (listof something) --> boolean
;purpose: to check if the list of words is a valid list
; of list of symbols
(define (listof-words? words)
  (and (list? words)
       (andmap (lambda (word) (and (list? word)
                                   (andmap (lambda (letter) (valid-alpha? letter))
                                           word))) words))
  )

;listof-words-tm?: (listof something) --> boolean
;purpose: turing machine words are either:
;  a list of symbols
;  a pair where the first item is a list of symbols
;               the second item is a starting index
(define (listof-words-tm? words)
  (and (list words) 
       (andmap (lambda (word) (and (list? word)
                                   (or (andmap (lambda (letter) (symbol? letter))
                                               word)
                                       (and (equal? (length word) 2)
                                            (andmap (lambda (letter) (symbol? letter))
                                                    (car word))
                                            (and (integer? (cadr word))
                                                 (equal? (abs (cadr word))
                                                         (cadr word))))
                                       ))
                 )
               words))
  )

;words-in-sigma?: (listof words) sigma --> boolean
;purpose: to return if the words are made of character from sigma
(define (words-in-sigma? words sigma)
  (if (andmap (lambda (word) (andmap (lambda (letter) (member letter sigma)) word)) words) #t #f))

;invalid-words: (listof words) sigma --> (listof words)
;purpose: to return if the words are made of character from sigma
(define (invalid-words words sigma)
  (filter (lambda (word) (ormap (lambda (letter) (not (member letter sigma))) word)) words))

;words-in-sigma?: (listof words) sigma --> boolean
;purpose: to return if the words are made of character from sigma
(define (words-in-sigma-tm? words sigma)
  (if (andmap (lambda (word) (or (andmap (lambda (letter) (member letter sigma)) word)
                                 (if (and (equal? (length word) 2)
                                          (integer? (cadr word))
                                          (list? (car word)))
                                     (andmap (lambda (letter) (member letter sigma)) (car word))
                                     #f)
                                 )
                ) words) #t #f))

;invalid-words-tm (listof words) sigma --> boolean
;purpose: to return the invalid turing machine words
(define (invalid-words-tm words sigma)
  (filter (lambda (word) (or (if (not (list? (car word)))
                                 (ormap (lambda (letter) (not (member letter sigma))) word)
                                 #f)
                             (if (and (equal? (length word) 2)
                                      (integer? (cadr word))
                                      (list? (car word)))
                                 (ormap (lambda (letter) (not (member letter sigma))) (car word))
                                 #f)
                             )
            ) words))

(define (acceptable-position? words)
  (andmap (lambda (x) (if (and (equal? 2 (length x))
                               (integer? (cadr x))
                               (list? (car x)))
                          (and (>= (cadr x) 0)
                               (< (cadr x) (length (car x))))
                          #t)
            )
          words))

(define (unacceptable-position words)
  (filter (lambda (x) (if (and (equal? 2 (length x))
                               (integer? (cadr x))
                               (list? (car x)))
                          (or (< (cadr x) 0)
                              (>= (cadr x) (length (car x))))
                          #f)
            )
          words)
  )

;check-input-dfa:
; (listof states) (listof alphabet) symbol (listof states) (listof dfa-rules) boolean symbol
; --> [(listof (listof symbols)) --> boolean]
;purpose: to take in all the components of a machine, and then a list
; of words to check in that machine, then run those words through the machine
; and make sure that they match the "accepts?" symbol, which will be either
; accept or reject. This ensures that this can be used for both accept lists
; and rejects lists.
(define (check-input-dfa states
                         sigma
                         start
                         finals
                         rules
                         add-dead
                         accepts?)
  (lambda (words)
    (define temp-machine (make-unchecked-dfa states
                                             sigma
                                             start
                                             finals
                                             rules
                                             add-dead))
    (andmap (lambda (x) (equal? (temp-machine x) (if accepts? 'accept 'reject))) words)
    )
  )

;return-input-dfa:
; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) boolean
; --> (listof (listof symbols))
;purpose: builds the machine and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
(define (return-input-dfa states
                          sigma
                          start
                          finals
                          rules
                          add-dead
                          words
                          accepts?)
  (define temp-machine (make-unchecked-dfa states
                                           sigma
                                           start
                                           finals
                                           rules
                                           add-dead))
  (filter (lambda (x)
            (equal?
             (temp-machine x)
             (if accepts? 'reject 'accept)))
          words))

;check-input-ndfa
; (listof states) (listof alphabet) symbol (listof states) (listof ndfa-rules) boolean symbol
; --> [(listof (listof symbols)) --> boolean]
;purpose: to take in all the components of a machine, and then a list
; of words to check in that machine, then run those words through the machine
; and make sure that they match the "accepts?" symbol, which will be either
; accept or reject. This ensures that this can be used for both accept lists
; and rejects lists.
(define (check-input-ndfa states
                          sigma
                          start
                          finals
                          rules
                          accepts?)
  (lambda (words)
    (define temp-machine (make-unchecked-ndfa states
                                              sigma
                                              start
                                              finals
                                              rules))
    (andmap (lambda (x) (equal? (temp-machine x) (if accepts? 'accept 'reject))) words)
    )
  )

;return-input-nfa:
; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
; --> (listof (listof symbols))
;purpose: builds the machine and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
(define (return-input-ndfa states
                           sigma
                           start
                           finals
                           rules
                           words
                           accepts?)
  (define temp-machine (make-unchecked-ndfa states
                                            sigma
                                            start
                                            finals
                                            rules))
  (filter (lambda (x) (equal? (temp-machine x) (if accepts? 'reject 'accept))) words)
  )

;check-input-ndpda
; (listof states) sigma gamma symbol (listof states) (listof ndpda-rules) boolean symbol
; --> [(listof (listof symbols)) --> boolean]
;purpose: to take in all the components of a machine, and then a list
; of words to check in that machine, then run those words through the machine
; and make sure that they match the "accepts?" symbol, which will be either
; accept or reject. This ensures that this can be used for both accept lists
; and rejects lists.
(define (check-input-ndpda states
                           sigma
                           gamma
                           start
                           finals
                           rules
                           accepts?)
  (lambda (words)
    (define temp-machine (make-unchecked-ndpda states
                                               sigma
                                               gamma
                                               start
                                               finals
                                               rules))
    (andmap (lambda (x) (equal? (temp-machine x) (if accepts? 'accept 'reject))) words)
    )
  )

;return-input-ndpda:
; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
; --> (listof (listof symbols))
;purpose: builds the machine and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
(define (return-input-ndpda states
                            sigma
                            gamma
                            start
                            finals
                            rules
                            words
                            accepts?)
  (define temp-machine (make-unchecked-ndpda states
                                             sigma
                                             gamma
                                             start
                                             finals
                                             rules))
  (filter (lambda (x) (equal? (temp-machine x) (if accepts? 'reject 'accept))) words)
  )

;apply-input-tm: tm word --> accept/reject
;purpose: to apply a turing machine to a word
(define (apply-input-tm machine word)
  (if (and (equal? (length word) 2)
           (integer? (cadr word)))
      (sm-apply machine (car word) (cadr word))
      (sm-apply machine word))
  )


;check-input-tm
; (listof states) (listof alphabet) symbol (listof states) (listof tm-rules) boolean symbol
; --> [(listof (listof symbols)) --> boolean]
;purpose: to take in all the components of a machine, and then a list
; of words to check in that machine, then run those words through the machine
; and make sure that they match the "accepts?" symbol, which will be either
; accept or reject. This ensures that this can be used for both accept lists
; and rejects lists.
(define (check-input-tm states
                        sigma
                        start
                        finals
                        rules
                        accept
                        accepts?)
  (lambda (words)
    (define temp-machine (make-unchecked-tm states
                                            sigma
                                            rules
                                            start
                                            finals
                                            accept))
    (andmap (lambda (x) (equal? (apply-input-tm temp-machine x) (if accepts? 'accept 'reject))) words)
    )
  )

;return-input-tm:
; (listof states) sigma symbol (listof states) (listof rules) boolean (listof (listof symbols) symbol
; --> (listof (listof symbols))
;purpose: builds the machine and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
(define (return-input-tm states
                         sigma
                         start
                         finals
                         rules
                         words
                         accept
                         accepts?)
  (define temp-machine (make-unchecked-tm states
                                          sigma
                                          rules
                                          start
                                          finals
                                          accept))
  (filter (lambda (x) (equal? (apply-input-tm temp-machine x) (if accepts? 'reject 'accept))) words)
  )

;check-input-mttm:
; (listof states) (listof alpha) state (listof states) (listof mttm-rule) (listof word) boolean symbol
; --> (listof (listof symbol))
;purpose: to take in all the components of a multi-tape tm, and then a list
; of words to check in that machine, then run those words through the machine
; and make sure that they match the "accepts?" symbol, which will be either
; accept or reject. This ensures that this can be used for both accept lists
; and rejects lists.
(define ((check-input-mttm states
                           sigma
                           start
                           finals
                           rules
                           num-tapes
                           accept
                           accepts?) words)
  (define temp-machine (make-unchecked-mttm states sigma start finals rules num-tapes accept))
  (andmap (lambda (word) (equal? (apply-input-tm temp-machine word) (if accepts? 'accept 'reject))) words)
  )

;return-input-mttm:
; (listof states) (listof alpha) state (listof states) (listof mttm-rule) (listof word) boolean symbol
; --> (listof (listof symbol))
;purpose: builds the multi-tape tm and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
(define (return-input-mttm states
                           sigma
                           start
                           finals
                           rules
                           num-tapes
                           words
                           accept
                           accepts?)
  (define temp-machine (make-unchecked-mttm states sigma start finals rules num-tapes accept))
  (filter (lambda (word) (equal? (apply-input-tm temp-machine word) (if accepts? 'reject 'accept))) words))

;; Grammars
;check-input-grammar:
; (listof states) (listof alphabet) (listof rg-rules) symbol boolean
; --> [(listof (listof symbols)) --> boolean]
;purpose: to take in all the components of a grammar, and then a list
; of words to check in that grammar, then run those words through the grammar
; and make sure that they either accept or reject
; This ensures that this can be used for both accept lists
; and rejects lists.
(define (check-input-grammar states
                             sigma
                             rules
                             start
                             accepts?
                             constructor
                             derive)
  (lambda (words)
    (define temp-grammar (constructor states
                                      sigma
                                      rules
                                      start))
    (andmap (lambda (x) (if accepts?
                            (not (string? (derive temp-grammar x)))
                            (string? (derive temp-grammar x))))
            words)
    )
  )
    

;return-input-grammar:
; (listof states) sigma (listof rules) symbol (listof (listof symbols) boolean
; grammar-constructor grammer-deriver --> (listof (listof symbols))
;purpose: builds the machine and finds all the words from the input
; list of words that don't accept/reject (based on accepts?)
; The grammar-constructor is a function that takes as input the states,
; sigma, rules, and start state, and constructs an unchecked version of that
; grammar.
; The grammer-deriver is a function that takes in a grammar and a word
; and returns a string if the word is not in the language of the grammar
(define (return-input-grammar states
                              sigma
                              rules
                              start
                              words
                              accepts?
                              constructor
                              derive)
  (define temp-machine (constructor states
                                    sigma
                                    rules
                                    start))
  (filter (lambda (x)
            (if accepts?
                ; If the words are meant to be accepted, since we are returning
                ; the list of words that failed, we check if derive returns
                ; a string.
                (string? (derive temp-machine x))
                ; If the words are meant to be rejected, since we are returning
                ; the list of words that passed, we check if derive returns
                ; not a string.
                (not (string? (derive temp-machine x)))))
          words)
  )

