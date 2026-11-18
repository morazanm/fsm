#lang racket/base

(require racket/set
         racket/format)

(provide parse-consumed-input)

(define EMPTY-WORD 'EP)

;; ci (listof NT) (listof terminal) -> (hash NT . (setof word)
;; Purpose: Parses the given ci to determine what words are generated from each NT
(define (parse-consumed-input ci loNT loT)  
  ;; (vectorof symbol) (setof NT) (setof terminal)  (hash NT . (setof word)-> (hash NT . (setof word)
  ;;Purpose: Parses the given ci to determine what words are generated from each NT
  ;;ACC: hash contains words for the NT processed so far
  (define (parse-ci ci NTs terminals ci-length acc)
    ;; number number number (hash NT . (setof word) -> (hash NT . (setof word)
    ;;Purpose: Parses the given ci to determine what words are generated from each NT
    ;;ACC: hash contains words for the NT processed so far
    (define (parse-ci-helper nt-first-idx nt-last-idx acc)
      ;;X -> Boolean
      ;;Purpose: Determines if X is a NT
      (define (is-NT? x)
        (set-member? NTs x))
      ;;number los -> word
      ;;Purpose: Removes all of the NTs from [nt-first-idx..hi]
      ;; ACC: word = the terminals - {EMPTY-WORD} from [hi..last-occur-idx]
      (define (remove-nts hi acc)
        ;;X -> Boolean
        ;;Purpose: Determines if X is a terminal
        (define (is-terminal? x)
          (set-member? terminals x))
        (if (< hi nt-first-idx)
            acc
            (remove-nts (sub1 hi)
                        (let ([curr-elem (vector-ref ci hi)])
                          (if (and (is-terminal? curr-elem)
                                   (not (eq? curr-elem EMPTY-WORD)))
                              (cons curr-elem acc)
                              acc)))))                  

      ;; NT number number -> number
      ;;Purpose: Finds the index of the last occurence of the given NT in ci
      ;;ACC: The index of the last NT equal to the given NT from ci[0..idx-1]
      (define (find-last-idx NT idx acc)
        (if (> idx nt-last-idx)
            acc
            (find-last-idx NT
                           (add1 idx)
                           (if (not (eq? NT (vector-ref ci idx))) acc idx))))     

      ;;number number -> number
      ;;Purpose: Finds the next index of a NT that comes after the first the nt-first-idx but is before the last-occurence.
      ;; If none can be found, return the index of a distinct that is after last-occurence.
      (define (find-next-first-NT idx last-occurence)
        ;; number NT -> number
        ;;Purpose: Finds a *DISTINCT* NT that occurs after the given NT
        (define (find-new-NT idx NT)
          (if (or (> idx ci-length) ;;shouldn't happened, could be removed
                  (and (is-NT? (vector-ref ci idx))
                       (not (eq? NT (vector-ref ci idx)))))
              idx
              (find-new-NT (add1 idx) NT)))
        (cond [(is-NT? (vector-ref ci idx)) idx]
              [(>= idx last-occurence) (find-new-NT idx (vector-ref ci last-occurence))]
              [(find-next-first-NT (add1 idx) last-occurence)]))

      ;; number -> number 
      ;;Purpose: Finds the next index of a NT that comes before the last occurence index but is after the nt-first-idx.
      ;; If none can be found, return the last valid index into the vector
      (define (find-next-last-NT idx)
        (cond [(is-NT? (vector-ref ci idx)) idx]
              [(<= idx nt-first-idx) ci-length]
              [(find-next-last-NT (sub1 idx))]))      
      (if (>= nt-first-idx ci-length)
          acc
          (let* ([first-elem (vector-ref ci nt-first-idx)]
                 [last-occur-idx (find-last-idx first-elem (add1 nt-first-idx) nt-first-idx)])        
            (parse-ci-helper (find-next-first-NT (add1 nt-first-idx) last-occur-idx)
                             (find-next-last-NT (sub1 last-occur-idx))
                             (hash-set acc
                                       first-elem
                                       (set-add (hash-ref acc first-elem)
                                                (remove-nts last-occur-idx '())))))))
    (parse-ci-helper 0 ci-length acc))  
  (parse-ci (list->vector ci)
            (list->set loNT)
            (list->set loT)
            (sub1 (length ci))
            (foldl (λ (NT acc)
                     (hash-set acc NT (set)))
                   (hash)
                   loNT)))

#|
(define to-string (λ (x) (apply ~a x)))

(define sampleNTs '(S A B))
(define sampleTs '(a b EP))
(define sampleCI '(S A a A a A EP A b A b A B EP B S))
(define sampleCI2 '(S A EP A B b B EP B B S))

(parse-consumed-input sampleCI2 sampleNTs sampleTs)
(to-string sampleCI2)
|#