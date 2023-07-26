#lang fsm
(require test-engine/racket-tests)

(define R1 (union-regexp (singleton-regexp "a")
                         (singleton-regexp "b")))

(define R2 (concat-regexp (singleton-regexp "m") R1))

(define R3 (kleenestar-regexp R2))

;; dgraph --> ndfa
;; Purpose: Create ndfa from given dgraph
(define (dgraph2lodgraph dgraph)
  
  ;; digraph --> Boolean
  (define (only-simple-edges? grph)
    (andmap (λ (e) (or (empty-regexp? (second e))
                       (singleton-regexp? (second e))))
            grph))

  ;; dgraph --> dedge
  ;; Assumption: dgraph has a nonsimple edge
  (define (extract-first-nonsimple grph)
    (first (filter (λ (e) (and (not (empty-regexp? (second e)))
                               (not (singleton-regexp? (second e)))))
                   grph)))
  
  ;; dgraph (listof dgraph) --> (listof dgraph)
  (define (bfs grph acc)
    #;(displayln (format "graph: ~a\n" (map (λ (e) (format "(~a ~a ~a)"
                                                         (first e)
                                                         (printable-regexp (second e))
                                                         (third e) ))
                                          grph)))
    (if (only-simple-edges? grph)
        (cons grph acc)
        (let* [(edge (extract-first-nonsimple grph))
               (fromst (first edge))
               (rexp (second edge))
               (tost (third edge))
               #;(dd (displayln (format "graph: ~a\n" (map (λ (e) (format "(~a ~a ~a)"
                                                                          (first e)
                                                                          (printable-regexp (second e))
                                                                          (third e) ))
                                                           grph))))
               #;(d (displayln (format "edge: (~a ~a ~a)\n" fromst (printable-regexp rexp) tost)))]
          (cond [(union-regexp? rexp)
                 (let [(newi1 (generate-symbol 'I '(I)))
                       (newi2 (generate-symbol 'I '(I)))
                       (newi3 (generate-symbol 'I '(I)))
                       (newi4 (generate-symbol 'I '(I)))]
                 (bfs
                  (append (list (list fromst (empty-regexp) newi1)
                                (list fromst (empty-regexp) newi2)
                                (list newi1 (union-regexp-r1 rexp) newi3)
                                (list newi2 (union-regexp-r2 rexp) newi4)
                                (list newi3 (empty-regexp) tost)
                                (list newi4 (empty-regexp) tost))
                          (remove edge grph))
                                
                  #;(cons (list fromst (union-regexp-r1 rexp) tost)
                        (cons (list fromst (union-regexp-r2 rexp) tost)
                              (remove edge grph)))
                  (cons grph acc)))]
                [(concat-regexp? rexp)
                 (let [(istate1 (generate-symbol 'I '(I)))
                       (istate2 (generate-symbol 'I '(I)))]
                   (bfs (append (list (list fromst (concat-regexp-r1 rexp) istate1)
                                      (list istate1 (empty-regexp) istate2)
                                      (list istate2 (concat-regexp-r2 rexp) tost))
                                (remove edge grph))
                        (cons grph acc))                   
                   #;(bfs (cons (list fromst (concat-regexp-r1 rexp) istate)
                              (cons (list istate (concat-regexp-r2 rexp) tost)
                                    (remove edge grph)))
                        (cons grph acc)))]
                [else
                 (let [(istart (generate-symbol 'I '(I)))]
                   (bfs
                    (append (list (list istart (empty-regexp) fromst)
                                  (list fromst (kleenestar-regexp-r1 rexp) tost)
                                  (list tost (empty-regexp) istart))
                            (remove edge grph))
                    (cons grph acc)))
                 #;(let [(istate (generate-symbol 'I '(I)))]
                   (bfs (cons (list fromst (empty-regexp) istate)
                              (cons (list istate (kleenestar-regexp-r1 rexp) istate)
                                    (cons (list istate (empty-regexp) tost)
                                          (remove edge grph))))
                        (cons grph acc)))]))))
  (bfs dgraph '()))

#| NO LONGER ILLUSTRATIVE
Illustrative tests; can't be implemented because of randomness in generating symbols

(check-equal? (dgraph2lodgraph (list (list 'S R1 'F)))
              (list
               (list (list 'S (singleton-regexp "a") 'F) (list 'S (singleton-regexp "b") 'F))
               (list (list 'S (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'F))))

(check-equal? (dgraph2lodgraph (list (list 'S R2 'F)))
              (list
               (list
                (list 'I-1648402 (singleton-regexp "a") 'F)
                (list 'I-1648402 (singleton-regexp "b") 'F)
                (list 'S (singleton-regexp "m") 'I-1648402))
               (list
                (list 'S (singleton-regexp "m") 'I-1648402)
                (list 'I-1648402 (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'F))
               (list
                (list
                 'S
                 (concat-regexp
                  (singleton-regexp "m")
                  (union-regexp (singleton-regexp "a") (singleton-regexp "b")))
                 'F))))

(check-equal? (dgraph2lodgraph (list (list 'S R3 'F)))
              (list
               (list
                (list 'I-1637562 (singleton-regexp "a") 'I-1637561)
                (list 'I-1637562 (singleton-regexp "b") 'I-1637561)
                (list 'I-1637561 (singleton-regexp "m") 'I-1637562)
                (list 'S (empty-regexp) 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list 'I-1637561 (singleton-regexp "m") 'I-1637562)
                (list 'I-1637562 (union-regexp (singleton-regexp "a") (singleton-regexp "b")) 'I-1637561)
                (list 'S (empty-regexp) 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list 'S (empty-regexp) 'I-1637561)
                (list
                 'I-1637561
                 (concat-regexp
                  (singleton-regexp "m")
                  (union-regexp (singleton-regexp "a") (singleton-regexp "b")))
                 'I-1637561)
                (list 'I-1637561 (empty-regexp) 'F))
               (list
                (list
                 'S
                 (kleenestar-regexp
                  (concat-regexp
                   (singleton-regexp "m")
                   (union-regexp (singleton-regexp "a") (singleton-regexp "b"))))
                 'F))))
|#