#lang racket

(require "main.rkt" test-engine/racket-tests)

; L(KLEENESTAR-abUaba) = (abUaba)*
;; does not require new start and final states
(define KLEENESTAR-abUaba (make-ndfa '(S A B C D E U V)
                                     '(a b)
                                     'V
                                     '(U)
                                     `((V ,EMP S)
                                       (S ,EMP U)
                                       (S a A)
                                       (A b B)
                                       (B a C)
                                       (C ,EMP S)
                                       (S a D)
                                       (D b E)
                                       (E ,EMP S))))

; L(KLEENESTAR-abUaba) = (abUaba)*
;; requires new start and final states
(define KLEENESTAR-abUaba2 (make-ndfa '(S A B C D E)
                                      '(a b)
                                      'S
                                      '(S)
                                      `((S a A)
                                        (A b B)
                                        (B a C)
                                        (C ,EMP S)
                                        (S a D)
                                        (D b E)
                                        (E ,EMP S))))

; L = a*
(define a* (make-ndfa '(S)
                      '(a b)
                      'S
                      '(S)
                      `((S a S))))

; L = a*b*
(define a*b* (make-ndfa '(S F)
                        '(a b)
                        'S
                        '(F)
                        `((S a S)
                          (S ,EMP F)
                          (F b F))))

; L = (aab)*
(define aab* (make-ndfa '(S A B)
                        '(a b)
                        'S
                        '(S)
                        `((S a A)
                          (A a B)
                          (B b S))))

;(define GR (sm-graph KLEENESTAR-abUaba))


;;; fsa --> regexp
;(define (fsa2regexp m)
;
;  (define (degree-in s rules)
;    (length (filter (λ (r) (eq? s (third r))) rules)))
;
;  (define (degree-out s rules) 
;    (length (filter (λ (r) (eq? s (first r))) rules)))
;
;  ;; nonempty-(listof edges) --> edge
;  ;; Purpose: Merge given edges into a single edge
;  (define (merge-edges edges)
;
;    (define (mk-union-label r1 r2)
;      (cond [(and (empty-regexp? r1) (kleenestar-regexp? r2)) r2]
;            [(and (empty-regexp? r2) (kleenestar-regexp? r1)) r1]
;            [else (union-regexp r1 r2)]))
;    
;    (cond [(empty? (rest edges)) (first edges)] ;; only 1 edge
;          [(empty? (rest (rest edges))) ;; only two edges
;           (list (first (first edges))
;                 (mk-union-label (second (first edges)) (second (second edges)))
;                 (third (first edges)))]
;          [else (let ((u1 (second (first edges)))
;                      (u2 (merge-edges (rest edges))))
;                  (list (first (first edges))
;                        (mk-union-label u1 u2)
;                        (third (first edges))))]))
;
;  (define (merge-medges G ne)
;    (if (empty? G)
;        ne
;        (let* ((frule (first G))
;               (ffrom (first frule))
;               (fto (third frule))
;               (medges (filter (λ (e) (and (eq? (first e) ffrom) (eq? (third e) fto))) G)))
;          (merge-medges (filter (λ (e) (not (and (eq? (first e) ffrom) (eq? (third e) fto))))
;                                G)
;                        (cons (merge-edges medges) ne)))))
;    
;
;  ;; (listof edges) (listof state) --> (listof edges)
;  ;; Assume: The list of states does not include start/final states
;  (define (remove-internal-states G istates strt fnl)
;
;    (define (remove-state s)
;      
;      ;; state (listof edges) (listof edges) --> (listof edges)
;      (define (make-new-rem-edges s G into-s-edges out-s-edges)
;
;        ;; regexp regexp --> regexp
;        (define (mk-concat-regexp re1 re2)          
;          (cond [(empty-regexp? re1) re2]
;                [(empty-regexp? re2) re1]
;                [else (concat-regexp re1 re2)]))
;
;        (define (merge-out-edge e ins)
;          (map (λ (ie)
;                 (list (first ie)
;                       (let* ((self-loop-lst (filter (λ (e) (and (eq? s (first e)) (eq? s (third e))))
;                                                     G))
;                              (self-loop-edge (if (null? self-loop-lst) '() (first self-loop-lst))))
;                         (if (null? self-loop-edge)
;                             (mk-concat-regexp (second ie) (second e)) ;; s does not have a self-loop
;                             (mk-concat-regexp (second ie)
;                                               (mk-concat-regexp (kleenestar-regexp (second self-loop-edge))
;                                                                 (second e)))))
;                       (third e)))
;               ins))
;          
;        (define (merge-into-edge e outs)
;
;          (define res (map (λ (eo)
;                             (list (first e)
;                                   (let* ((self-loop-lst (filter (λ (e) (and (eq? s (first e)) (eq? s (third e))))
;                                                                 G))
;                                          (self-loop-edge (if (null? self-loop-lst) '() (first self-loop-lst))))
;                                     (if (null? self-loop-edge)
;                                         (mk-concat-regexp (second e) (second eo)) ;; s does not have a self-loop
;                                         (mk-concat-regexp (second e)
;                                                           (mk-concat-regexp (kleenestar-regexp (second self-loop-edge))
;                                                                             (second eo)))))
;                                   (third eo)))
;                           outs))
;          res)
;        (let* ((mins (append-map (λ (e)
;                                   (let* ((me (merge-into-edge e out-s-edges)))
;                                     me))
;                                 into-s-edges))
;               (mouts (append-map (λ (e)
;                                    (let* ((me (merge-out-edge e into-s-edges)))
;                                      me))
;                                  out-s-edges)))
;          (append mins mouts)))
;
;      (define (remove-self-loops g) (filter (λ (e) (not (eq? (first e) (third e)))) g))
;      
;      (let* ((out-s-edges (remove-self-loops (filter (λ (e) (eq? (first e) s)) G)))
;             (into-s-edges (remove-self-loops (filter (λ (e) (eq? (third e) s)) G))))
;        (make-new-rem-edges s G into-s-edges out-s-edges)))
;    (cond [(null? istates) G]
;          [(or (eq? (first istates) strt) (eq? (first istates) fnl))
;           (remove-internal-states G (rest istates) strt fnl)]
;          [else (let* ((rems (first istates))
;                       (removed-state-edges (remove-state rems))
;                       (new-G (remove-duplicates
;                               (append removed-state-edges
;                                       (filter (λ (e)
;                                                 (and (not (eq? (first e) rems))
;                                                      (not (eq? (third e) rems))))
;                                               G))))
;                       (mme-G (merge-medges  new-G '())))
;                  (remove-internal-states mme-G (rest istates) strt fnl))]))
;  
;  (let* ((rules (sm-getrules m)) ;; edge representation of a graph
;         (start (sm-getstart m))
;         (states (sm-getstates m))
;         (finals (sm-getfinals m))
;         (trules (map (λ (r)
;                        (list (first r)
;                              (if (eq? (second r) EMP)
;                                  (empty-regexp)
;                                  (singleton-regexp (symbol->string (second r))))
;                              (third r)))
;                      rules)) ;; edges with singleton regexp
;         (new-start (if (= (degree-in (sm-getstart m) rules) 0)
;                        start
;                        (generate-symbol start states)))
;         (new-final (if (and (= (length finals) 1)
;                             (= (degree-out (first finals) rules) 0))
;                        (first finals)
;                        (generate-symbol 'F states)))
;         (new-states (cond [(and (not (eq? start new-start))
;                                 (not (eq? new-final (first finals))))
;                            (cons new-start (cons new-final states))]
;                           [(and (eq? start new-start) (eq? new-final (first finals)))
;                            states]
;                           
;                           [(eq? start new-start) (cons new-final states)]
;                           [else (cons new-start states)]))
;         (new-finals (list new-final))
;         (new-graph (cond [(and (not (eq? start new-start))
;                                (not (eq? (first finals) new-final))) ;; need new start and final states
;                           (cons (list new-start (empty-regexp) start)
;                                 (append (map (λ (fs) (list fs (empty-regexp) new-final))
;                                              finals)
;                                         trules))]
;                          [(not (eq? start new-start)) ;; need new start state
;                           (cons (list new-start (empty-regexp) start) trules)]
;                          [(not (eq? (first finals) new-final)) ;; need new final state
;                           (append (map (λ (fs) (list fs (empty-regexp) new-final))
;                                        finals)
;                                   trules)]
;                          ;; new start and final states not needed
;                          [else trules]))
;         (merged-edges-graph (merge-medges new-graph '())) ;; graph with merged mutiple edges between any two nodes and new start and final state if necessary
;         (final-graph
;          (remove-internal-states merged-edges-graph
;                                  new-states
;                                  new-start
;                                  new-final)))
;    
;    (printable-regexp (simplify-regexp (second (first final-graph))))))

(check-expect (fsa->regexp KLEENESTAR-abUaba)
              "(aba(aba)* U (ε U (a U aba(aba)*a)b((a U aba(aba)*a)b)*(ε U aba(aba)*)))")

(check-expect (fsa->regexp KLEENESTAR-abUaba2)
              "(aba(aba)* U (ε U (a U aba(aba)*a)b((a U aba(aba)*a)b)*(ε U aba(aba)*)))")

(check-expect (fsa->regexp a*)
              "a*")

(check-expect (fsa->regexp a*b*)
              "a*b*")

(check-expect (fsa->regexp aab*)
              "(ε U aa(baa)*b)")

(test)