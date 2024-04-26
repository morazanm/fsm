#lang racket

(require "../fsm-core/private/misc.rkt"
         "../fsm-core/private/constants.rkt"
         "../fsm-core/private/csg.rkt"
         )

(define (csg-derive-edited g w)
    
  ; csg-rule natnum (listof symbol) --> (listof (listof symbol))
  (define (use-csg-rule r str i)
   
    ; (listof symbol) (listof symbol) natnum --> (listof (listof symbol))
    (define (helper lhs rhs i)
      (cond [(< (- (length (first str)) i) (length lhs)) '()]
            [else
             (let* [
                    (subword (sublist (first str) i (length lhs)))
                    ]
               (cond [(equal? lhs subword)
                      (if (equal? rhs (list EMP))
                          (cons (list (subst-in-list (first str) i (length lhs) '()) lhs rhs)
                                (helper lhs rhs (+ i 1))
                                )
                          (cons (list (subst-in-list (first str) i (length lhs) rhs) lhs rhs)
                                (helper lhs rhs (+ i 1))
                                )
                          )
                      ]
                     [else (helper lhs rhs (+ i 1))]
                     )
               )
             ]
            )
      )
    (let [
          (res (helper (csg-rule-lhs r) (csg-rule-rhs r) i))
          ]
      res
      )
    )
    
  ; (listof symbol) (listof csg-rule) --> (listof (listof symbol))
  (define (apply-one-step curr rls)
    (cond [(null? rls) '()]
          [else
           (append (use-csg-rule (car rls) curr 0) (apply-one-step curr (cdr rls)))
           ]
          )
    )
    
  ; (listof symbol) (listof (listof (listof symbol))) -> (listof (listof symbol))
  (define (bfs-deriv generated-derivations tovisit)

    ;(displayln generated-derivations)
    (define (ins paths)
      (define (insert path sortedpaths)
        (cond [(null? sortedpaths) (list path)]
              [(< (length (car (first path))) (length (caar (first sortedpaths))))
               (cons path sortedpaths)]
              [else (cons (car sortedpaths) (insert path (cdr sortedpaths)))]
              )
        )
        
      (cond [(null? paths) '()]
            [else (insert (car paths) (ins (cdr paths)))]
            )
      )
      
      
    (cond [(null? tovisit) '()]
          [else
           (let* ((firstpath (car tovisit))
                  (current (car firstpath))
                  )
             (cond [(equal? w (first current)) firstpath]
                   [else (let* [
                                (new-words (apply-one-step current (csg-getrules g)))
                                (newstrings (filter (lambda (s) (not (member (first s) generated-derivations)))
                                                    new-words)
                                            )
                                (new-queue-paths (map (lambda (s)
                                                              (cons s firstpath))
                                                            newstrings)
                                                 )
                                (newpaths (ins (append (cdr tovisit)
                                                       new-queue-paths)
                                               )
                                          )
                                ]
                           (bfs-deriv (append newstrings generated-derivations) newpaths)
                           )
                         ]
                   )
             )
           ]
          )
    )
  
  (define (move-elements-over lst) (if (= (length lst) 2)
                                     (cons (list (first (first lst)) (second (second lst)) (third (second lst))) (cons (list (first (second lst)) '() '()) '()))
                                     (cons (list (first (first lst)) (second (second lst)) (third (second lst))) (move-elements-over (rest lst)))
                                     )
  )
    
  (let* [
         (res (bfs-deriv '() (list (list (list (list (csg-getstart g)) '() '() )))))
         (result (move-elements-over (reverse res)))
         ]
      (if (null? result)
          (format "~s is not in L(G)." w)
          (append-map (lambda (l)
                        (if (equal? w (first l)) 
                            (if (null? l)
                                (list EMP)
                                (list (los->symbol (first l)) (los->symbol (second l)) (los->symbol (third l)))
                                )
                            (list (los->symbol (first l)) (los->symbol (second l)) (los->symbol (third l)) ARROW)
                            )
                        )
                       result)
          )
    )
  )

;; rename-symb
;; MutableHashTable Symbol -> Symbol
;; Purpose:  Returns a unique version of the symbol given (via the addition of
;; a previously unused number to the end of it)
(define (rename-symb hashtable nt)
  (let [(result (hash-ref hashtable nt #f))] 
    (if result
        (begin (hash-set! hashtable nt (add1 result))
               (string->symbol (format "~s~s" nt (add1 result))))
        (begin (hash-set! hashtable nt 0)
               (string->symbol (format "~s0" nt))))))


;; generate-levels-list-helper
;; (listof symbol) (listof (listof symbol)) (listof (listof symbol)) MutableHashTable ->
;; (listof symbol) (U #f symbol)) -> (listof (listof (listof symbol)))
(define (generate-levels-list-helper current-state rules prev-states used-names find-nt-func)
  ;; The list of generated rules used contains an empty list denoting no more rules, hence
  ;; the need to call "first" first
  (if (empty? (first rules))
      ;; If theres no more rules to apply than computation is done
      '()
      (let [(current-nt (find-nt-func current-state))]
        (if (boolean? current-nt)
            ;; If its fails to find a nonterminal in the current state, attempt to go back up
            ;; the stack to a previous state
            (if (empty? prev-states)
                ;; If there are no more previous states, than the computation is done
                '()
                (let* [(prev-state (first prev-states))
                       (prev-current-nt (find-nt-func prev-state))
                       ;; Need to remove the leftmost-nt from the previous state since we just went
                       ;; down its respective path
                       ;; when we call the function again with it removed, the next nt will be processed
                       (updated-states (filter (lambda (x) (not (eq? prev-current-nt x))) prev-state))
                       ]
                  ;; Don't reduce the number of rules here since one was not sucessfully applied, only
                  ;; remove the state we popped off the stack
                  (generate-levels-list-helper updated-states
                                               rules
                                               (rest prev-states)
                                               used-names
                                               find-nt-func)))
            (local [;; Just sticks a number after the symbol itself, uses a hash table to keep track of what numbers
                    ;; were already used for a specific symbol
                    (define renamed-states (map (lambda (st) (rename-symb used-names st)) (first rules)))
                    
                    ;; Creates a new level by taking the current rule that is meant to be applied at this point of
                    ;; the derivation
                    ;; and creating an edge between each of the elements within and the current nonterminal being
                    ;;processed
                    (define (new-level start) (map (lambda (st) (list start st))
                                                   renamed-states))]
              (cons (new-level current-nt)
                    (generate-levels-list-helper renamed-states
                                                 (rest rules)
                                                 (cons current-state prev-states)
                                                 used-names find-nt-func))
              )))))

(define G1 (make-unchecked-csg '(S)
                                 '(a b)
                                 (list (list 'S ARROW EMP)
                                       (list 'aSb ARROW 'aaSbb)
                                       (list 'S ARROW 'aSb)
                                       )
                                 'S
                                 )
  )

;; S: Generates words in word word
;; A: Generates an A to match an already generated a
;; B: Generates a B to match and already generated b
#;(define P1 (make-unchecked-csg '(S A B)
                               '(a b)
                               (list (list 'S ARROW EMP)
                                     (list 'S ARROW 'aDA)
                                     (list 'S ARROW 'bDB)
                                     (list 'D ARROW EMP)
                                     (list 'D ARROW 'aDA)
                                     (list 'D ARROW 'bDB)
                                     ;(list 'DB 
                                     (list 'AB ARROW 'BA)
                                     (list 'BA ARROW 'AB)
                                     (list 'A ARROW 'a)
                                     (list 'B ARROW 'b)
                                     )
                               'S
                               )
  )

;; S: Generates words in form of a^n b^2n c^3n
;; A: Generates an A
;; B: Generates a B
;; C: Generates a C
(define P2 (make-unchecked-csg '(S A B C D E F)
                               '(a b c)
                               (list (list 'S ARROW 'ABBCCCS)
                                     (list 'S ARROW 'F)
                                     (list 'AB ARROW 'BA)
                                     (list 'AC ARROW 'CA)
                                     (list 'BA ARROW 'AB)
                                     (list 'BC ARROW 'CB)
                                     (list 'CB ARROW 'BC)
                                     (list 'CA ARROW 'AC)
                                     (list 'AD ARROW 'Da)
                                     (list 'BE ARROW 'Eb)
                                     (list 'CF ARROW 'Fc)
                                     (list 'F ARROW 'E)
                                     (list 'E ARROW 'D)
                                     (list 'D ARROW EMP)
                                     )
                               'S
                               )
  )



(define COPY (make-unchecked-csg '(S L M Z P K Z)
                                 '(a b)
                                 (list (list 'S ARROW 'LMZ)
                                       (list 'LM ARROW 'aLM)
                                       (list 'LM ARROW 'bLM)
                                       (list 'aLM ARROW 'aLKM)
                                       (list 'bLM ARROW 'bLPM)
                                       (list 'LPM ARROW 'LMP)
                                       (list 'LKM ARROW 'LMK)
                                       (list 'Pa ARROW 'aP)
                                       (list 'Pb ARROW 'bP)
                                       (list 'Ka ARROW 'aK)
                                       (list 'Kb ARROW 'bK)
                                       (list 'PZ ARROW 'bZ)
                                       (list 'KZ ARROW 'aZ)
                                       (list 'L ARROW EMP)
                                       (list 'M ARROW EMP)
                                       (list 'Z ARROW EMP)
                                       )
                                 'S
                                 )
  )
(define (COPY-COMP w) (make-unchecked-csg '(S L M Z P K Z R)
                                 '(a b)
                                 (list (list 'S ARROW 'LMZ)
                                       (list 'LM ARROW 'aLM)
                                       (list 'LM ARROW 'bLM)
                                       (list 'aLM ARROW 'aLKM)
                                       (list 'bLM ARROW 'bLPM)
                                       (list 'LPM ARROW 'LMP)
                                       (list 'LKM ARROW 'LMK)
                                       (list 'Pa ARROW 'aP)
                                       (list 'Pb ARROW 'bP)
                                       (list 'Ka ARROW 'aK)
                                       (list 'Kb ARROW 'bK)
                                       (list 'PZ ARROW 'bZ)
                                       (list 'KZ ARROW 'aZ)
                                       (list (los->symbol (list w 'LM w 'Z)) ARROW (los->symbol (list w 'LR w 'Z)))
                                       (list 'Ra ARROW 'R)
                                       (list 'Rb ARROW 'R)
                                       (list 'LRZ ARROW EMP)
                                       ;(list
                                       )
                                 'S
                                 )
  )


(define anbn (make-unchecked-csg '(S A B)
                                 '(a b)
                                 (list (list 'S ARROW 'AaB)
                                       (list 'AaA ARROW 'aSb)
                                       (list 'AaA ARROW EMP)
                                       (list 'B ARROW 'A)
                                       )
                                 'S
                                 )
  )

;(csg-derive anbn '(a a a b b b))
;(csg-derive anbn '())

; ABCABC
;(csg-derive P2 '(a b b c c c))
;(csg-derive P2 '(a a b b b b c c c c c c))
(csg-derive COPY '(a b a a b a))
;(csg-derive P1 '(a b b a a b b a))

;(define result1 (csg-derive-edited G1 '(a a b b)))
;(displayln result1)


;(move-elements-over (csg-derive-edited G1 '(a a b b)))


;(csg-derive G1 '(a a b b))