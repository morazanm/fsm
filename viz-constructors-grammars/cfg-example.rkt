#lang fsm


;; Syntactic Categories
;;  S = words such that number of b > number of a
;;  A = words such that number of b >= number of a


;; L =w | win(ab)*AND w has more b than a

(define numb>numa (make-cfg '(S A)
                            '(a b)
                            `((S ,ARROW b)
                              (S ,ARROW AbA)
                              (A ,ARROW AaAbA)
                              (A ,ARROW AbAaA)
                              (A ,ARROW ,EMP)
                              (A ,ARROW bA))
                            'S))

;; levels is a structure that has
;; list of levels
;; accumulator for all nodes that appear in levels
;; leftmost accumulator (basically just the root node we need to remember)
(struct levels (lol acc lma))

;; lower-first?
;; edge -> Boolean
;; Purpose: To check if the second element is lowercase
(define (lower-first? edge)
  (lower? (second edge)))

;; rename-nodes
;; levels -> levels
;; Purpose: To rename the terminals that reoccur in extracted edges
        
  
                         
  


;; w-der
;; derivation -> derivation-list
;; Purpose: To turn the derivation into a list
(define (w-der rg word)
  (map symbol->fsmlos (filter (λ (x) (not (equal? x '->)))
                              (grammar-derive rg word))))

;; lower?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is down case
(define (lower? symbol)
  (not (char-upper-case? (string-ref (symbol->string symbol) 0))))


;; list-intersect?
;; (listof symbol) (listof symbol) -> Boolean
;; Purpose: To check if two lists have the same element in them
(define (list-intersect? los1 los2)
  (ormap (λ (symbol) (member symbol los2)) los1))


;; generate-level
;; (listof symbol) (listof symbol) -> level
;; Purpose: To generate levels for intersect lists
(define (generate-level los1 los2)
  (let* [(leftmost (takef los2 lower?))
         (rightmost (take-right los2 (- (length los1) 1)))
         (nonterminal (first (dropf los1 lower?)))
         (new (if (empty? (drop-right los2 (length rightmost)))
                  (list 'ε)
                  (drop (drop-right los2 (length rightmost)) (length leftmost))))]
    (levels (for*/list ([i (list nonterminal)]
                       [j new])
             (list i j)) (list nonterminal) nonterminal)))


    
;; create-levels
;; derivation-list -> (listof level)
;; To generate a list of levels from wder
(define (create-levels wd)
  (if (= (length wd) 1)
      empty
      (cons (generate-level (first wd) (second wd)) (create-levels (rest wd)))))
        












        

