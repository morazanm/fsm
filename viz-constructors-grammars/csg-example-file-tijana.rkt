#lang fsm




(define even-bs-odd-as (make-cfg '(S A B C)
                                 '(a b)
                                 `((S ,ARROW aA)
                                   (S ,ARROW bB)
                                   (S ,ARROW a)
                                   (A ,ARROW aS)
                                   (A ,ARROW bC)
                                   (B ,ARROW aC)
                                   (B ,ARROW bS)
                                   (C ,ARROW aB)
                                   (C ,ARROW bA)
                                   (C ,ARROW b))
                                 'S))




;; yield is a strucutre that has
;; pr - processed part of the word
;; nt - nonterminal
;; up - unprocessed part of the word
(struct yield (pr nt up))

;; nonterminal?
;; symbol -> Boolean
;; Purpose: Determines if the first character within the symbol is a uppercase letter,
;; and hence a nonterminal
(define (nonterminal? symb)
  (let [(ascii-val (char->integer (first (string->list (symbol->string symb)))))]
    (and (<= 65 ascii-val)
         (>= 90 ascii-val))))

;; nnt?
;; symbol -> Boolean
;; Purpose: Just the opposite of nonterminal?
(define (nnt? symb)
  (not (nonterminal? symb)))

;; lower?
;; symbol -> Boolean
;; Purpose: Determines if a symbol is down case
(define (lower? symbol)
  (not (char-upper-case? (string-ref (symbol->string symbol) 0))))


;; create-yield
;; der -> yield
;; Purpose: To take a single derivation and turn it into yield structure
(define (create-yield wd)
  (let* [(firstnt (if (empty? (dropf wd lower?))
                      '()
                      (first (dropf wd lower?))))
         (leftmost (takef wd lower?))
         (rightmost (remove* (cons firstnt leftmost) wd))]
    (yield leftmost firstnt rightmost)))
         
         
         

;; create-yields
;; w-der -> yields
;; Purpose: To create the yield structure from the word derivation
(define (create-yields w-der)
  (let* [(wd (map symbol->fsmlos (filter (λ (x) (not (equal? x '->))) w-der)))]
    (map create-yield wd)))
         
         














