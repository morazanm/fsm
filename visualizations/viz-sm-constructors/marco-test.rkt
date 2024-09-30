#lang fsm

(require "../fsm-gviz/private/lib.rkt")

(define (make-node-graph graph los s f)
  (foldl (λ (state result)
           (add-node
            result
            state
            #:atb (hash 'color (cond [(and (eq? state s)
                                           (member state f))
                                      'violet]
                                     [(eq? state s) 'darkgreen]
                                     [(member state f)
                                      'violet] 
                                     [else 'black])
                        'shape (if (member state f)
                                   'doublecircle
                                   'circle)
                        'label (if (equal? state '())
                                   'ds  
                                   state)
                        'fontcolor 'black
                        'font "Sans")))
         graph
         los))

(define (los2symb los)
  (define (helper los)
    (if (empty? (rest los))
        (symbol->string (first los))
        (string-append (symbol->string (first los))
                       " "
                       (helper (rest los)))))
  (string->symbol (helper los)))

(define G (create-graph 'dgraph #:atb (hash 'rankdir "TB" 'font "Sans")))

(begin
  (set! G (make-node-graph G '(S0 b0 S1 a0 b1 S2 a1) 'Z '(Z))) ;; the order in the second argument matters: leftmost first!
  (set! G (foldl (λ (rule result) (add-edge result
                                    (second rule)
                                    (first rule)
                                    (third rule)
                                    #:atb (hash 'fontsize 20
                                                'style 'solid
                                                'color 'black)))
         G
         '((S0 "" a0) (S0 "" S1) (S0 "" b0)
           (S1 "" a1) (S1 "" S2) (S1 "" b1))))
  (graph->bitmap  G))