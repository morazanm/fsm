#lang racket/base

(module+ test
  (require "../../../../fsm-core/private/callgraphs/cg-defs.rkt"
           rackunit)

  ;; Tests for ndfa-Edge-fromst
  (check-equal? (ndfa-Edge-fromst (ndfa-edge 'Y 'b 'T)) 'Y)
  (check-equal? (ndfa-Edge-fromst (ndfa-edge 'Z 'ε 'R)) 'Z)
  (check-equal? (ndfa-Edge-fromst (ndfa-spedge 'Q 'b 'ds)) 'Q)
  (check-equal? (ndfa-Edge-fromst (ndfa-spedge 'A 'c 'ds)) 'A)

  ;; Tests for ndfa-Edge-read
  (check-equal? (ndfa-Edge-read (ndfa-edge 'Y 'b 'T)) 'b)
  (check-equal? (ndfa-Edge-read (ndfa-edge 'Z 'ε 'R)) 'ε)
  (check-equal? (ndfa-Edge-read (ndfa-spedge 'Q 'b 'ds)) 'b)
  (check-equal? (ndfa-Edge-read (ndfa-spedge 'A 'c 'ds)) 'c)

  ;; Tests for ndfa-Edge-tost
  (check-equal? (ndfa-Edge-tost (ndfa-edge 'Y 'b 'T)) 'T)
  (check-equal? (ndfa-Edge-tost (ndfa-edge 'Z 'ε 'R)) 'R)
  (check-equal? (ndfa-Edge-tost (ndfa-spedge 'Q 'b 'ds)) 'ds)
  (check-equal? (ndfa-Edge-tost (ndfa-spedge 'A 'c 'ds)) 'ds)

  
  ;; Tests for ndfa-Edges-equal?
  (check-equal? (ndfa-Edges-equal? (ndfa-edge 'Y 'b 'T) (ndfa-edge 'Y 'b 'T)) #t)
  (check-equal? (ndfa-Edges-equal? (ndfa-edge 'Z 'ε 'R) (ndfa-edge 'Y 'b 'T)) #f)
  (check-equal? (ndfa-Edges-equal? (ndfa-spedge 'Q 'b 'ds) (ndfa-edge 'Q 'b 'ds)) #t)
  (check-equal? (ndfa-Edges-equal? (ndfa-spedge 'A 'c 'ds) (ndfa-edge 'Q 'b 'ds)) #f)

  ;; Tests for ndfa-rule-fromst
  (check-equal? (ndfa-rule-fromst '(S ε S)) 'S)
  (check-equal? (ndfa-rule-fromst '(S a ds)) 'S)
  (check-equal? (ndfa-rule-fromst '(X a ds)) 'X)
  (check-equal? (ndfa-rule-fromst '(Y b X)) 'Y)

  ;; Tests for ndfa-rule-tost
  (check-equal? (ndfa-rule-tost '(S ε S)) 'S)
  (check-equal? (ndfa-rule-tost '(S a ds)) 'ds)
  (check-equal? (ndfa-rule-tost '(X a ds)) 'ds)
  (check-equal? (ndfa-rule-tost '(Y b X)) 'X)

  ;; Tests for ndfa-rule-read
  (check-equal? (ndfa-rule-read '(S ε S)) 'ε)
  (check-equal? (ndfa-rule-read '(S a ds)) 'a)
  (check-equal? (ndfa-rule-read '(X a ds)) 'a)
  (check-equal? (ndfa-rule-read '(Y b X)) 'b)

  ;; Tests for ndfa-stucis-equal?
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'S '(a b)) (ndfa-stuci 'S '(a))) #f)
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'S '(b)) (ndfa-stuci 'S '())) #f)
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'Q '()) (ndfa-stuci 'S '())) #f)
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'S '(a b)) (ndfa-stuci 'S '(a b))) #t)
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'R '(a b b)) (ndfa-stuci 'R '(a b b))) #t)
  (check-equal? (ndfa-stucis-equal? (ndfa-stuci 'S '()) (ndfa-stuci 'S '())) #t)

  ;; Tests for pda-Edge-fromst 
  (check-equal? (pda-Edge-fromst (pda-edge 'S 'ε 'ε 'Q '(S))) 'S)
  (check-equal? (pda-Edge-fromst (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'S)
  (check-equal? (pda-Edge-fromst (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'Q)
  (check-equal? (pda-Edge-fromst (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'Q)

  
  ;; Tests for pda-Edge-read 
  (check-equal? (pda-Edge-read (pda-edge 'S 'ε 'ε 'Q '(S))) 'ε)
  (check-equal? (pda-Edge-read (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
  (check-equal? (pda-Edge-read (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'ε)
  (check-equal? (pda-Edge-read (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'ε)

  ;; Tests for pda-Edge-pop 
  (check-equal? (pda-Edge-pop (pda-edge 'S 'ε 'ε 'Q '(S))) 'ε)
  (check-equal? (pda-Edge-pop (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
  (check-equal? (pda-Edge-pop (cutoff-edge 'Q 'ε '(S) 'Q '(b))) '(S))
  (check-equal? (pda-Edge-pop (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) '(S))

  ;; Tests for pda-Edge-tost 
  (check-equal? (pda-Edge-tost (pda-edge 'S 'ε 'ε 'Q '(S))) 'Q)
  (check-equal? (pda-Edge-tost (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ds)
  (check-equal? (pda-Edge-tost (cutoff-edge 'Q 'ε '(S) 'Q '(b))) 'Q)
  (check-equal? (pda-Edge-tost (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) 'Q)

  ;; Tests for pda-Edge-push 
  (check-equal? (pda-Edge-push (pda-edge 'S 'ε 'ε 'Q '(S))) '(S))
  (check-equal? (pda-Edge-push (pda-spedge 'S 'ε 'ε 'ds 'ε)) 'ε)
  (check-equal? (pda-Edge-push (cutoff-edge 'Q 'ε '(S) 'Q '(b))) '(b))
  (check-equal? (pda-Edge-push (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) '(A b A))

  ;; Tests for pda-Edges-equal?
  (check-equal? (pda-Edges-equal? (pda-edge 'S 'ε 'ε 'Q '(S)) (cutoff-edge 'S 'ε 'ε 'Q '(S))) #t)
  (check-equal? (pda-Edges-equal? (pda-spedge 'S 'ε 'ε 'ds 'ε) (pda-edge 'S 'ε 'ε 'Q '(S))) #f)
  (check-equal? (pda-Edges-equal? (cutoff-edge 'Q 'ε '(S) 'Q '(b)) (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A))) #f)
  (check-equal? (pda-Edges-equal? (cutoff-spedge 'Q 'ε '(S) 'Q '(A b A)) (pda-spedge 'Q 'ε '(S) 'Q '(A b A))) #t)

  ;; Tests for pda-stucis-equal?
  (check-equal? (pda-stucis-equal? (pda-stuci 'S '(a b) '(a) 0) (pda-stuci 'S '(a) '(a) 0)) #f)
  (check-equal? (pda-stucis-equal? (pda-stuci 'S '() '(b) 0) (pda-stuci 'S '() '() 1)) #f)
  (check-equal? (pda-stucis-equal? (pda-stuci 'Q '() '(b) 0) (pda-stuci 'S '() '(b) 1)) #f)
  (check-equal? (pda-stucis-equal? (pda-stuci 'S '(a b) '() 0) (pda-stuci 'S '(a b) '() 10)) #t)
  (check-equal? (pda-stucis-equal? (pda-stuci 'R '(a b) '(a) 0) (pda-stuci 'R '(a b) '(a) 10)) #t)
  (check-equal? (pda-stucis-equal? (pda-stuci 'S '() '() 0) (pda-stuci 'S '() '() 1)) #t)

  
  ;; Tests for pda-rule-fromst
  (check-equal? (pda-rule-fromst '((S ε ε) (X ε))) 'S)
  (check-equal? (pda-rule-fromst '((S a ε) (S (b b)))) 'S)
  (check-equal? (pda-rule-fromst '((X a (a)) (ds ε))) 'X)
  (check-equal? (pda-rule-fromst '((Y b (b b)) (ds ε))) 'Y)

  ;; Tests for pda-rule-tost
  (check-equal? (pda-rule-tost '((S ε ε) (X ε))) 'X)
  (check-equal? (pda-rule-tost '((S a ε) (S (b b)))) 'S)
  (check-equal? (pda-rule-tost '((X a (a)) (ds ε))) 'ds)
  (check-equal? (pda-rule-tost '((Y b (b b)) (ds ε))) 'ds)

  ;; Tests for read
  (check-equal? (pda-rule-read '((S ε ε) (X ε))) 'ε)
  (check-equal? (pda-rule-read '((S a ε) (S (b b)))) 'a)
  (check-equal? (pda-rule-read '((X a (a)) (ds ε))) 'a)
  (check-equal? (pda-rule-read '((Y b (b b)) (ds ε))) 'b)

  ;; Tests for pda-rule-pop
  (check-equal? (pda-rule-pop '((S ε ε) (X ε))) 'ε)
  (check-equal? (pda-rule-pop '((S a ε) (S (b b)))) 'ε)
  (check-equal? (pda-rule-pop '((X a (a)) (ds ε))) '(a))
  (check-equal? (pda-rule-pop '((Y b (b b)) (ds ε))) '(b b))

  ;; Tests for pda-rule-push
  (check-equal? (pda-rule-push '((S ε ε) (X ε))) 'ε)
  (check-equal? (pda-rule-push '((S a ε) (S (b b)))) '(b b))
  (check-equal? (pda-rule-push '((X a (a)) (ds ε))) 'ε)
  (check-equal? (pda-rule-push '((Y b (b b)) (ds ε))) 'ε)

  ;; Tests for tm-Edge-fromst 
  (check-equal? (tm-Edge-fromst (tm-edge 'S 'a 'Q '_)) 'S)
  (check-equal? (tm-Edge-fromst (tm-spedge 'S 'b 'A 'R)) 'S)
  (check-equal? (tm-Edge-fromst (tm-cutoff-edge 'Q '_ 'Q 'L)) 'Q)
  (check-equal? (tm-Edge-fromst (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'Q)

  ;; Tests for tm-Edge-read
  (check-equal? (tm-Edge-read (tm-edge 'S 'a 'Q '_)) 'a)
  (check-equal? (tm-Edge-read (tm-spedge 'S 'b 'A 'R)) 'b)
  (check-equal? (tm-Edge-read (tm-cutoff-edge 'Q '_ 'Q 'L)) '_)
  (check-equal? (tm-Edge-read (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'b)

  
  ;; Tests for tm-Edge-tost 
  (check-equal? (tm-Edge-tost (tm-edge 'S 'a 'Q '_)) 'Q)
  (check-equal? (tm-Edge-tost (tm-spedge 'S 'b 'A 'R)) 'A)
  (check-equal? (tm-Edge-tost (tm-cutoff-edge 'Q '_ 'Q 'L)) 'Q)
  (check-equal? (tm-Edge-tost (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'Q)

  ;; Tests for tm-Edge-action 
  (check-equal? (tm-Edge-action (tm-edge 'S 'a 'Q '_)) '_)
  (check-equal? (tm-Edge-action (tm-spedge 'S 'b 'A 'R)) 'R)
  (check-equal? (tm-Edge-action (tm-cutoff-edge 'Q '_ 'Q 'L)) 'L)
  (check-equal? (tm-Edge-action (tm-cutoff-spedge 'Q 'b 'Q 'a)) 'a)

  ;; Tests for pda-Edges-equal?
  (check-equal? (tm-Edges-equal? (tm-cutoff-edge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'E '_ 'Q 'a)) #f)
  (check-equal? (tm-Edges-equal? (tm-cutoff-spedge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'Q 'b 'Q 'b)) #f)
  (check-equal? (tm-Edges-equal? (tm-edge 'S 'a 'Q '_) (tm-cutoff-edge 'S 'a 'Q '_)) #t)
  (check-equal? (tm-Edges-equal? (tm-cutoff-spedge 'Q 'b 'Q 'a) (tm-cutoff-spedge 'Q 'b 'Q 'a)) #t)

  ;; Tests for tm-stucis-equal?
  (check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b c) 1 0)) #f)
  (check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 3 0) (tm-stuci 'S '(@ _ a b) 1 0)) #f)
  (check-equal? (tm-stucis-equal? (tm-stuci 'Q '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b c) 1 0)) #f)
  (check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 0) (tm-stuci 'S '(@ _ a b) 1 0)) #t)
  (check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ a b c) 1 10) (tm-stuci 'S '(@ a b c) 1 0)) #t)
  (check-equal? (tm-stucis-equal? (tm-stuci 'S '(@ _ a b) 1 7) (tm-stuci 'S '(@ _ a b) 1 5)) #t)

  ;; Tests for tm-rule-fromst
  (check-equal? (tm-rule-fromst '((S a) (Q _))) 'S)
  (check-equal? (tm-rule-fromst '((S b) (A R))) 'S)
  (check-equal? (tm-rule-fromst '((Q _) (Q L))) 'Q)
  (check-equal? (tm-rule-fromst '((Q b) (Q a))) 'Q)

  ;; Tests for tm-rule-tost
  (check-equal? (tm-rule-tost '((S a) (Q _))) 'Q)
  (check-equal? (tm-rule-tost '((S b) (A R))) 'A)
  (check-equal? (tm-rule-tost '((Q _) (Q L))) 'Q)
  (check-equal? (tm-rule-tost '((Q b) (Q a))) 'Q)

  
  ;; Tests for tm-rule-read
  (check-equal? (tm-rule-read '((S a) (Q _))) 'a)
  (check-equal? (tm-rule-read '((S b) (A R))) 'b)
  (check-equal? (tm-rule-read '((Q _) (Q L))) '_)
  (check-equal? (tm-rule-read '((Q b) (Q a))) 'b)

  ;; Tests for tm-rule-action
  (check-equal? (tm-rule-action '((S a) (Q _))) '_)
  (check-equal? (tm-rule-action '((S b) (A R))) 'R)
  (check-equal? (tm-rule-action '((Q _) (Q L))) 'L)
  (check-equal? (tm-rule-action '((Q b) (Q a))) 'a)

  ;; Sample tape
  (define TAPE (mcons '@
                      (mcons '_
                             (mcons 'x
                                    (mcons 'x
                                           (mcons 'a
                                                  (mcons 'x
                                                         (mcons 'x
                                                                (mcons '_ '())))))))))

  ;; Tests for mcons-set-i!
  (check-equal? (begin
                  (mcons-set-i! TAPE 4 'x)
                  TAPE)
                (mcons '@
                       (mcons '_
                              (mcons 'x
                                     (mcons 'x
                                            (mcons 'x
                                                   (mcons 'x
                                                          (mcons 'x
                                                                 (mcons '_ '())))))))))
  (check-equal? (begin
                  (mcons-set-i! TAPE 7 'x)
                  TAPE)
                (mcons '@
                       (mcons '_
                              (mcons 'x
                                     (mcons 'x
                                            (mcons 'x
                                                   (mcons 'x
                                                          (mcons 'x
                                                                 (mcons 'x '())))))))))

  

  ;; Tests for tape-at-i
  (check-equal? (tape-at-i (tm-stuci 'S TAPE 0 10)) '@)
  (check-equal? (tape-at-i (tm-stuci 'B TAPE 1 10)) '_)
  (check-equal? (tape-at-i (tm-stuci 'Q TAPE 2 10)) 'x)
  (check-equal? (tape-at-i (tm-stuci 'Q (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 8 10))
                '_)

  ;; Tests for tape-at-i
  (check-equal? (tapes-at-i (mttm-stuci 'S (list TAPE (mcons '_ '())) '(0 0) 10)) '(@ _))
  (check-equal? (tapes-at-i (mttm-stuci 'B (list TAPE (mcons '_ '()) (mcons '_ (mcons 'b (mcons 'c '())))) '(1 2 2) 10)) '(_ _ c))
  (check-equal? (tapes-at-i (mttm-stuci 'Q (list TAPE) '(2) 10)) '(x))
  (check-equal? (tapes-at-i (mttm-stuci 'Q (list (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))) '(8) 10))
                '(_))

  ;; Tests for tape-left-i
  (check-equal? (tape-left-i (tm-stuci 'B
                                       (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))) 1 10))
                '@)
  (check-equal? (tape-left-i (tm-stuci 'Q
                                       (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 2 10))
                '_)
  (check-equal? (tape-left-i (tm-stuci 'Q
                                       (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 3 10))
                'a)
  (check-equal? (tape-left-i (tm-stuci 'Q
                                       (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 0 10))
                '())

  ;; Tests for tape-right-i
  (check-equal? (tape-right-i (tm-stuci 'B
                                        (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))) 0 10))
                '_)
  (check-equal? (tape-right-i (tm-stuci 'Q
                                        (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 1 10))
                'a)
  (check-equal? (tape-right-i (tm-stuci 'Q
                                        (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 2 10))
                'b)
  (check-equal? (tape-right-i (tm-stuci 'Q
                                        (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '())))))))) 8 10))
                '_)

  ;; Tests for create-tape
  (check-equal? (create-tape '(_ x x x x x x))
                (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
  (check-equal? (create-tape '(a b c d e f))
                (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
  (check-equal? (create-tape '())
                (mcons '@ '()))
  (check-equal? (create-tape '(@ _ a b c))
                (mcons '@ (mcons '_ (mcons 'a (mcons 'b (mcons 'c '()))))))

  ;; Tests for add-blank
  (check-equal? (add-blank (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
                (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons '_ '()))))))))))
  (check-equal? (add-blank (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
                (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f (mcons '_ '())))))))))
  (check-equal? (add-blank (mcons '@ '()))
                (mcons '@ (mcons '_ '())))

  ;; Tests for create-tape-copy
  (check-equal? (create-tape-copy (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
                (mcons '@ (mcons '_ (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x (mcons 'x '())))))))))
  (check-equal? (create-tape-copy (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
                (mcons '@ (mcons 'a (mcons 'b (mcons 'c (mcons 'd (mcons 'e (mcons 'f '()))))))))
  (check-equal? (create-tape-copy (mcons '@ '()))
                (mcons '@ '()))
  )