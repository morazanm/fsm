
(module mttm racket
  (require "../constants.rkt" "../misc.rkt" "../word.rkt")
  (provide make-mttm
           mttm-get-states
           mttm-get-sigma
           mttm-get-start
           mttm-get-finals
           mttm-get-rules
           mttm-what-am-i
           mttm-get-accept
           mttm-apply
           mttm-show-transitions)

  ;; An action is either: 1. RIGHT  2. LEFT or 3. alpha

  ;; A mttm-rule is (list (list state (listof alpha|BLANK)) (list state (listof action)))


  ;; A mttm-config is (list state (list natnum (listof symbol))^+)



  ;; (listof state) (listof alpha) state state (listof mttm-rule)
  ;; Assume: Rules are for k tapes
  (define (make-mttm sts alpha start finals rules k . accept)
    (define sigma (if (member LM alpha) alpha (cons LM alpha)))
    (define accept-state (if (null? accept) (void) (car accept)))
    (define (show-transitions w t1pos)
      ;; (listof mttm-config) --> (listof tmconfig)
      (define (run paths)
        ;; (display (format "PATHS: ~s\n" paths))
        (if (empty? paths)
            (cond [(void? accept-state)
                   (error "Turing machine exhausted all computation paths without reaching a halting state.")]
                  [else (list 'reject)])
            (let* ((currpath (first paths))
                   ;(d (displayln (format "First of urrent path: ~s\n" (first currpath))))
                   (currstate (first (first currpath))))
              (if (member currstate finals)
                  (cond [(void? accept-state) currpath] ;; if not a lang recog halt
                        [(eq? currstate accept-state)   ;; if lang recog add 'accept
                         (cons 'accept currpath)]
                        [else (run (rest paths))]) ;; explore the rest of the computations
                  (let* ((pos-tapes (rest (first currpath)))
                         ;(ddd (display (format "pos-tapes: ~s\n" pos-tapes)))
                         (posns (map first pos-tapes))
                         (tapes (map second pos-tapes))
                         ;(ddd (display (format "pons: ~s\n" posns)))
                         ;(ddd (display (format "tapes: ~s\n" tapes)))
                         (reads (map (λ (p t)
                                       (if (null? t)
                                           BLANK
                                           (list-ref t p)))
                                     posns
                                     tapes))
                         ;(dd (display (format "reads: ~s\n" reads)))
                         (rls (filter (λ (r)
                                        ;(display (format "reads: ~a\n" reads))
                                        ;(display (format "rule: ~a\n" r))
                                        ;(display (format "currs: ~a\n" currstate))
                                        ;(display (format "2nd 1st r: ~a\n" (second (first r))))
                                        #;(display (format "equal: ~a\n\n" (and (equal? (first (first r))
                                                                                        currstate)
                                                                                (equal? (second (first r)) reads))))
                                        (and (equal? (first (first r)) currstate)
                                             (equal? (second (first r)) reads)))                                             
                                      rules))
                         ;(dd (display (format "rls: ~s\n" rls)))
                         (newconfigs (map (λ (r)
                                            (let* ((actions (second (second r))))
                                              (cons (first (second r))
                                                    (map (λ (p t a)
                                                           (cond [(eq? a LEFT)
                                                                  (cons (sub1 p) (list t))]
                                                                 [(eq? a RIGHT)
                                                                  (cons (add1 p)
                                                                        (if (< (add1 p) (length t)) ;; add a blank to the right if necessary
                                                                            (list t)
                                                                            (list (append t (list BLANK)))))]
                                                                 [else 
                                                                  (cons p (list (append (take t p)
                                                                                        (cons a (drop t (add1 p))))))]))
                                                         posns
                                                         tapes
                                                         actions))))
                                          rls))
                         (newpaths (map (λ (config) (cons config currpath)) newconfigs)))
                    (run (append (rest paths) newpaths)))))))
      (let* ((init-config (cons start
                                (for/list [(n k)]
                                  (if (= n 0)
                                      (list  t1pos (if (null? w) `(,BLANK) w)) ;; head pos and tape for T1
                                      (list 0 (list BLANK)))))) ;; head pos and tape for all non-T1
             (res (reverse (run (list (list init-config)))))
             ;(dddd (display (format "res: ~s\n" res)))
             )
        res))
    (lambda (mess . junk)
      (cond [(eq? mess 'get-states) sts]
            [(eq? mess 'get-sigma) (remove '@ sigma)]
            [(eq? mess 'get-start) start]
            [(eq? mess 'get-finals) finals]
            [(eq? mess 'get-rules) rules]
            [(eq? mess 'get-numtapes) k]
            [(eq? mess 'whatami)
             (if (equal? accept-state (void)) 'mttm 'mttm-language-recognizer)]
            [(eq? mess 'get-accept)
             (if (equal? accept-state (void))
                 (error "This mttm is not a language recognizer.")
                 accept-state)]
            [(eq? mess 'apply) (λ (w t1pos)
                                 (last (show-transitions w t1pos)))]
            [(eq? mess 'show-transitions) show-transitions]
            [else (format "Unknown message give to mttm: ~s" mess)])))
  

  (define (mttm-get-numtapes M) (M 'get-numtapes))
  (define (mttm-get-states M) (M 'get-states))
  (define (mttm-get-sigma M) (M 'get-sigma))
  (define (mttm-get-start M) (M 'get-start))
  (define (mttm-get-finals M) (M 'get-finals))
  (define (mttm-get-rules M) (M 'get-rules))
  (define (mttm-what-am-i M) (M 'whatami))
  (define (mttm-get-accept M) (M 'get-accept))
  (define (mttm-apply M w . t1pos) ((M 'apply) (if (empty? w) `(,BLANK) w) (if (null? t1pos) 0 (car t1pos))))
  (define (mttm-show-transitions M w . t1pos)
    ((M 'show-transitions) (if (empty? w) `(,BLANK) w) (if (null? t1pos) 0 (car t1pos))))

  
  ) ; closes module

;; 1 tape
;#;(define M1 (make-mttm '(S Y N)
;                        `(a b ,LM)
;                        'S
;                        '(Y N)
;                        '(((S (a)) (S (R)))
;                          ((S (b)) (N (b)))
;                          ((S (_)) (Y (_))))
;                        #;(list (list (list 'S (list 'a))    (list 'S (list RIGHT)))
;                                (list (list 'S (list 'b))    (list 'N (list 'b)))
;                                (list (list 'S (list BLANK)) (list 'Y (list BLANK))))
;                        1
;                        'Y))
;;; L = a*
;;; two tapes
;(define M1 (make-mttm '(S Y N)
;                      `(a b ,LM)
;                      'S
;                      '(Y N)
;                      '(((S (a _)) (S (R R)))
;                        ((S (b _)) (N (b _)))
;                        ((S (_ _)) (Y (_ _))))
;                      2
;                      'Y))
;
;;; Add (ok, this is a dumb way to do add but for testing it's great)
;;; three tapes
;;; PRE: _A_B T1 head starts at position 0 (blank before A)
;;;      _
;;;      _
;;; POST: A+B (on T1)
;;; HOW: Copy A to T2, Copy B to T3, COPY T2 and T3 to T1
;(define ADD (make-mttm '(S A T U V)
;                       `(I)
;                       'S
;                       '(H)
;                       `(((S (,BLANK ,BLANK ,BLANK)) (A (R R R)))
;                         ((A (I ,BLANK ,BLANK)) (A (,BLANK I ,BLANK)))
;                         ((A (,BLANK I ,BLANK)) (A (R R ,BLANK)))
;                         ((A (,BLANK ,BLANK ,BLANK)) (T (R ,BLANK ,BLANK)))
;                         ((T (I ,BLANK ,BLANK)) (T (,BLANK ,BLANK I)))
;                         ((T (,BLANK ,BLANK I)) (T (R ,BLANK R)))
;                         ((T (,BLANK ,BLANK ,BLANK)) (Q (L ,BLANK ,BLANK)))
;                         ((Q (,BLANK ,BLANK ,BLANK)) (U (L L L)))
;                         ((U (,BLANK I I)) (U (I I ,BLANK)))
;                         ((U (I I ,BLANK)) (U (L I L)))
;                         ((U (,BLANK I ,BLANK)) (V (,BLANK I ,BLANK)))
;                         ((V (,BLANK I ,BLANK)) (V (I ,BLANK ,BLANK)))
;                         ((V (I ,BLANK ,BLANK)) (V (L L ,BLANK)))
;                         ((V (,BLANK ,BLANK ,BLANK)) (H (,BLANK ,BLANK ,BLANK))))
;                       3))
;
;;; a^nb^nc^nd^n
;;; four tapes
;;; PRE: _w_ T1 head starts at position 0 (blank before w)
;;;      _
;;;      _
;;;      _
;;; POST: Y for accept and N for reject
;;; HOW: Skip a's; copy b's, c's, d's to T2, T3, and T4. move all heaps to
;;;      starting blank, match a's, b's, c's, and d's
;(define a^nb^nc^nd^n (make-mttm
;                      '(S A Y N)
;                      '(a b c d)
;                      'S
;                      '(Y N)
;                      `(;; Move to pos 1 on all heads
;                        ((S (,BLANK ,BLANK ,BLANK ,BLANK)) (Q (R R R R)))
;                        ;; check for empty
;                        ((Q (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK)))
;                        ((Q (a ,BLANK ,BLANK ,BLANK)) (A (a ,BLANK ,BLANK ,BLANK)))
;                        ;; no a's reject
;                        ((Q (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
;                        ((Q (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
;                        ((Q (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
;                        ;; Skip the a's on T1
;                        ((A (a ,BLANK ,BLANK ,BLANK)) (A (R ,BLANK ,BLANK ,BLANK)))
;                        ((A (b ,BLANK ,BLANK ,BLANK)) (B (b ,BLANK ,BLANK ,BLANK)))
;                        ;; No b's reject
;                        ((A (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
;                        ((A (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
;                        ((A (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
;                        ;; Copy the b's on T1 to T2
;                        ((B (b ,BLANK ,BLANK ,BLANK)) (B (b b ,BLANK ,BLANK)))
;                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
;                        ((B (b b ,BLANK ,BLANK)) (B (R R ,BLANK ,BLANK)))
;                        ((B (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK ,BLANK ,BLANK)))
;                        ;; No c's reject
;                        ((B (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
;                        ((B (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
;                        ((B (d ,BLANK ,BLANK ,BLANK)) (N (d ,BLANK ,BLANK ,BLANK)))
;                        ;; Copy the c's on T1 to T3
;                        ((C (c ,BLANK ,BLANK ,BLANK)) (C (c ,BLANK c ,BLANK)))
;                        ((C (c ,BLANK c ,BLANK)) (C (R ,BLANK R ,BLANK)))
;                        ((C (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK ,BLANK)))
;                        ;; No d's reject
;                        ((C (,BLANK ,BLANK ,BLANK ,BLANK)) (N (,BLANK ,BLANK ,BLANK ,BLANK)))
;                        ((C (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
;                        ((C (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
;                        ;; Copy the d's on T1 to T4
;                        ((D (d ,BLANK ,BLANK ,BLANK)) (D (d ,BLANK ,BLANK d)))
;                        ((D (d ,BLANK ,BLANK d)) (D (R ,BLANK ,BLANK R)))
;                        ((D (,BLANK ,BLANK ,BLANK ,BLANK)) (E (L L L L)))
;                        ;; non d reject
;                        ((D (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
;                        ((D (b ,BLANK ,BLANK ,BLANK)) (N (b ,BLANK ,BLANK ,BLANK)))
;                        ((D (c ,BLANK ,BLANK ,BLANK)) (N (c ,BLANK ,BLANK ,BLANK)))
;                        ;; Match a's, b's, c's, and d's to accept. Otherwise, reject if BLANK read and num of BLANK is < 4
;                        ((E (,BLANK ,BLANK ,BLANK ,BLANK)) (Y (,BLANK ,BLANK ,BLANK ,BLANK))) ;; accept
;                        ((E (d b c d)) (E (L b c d))) ;; skip d's on T1
;                        ((E (c b c d)) (E (L b c d))) ;; skip c's on T1 
;                        ((E (b b c d)) (E (L b c d))) ;; skip c's on T1
;                        ((E (a b c d)) (E (L L L L))) ;; on match move all heads L
;                        ;; Wrong number of blanks reject
;                        ((E (,BLANK b c d)) (N (,BLANK b c d)))
;                        ((E (a ,BLANK c d)) (N (a ,BLANK c d)))
;                        ((E (a b ,BLANK d)) (N (a b ,BLANK d)))
;                        ((E (a b c ,BLANK)) (N (a b c ,BLANK)))
;                        ((E (,BLANK ,BLANK c d)) (N (,BLANK ,BLANK c d)))
;                        ((E (,BLANK b ,BLANK d)) (N (,BLANK b ,BLANK d)))
;                        ((E (,BLANK b c ,BLANK)) (N (,BLANK b c ,BLANK)))
;                        ((E (a ,BLANK ,BLANK d)) (N (a ,BLANK ,BLANK d)))
;                        ((E (a ,BLANK c ,BLANK)) (N (a ,BLANK c ,BLANK)))
;                        ((E (a b ,BLANK ,BLANK)) (N (a b ,BLANK ,BLANK)))
;                        ((E (a ,BLANK ,BLANK ,BLANK)) (N (a ,BLANK ,BLANK ,BLANK)))
;                        ((E (,BLANK b ,BLANK ,BLANK)) (N (,BLANK b ,BLANK ,BLANK)))
;                        ((E (,BLANK ,BLANK c ,BLANK)) (N (,BLANK ,BLANK c ,BLANK)))
;                        ((E (,BLANK ,BLANK ,BLANK d)) (N (,BLANK ,BLANK ,BLANK d))))
;                      4
;                      'Y))
;
;                        
;                      
;
;
;;(M1-transitions '(a a a) 'S '(Y N) '(((S (a)) (S (R))) ((S (b)) (N (b))) ((S (_)) (Y (_)))) 'Y)
;
;(check-expect (mttm-get-states M1) '(S Y N))
;(check-expect (mttm-get-sigma M1) '(a b))
;(check-expect (mttm-get-start M1) 'S)
;(check-expect (mttm-get-finals M1) '(Y N))
;(check-expect (mttm-get-accept M1) 'Y)
;(check-expect (mttm-get-rules M1)
;              '(((S (a _)) (S (R R)))
;                ((S (b _)) (N (b _)))
;                ((S (_ _)) (Y (_ _)))))
;(check-expect (mttm-what-am-i M1) 'mttm-language-recognizer)
;(check-expect (mttm-apply M1 '(a a a a)) 'accept)
;(check-expect (mttm-apply M1 '()) 'accept)
;(check-expect (mttm-apply M1 '(a a b a a)) 'reject)
;(check-expect (mttm-show-transitions M1 '(a a a))
;              '((S (0 (a a a)) (0 (_)))
;                (S (1 (a a a)) (1 (_ _)))
;                (S (2 (a a a)) (2 (_ _ _)))
;                (S (3 (a a a _)) (3 (_ _ _ _)))
;                (Y (3 (a a a _)) (3 (_ _ _ _)))
;                accept))
;(check-expect (mttm-show-transitions M1 '(a a a b a))
;              '((S (0 (a a a b a)) (0 (_)))
;                (S (1 (a a a b a)) (1 (_ _)))
;                (S (2 (a a a b a)) (2 (_ _ _)))
;                (S (3 (a a a b a)) (3 (_ _ _ _)))
;                (N (3 (a a a b a)) (3 (_ _ _ _)))
;                reject))
;
;(check-expect (mttm-show-transitions M1 '())
;              '((S (0 (_)) (0 (_)))
;                (Y (0 (_)) (0 (_)))
;                accept))
;
;(check-expect (mttm-apply ADD `(,BLANK I I I ,BLANK I I))
;              '(H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _))))
;
;(check-expect (mttm-show-transitions ADD `(,BLANK I I I ,BLANK I I))
;              '((S (0 (_ I I I _ I I)) (0 (_)) (0 (_)))
;                (A (1 (_ I I I _ I I)) (1 (_ _)) (1 (_ _)))
;                (A (1 (_ _ I I _ I I)) (1 (_ I)) (1 (_ _)))
;                (A (2 (_ _ I I _ I I)) (2 (_ I _)) (1 (_ _)))
;                (A (2 (_ _ _ I _ I I)) (2 (_ I I)) (1 (_ _)))
;                (A (3 (_ _ _ I _ I I)) (3 (_ I I _)) (1 (_ _)))
;                (A (3 (_ _ _ _ _ I I)) (3 (_ I I I)) (1 (_ _)))
;                (A (4 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
;                (T (5 (_ _ _ _ _ I I)) (4 (_ I I I _)) (1 (_ _)))
;                (T (5 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (1 (_ I)))
;                (T (6 (_ _ _ _ _ _ I)) (4 (_ I I I _)) (2 (_ I _)))
;                (T (6 (_ _ _ _ _ _ _)) (4 (_ I I I _)) (2 (_ I I)))
;                (T (7 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
;                (Q (6 (_ _ _ _ _ _ _ _)) (4 (_ I I I _)) (3 (_ I I _)))
;                (U (5 (_ _ _ _ _ _ _ _)) (3 (_ I I I _)) (2 (_ I I _)))
;                (U (5 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (2 (_ I _ _)))
;                (U (4 (_ _ _ _ _ I _ _)) (3 (_ I I I _)) (1 (_ I _ _)))
;                (U (4 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (1 (_ _ _ _)))
;                (U (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
;                (V (3 (_ _ _ _ I I _ _)) (3 (_ I I I _)) (0 (_ _ _ _)))
;                (V (3 (_ _ _ I I I _ _)) (3 (_ I I _ _)) (0 (_ _ _ _)))
;                (V (2 (_ _ _ I I I _ _)) (2 (_ I I _ _)) (0 (_ _ _ _)))
;                (V (2 (_ _ I I I I _ _)) (2 (_ I _ _ _)) (0 (_ _ _ _)))
;                (V (1 (_ _ I I I I _ _)) (1 (_ I _ _ _)) (0 (_ _ _ _)))
;                (V (1 (_ I I I I I _ _)) (1 (_ _ _ _ _)) (0 (_ _ _ _)))
;                (V (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))
;                (H (0 (_ I I I I I _ _)) (0 (_ _ _ _ _)) (0 (_ _ _ _)))))
;
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK a a b b c c d d)) 'accept)
;(check-expect (mttm-show-transitions a^nb^nc^nd^n `(,BLANK a a b b c c d d))
;              '((S (0 (_ a a b b c c d d)) (0 (_)) (0 (_)) (0 (_)))
;                (Q (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (1 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (2 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (3 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (B (3 (_ a a b b c c d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (B (3 (_ a a b b c c d d)) (1 (_ b)) (1 (_ _)) (1 (_ _)))
;                (B (4 (_ a a b b c c d d)) (2 (_ b _)) (1 (_ _)) (1 (_ _)))
;                (B (4 (_ a a b b c c d d)) (2 (_ b b)) (1 (_ _)) (1 (_ _)))
;                (B (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
;                (C (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
;                (C (5 (_ a a b b c c d d)) (3 (_ b b _)) (1 (_ c)) (1 (_ _)))
;                (C (6 (_ a a b b c c d d)) (3 (_ b b _)) (2 (_ c _)) (1 (_ _)))
;                (C (6 (_ a a b b c c d d)) (3 (_ b b _)) (2 (_ c c)) (1 (_ _)))
;                (C (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ _)))
;                (D (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ _)))
;                (D (7 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (1 (_ d)))
;                (D (8 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (2 (_ d _)))
;                (D (8 (_ a a b b c c d d)) (3 (_ b b _)) (3 (_ c c _)) (2 (_ d d)))
;                (D (9 (_ a a b b c c d d _)) (3 (_ b b _)) (3 (_ c c _)) (3 (_ d d _)))
;                (E (8 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (7 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (6 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (5 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (4 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (3 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (2 (_ a a b b c c d d _)) (2 (_ b b _)) (2 (_ c c _)) (2 (_ d d _)))
;                (E (1 (_ a a b b c c d d _)) (1 (_ b b _)) (1 (_ c c _)) (1 (_ d d _)))
;                (E (0 (_ a a b b c c d d _)) (0 (_ b b _)) (0 (_ c c _)) (0 (_ d d _)))
;                (Y (0 (_ a a b b c c d d _)) (0 (_ b b _)) (0 (_ c c _)) (0 (_ d d _)))
;                accept))
;
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK a a a a b b b b c c c c d d d))
;              'reject)
;
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK b b b c c c d d d))
;              'reject)
;
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK a a b c d d))
;              'reject)
;
;(check-expect (mttm-show-transitions a^nb^nc^nd^n `(,BLANK a a a a b b b b c c c c d d d))
;              '((S (0 (_ a a a a b b b b c c c c d d d)) (0 (_)) (0 (_)) (0 (_)))
;                (Q (1 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (1 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (2 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (3 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (4 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (A (5 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (B (5 (_ a a a a b b b b c c c c d d d)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (B (5 (_ a a a a b b b b c c c c d d d)) (1 (_ b)) (1 (_ _)) (1 (_ _)))
;                (B (6 (_ a a a a b b b b c c c c d d d)) (2 (_ b _)) (1 (_ _)) (1 (_ _)))
;                (B (6 (_ a a a a b b b b c c c c d d d)) (2 (_ b b)) (1 (_ _)) (1 (_ _)))
;                (B (7 (_ a a a a b b b b c c c c d d d)) (3 (_ b b _)) (1 (_ _)) (1 (_ _)))
;                (B (7 (_ a a a a b b b b c c c c d d d)) (3 (_ b b b)) (1 (_ _)) (1 (_ _)))
;                (B (8 (_ a a a a b b b b c c c c d d d)) (4 (_ b b b _)) (1 (_ _)) (1 (_ _)))
;                (B (8 (_ a a a a b b b b c c c c d d d)) (4 (_ b b b b)) (1 (_ _)) (1 (_ _)))
;                (B (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ _)) (1 (_ _)))
;                (C (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ _)) (1 (_ _)))
;                (C (9 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (1 (_ c)) (1 (_ _)))
;                (C (10 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (2 (_ c _)) (1 (_ _)))
;                (C (10 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (2 (_ c c)) (1 (_ _)))
;                (C (11 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (3 (_ c c _)) (1 (_ _)))
;                (C (11 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (3 (_ c c c)) (1 (_ _)))
;                (C (12 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (4 (_ c c c _)) (1 (_ _)))
;                (C (12 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (4 (_ c c c c)) (1 (_ _)))
;                (C (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ _)))
;                (D (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ _)))
;                (D (13 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (1 (_ d)))
;                (D (14 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (2 (_ d _)))
;                (D (14 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (2 (_ d d)))
;                (D (15 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (3 (_ d d _)))
;                (D (15 (_ a a a a b b b b c c c c d d d)) (5 (_ b b b b _)) (5 (_ c c c c _)) (3 (_ d d d)))
;                (D
;                 (16 (_ a a a a b b b b c c c c d d d _))
;                 (5 (_ b b b b _))
;                 (5 (_ c c c c _))
;                 (4 (_ d d d _)))
;                (E
;                 (15 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E
;                 (14 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E
;                 (13 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E
;                 (12 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E
;                 (11 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E
;                 (10 (_ a a a a b b b b c c c c d d d _))
;                 (4 (_ b b b b _))
;                 (4 (_ c c c c _))
;                 (3 (_ d d d _)))
;                (E (9 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (8 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (7 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (6 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (5 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (4 (_ a a a a b b b b c c c c d d d _)) (4 (_ b b b b _)) (4 (_ c c c c _)) (3 (_ d d d _)))
;                (E (3 (_ a a a a b b b b c c c c d d d _)) (3 (_ b b b b _)) (3 (_ c c c c _)) (2 (_ d d d _)))
;                (E (2 (_ a a a a b b b b c c c c d d d _)) (2 (_ b b b b _)) (2 (_ c c c c _)) (1 (_ d d d _)))
;                (E (1 (_ a a a a b b b b c c c c d d d _)) (1 (_ b b b b _)) (1 (_ c c c c _)) (0 (_ d d d _)))
;                (N (1 (_ a a a a b b b b c c c c d d d _)) (1 (_ b b b b _)) (1 (_ c c c c _)) (0 (_ d d d _)))
;                reject))
;
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK a b c)) 'reject)
;(check-expect (mttm-apply a^nb^nc^nd^n `(,BLANK a b c d b)) 'reject)
;(check-expect (mttm-show-transitions a^nb^nc^nd^n '())
;              '((S (0 (_)) (0 (_)) (0 (_)) (0 (_)))
;                (Q (1 (_ _)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                (Y (1 (_ _)) (1 (_ _)) (1 (_ _)) (1 (_ _)))
;                accept))
;
;(test)



