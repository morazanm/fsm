
#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         "david-imsg-state.rkt"
         "david-viz-constants.rkt"
         "../../fsm-core/private/constants.rkt")

(provide ndfa-create-draw-informative-message
         pda-create-draw-informative-message
         tm-create-draw-informative-message
         mttm-create-draw-informative-message)

(define FONT-SIZE 20)

(define DUMMY-TM-RULE '(@ @))

(define MAX-AUX-TAPE-AMOUNT 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tape-img tape start-index color-pair color-scheme)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let* ([BLANK-COLOR (color-palette-blank-color color-scheme)]
         [letter-imgs
          (build-list
           TAPE-SIZE
           (λ (i)
             (if (< (+ start-index i) (length tape))
                 (let [(tape-element (list-ref tape (+ start-index i)))
                       (FONT-COLOR (color-palette-font-color color-scheme))]
                   (overlay (text (if (symbol? tape-element)
                                      (symbol->string tape-element)
                                      (number->string tape-element))
                                  FONT-SIZE
                                  (cond [(empty? color-pair) (color-palette-font-color color-scheme)]
                                        [(and (not (empty? (first color-pair)))
                                              (< (+ start-index i) (first (first color-pair))))
                                         (second (first color-pair))]
                                        [(and (not (empty? (second color-pair)))
                                              (= (+ start-index i) (first (second color-pair))))
                                         (second (second color-pair))]
                                        [else FONT-COLOR]))
                            (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR))))
                 (overlay (square 21 'solid BLANK-COLOR) (square (add1 21) 'solid BLANK-COLOR)))))])
    (make-tape-img letter-imgs start-index)))


(define (draw-imsg imsg-st)
  (let* [(tape (zipper-current (imsg-state-tm-tape imsg-st)))
         (start-index (if (> (length tape) TM-TAPE-SIZE)
                          (imsg-state-tm-word-img-offset imsg-st)
                          0))
         (head-pos (if (or (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                           (zipper-empty? (imsg-state-tm-rules-used imsg-st)))
                       0
                       (zipper-current (imsg-state-tm-head-position imsg-st))))
         (FONT-COLOR (color-palette-font-color (imsg-state-tm-color-pallete imsg-st)))
         (BLANK-COLOR (color-palette-blank-color (imsg-state-tm-color-pallete imsg-st)))
         (REJECT-COLOR (color-palette-imsg-reject-color (imsg-state-tm-color-pallete imsg-st)))]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid BLANK-COLOR)
                 (text (number->string start-index) 10 FONT-COLOR))
          (beside (above (first loi)
                         (square 5 'solid BLANK-COLOR)
                         (text (number->string start-index) 10 FONT-COLOR))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TM-TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   [else FONT-COLOR]))
                                                       (overlay (square 50 'solid BLANK-COLOR)
                                                                (square (add1 50) 'solid
                                                                        FONT-COLOR)))
                                              (overlay (text (symbol->string BLANK)
                                                             24
                                                             (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   
                                                                   [else FONT-COLOR]))
                                                       (square 50 'solid BLANK-COLOR)
                                                       (square (add1 50) 'solid
                                                               FONT-COLOR))))))]
      (make-tape-img letter-imgs start-index))))

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* ([upci (ci-upci (zipper-current (imsg-state-ndfa-ci imsg-st)))]
         [pci (ci-pci (zipper-current (imsg-state-ndfa-ci imsg-st)))]
         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append pci upci)]
         [pci-length (length pci)]
         [sub1-pci-length (sub1 pci-length)]
         [FONT-COLOR (color-palette-font-color (imsg-state-ndfa-color-pallete imsg-st))]
         [BLANK-COLOR (color-palette-blank-color (imsg-state-ndfa-color-pallete imsg-st))]
         [REJECT-COLOR (color-palette-imsg-reject-color (imsg-state-ndfa-color-pallete imsg-st))]
         [ACCEPT-COLOR (color-palette-imsg-accept-color (imsg-state-ndfa-color-pallete imsg-st))]
         [FADED-WORD-COLOR (color-palette-faded-word-color (imsg-state-ndfa-color-pallete imsg-st))]
         [COMPUTATION-LENGTH-COLOR (color-palette-computation-length-color (imsg-state-ndfa-color-pallete imsg-st))]
         [machine-decision (if (imsg-state-ndfa-accepted? imsg-st) 'accept 'reject)])
   (above/align
      'left
      (cond [(and (empty? pci)
                  (empty? upci))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FADED-WORD-COLOR)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE ACCEPT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (eq? machine-decision 'reject)
                  (equal? upci (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input imsg-st))))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-ndfa-word-img-offset imsg-st)
                                         0)
                                     (if (empty? pci)
                                         '()
                                         (list (list sub1-pci-length FADED-WORD-COLOR)
                                               (list sub1-pci-length REJECT-COLOR)))
                                     (imsg-state-ndfa-color-pallete imsg-st)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? pci)
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img (take pci sub1-pci-length)
                                         (if (> sub1-pci-length TAPE-SIZE)
                                             (imsg-state-ndfa-word-img-offset imsg-st)
                                             0)
                                         '()
                                         (imsg-state-ndfa-color-pallete imsg-st)))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? pci)
                                                          '()
                                                          (list (list (length pci) FADED-WORD-COLOR) '()))
                                                      (imsg-state-ndfa-color-pallete imsg-st)))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img pci
                                                      (if (> (length pci) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0) 
                                                      '()
                                                      (imsg-state-ndfa-color-pallete imsg-st))))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (hash-ref (imsg-state-ndfa-computation-lengths imsg-st)
                                              upci
                                              0)))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
      (cond [(and (empty? upci) (eq? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (empty? upci) (eq? machine-decision 'reject)
                   (not (empty? (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input imsg-st)))))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [(and (eq? machine-decision 'reject) 
                  (equal? upci (ndfa-config-word (imsg-state-ndfa-farthest-consumed-input imsg-st))))
              (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))


(define (pda-create-draw-informative-message imsg-st)
  (let* ([upci (ci-upci (zipper-current (imsg-state-pda-ci imsg-st)))]
         [pci (ci-pci (zipper-current (imsg-state-pda-ci imsg-st)))]
         ;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append pci upci)]
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (pda-config-stack (zipper-current (imsg-state-pda-stack imsg-st)))]
         [machine-decision (if (imsg-state-pda-accepted? imsg-st) 'accept 'reject)]
         [farthest-consumed-input (pda-config-word (imsg-state-pda-farthest-consumed-input imsg-st))]
         [computation-has-cut-off? (imsg-state-pda-computation-has-cut-off? imsg-st)]
         [FONT-COLOR (color-palette-font-color (imsg-state-pda-color-pallete imsg-st))]
         [BLANK-COLOR (color-palette-blank-color (imsg-state-pda-color-pallete imsg-st))]
         [REJECT-COLOR (color-palette-imsg-reject-color (imsg-state-pda-color-pallete imsg-st))]
         [ACCEPT-COLOR (color-palette-imsg-accept-color (imsg-state-pda-color-pallete imsg-st))]
         [FADED-WORD-COLOR (color-palette-faded-word-color (imsg-state-pda-color-pallete imsg-st))]
         [COMPUTATION-LENGTH-COLOR (color-palette-computation-length-color (imsg-state-pda-color-pallete imsg-st))]
         [CUT-OFF-COLOR (color-palette-ismg-cut-off-color (imsg-state-pda-color-pallete imsg-st))]
         [FONT-SIZE 20])
    ;(displayln 
    (above/align
      'left
      (cond [(and (empty? pci)
                  (empty? upci))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FADED-WORD-COLOR)
                          (text (format "~a" EMP) FONT-SIZE REJECT-COLOR)))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE FONT-COLOR)
                          (text (format "~a" EMP) FONT-SIZE BLANK-COLOR))))]
            [(and (zipper-at-end? (imsg-state-pda-shown-accepting-trace imsg-st))
                  (equal? upci farthest-consumed-input)
                  computation-has-cut-off?)
             (let* ([pci-length (length pci)]
                    [sub1-pci-length (sub1 pci-length)])
               (above/align 'left
                          (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                  (text "Word: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? pci)
                                                     '()
                                                     (list (list sub1-pci-length FADED-WORD-COLOR)
                                                           (list sub1-pci-length CUT-OFF-COLOR)))
                                                 (imsg-state-pda-color-pallete imsg-st)))
                          (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img (take pci sub1-pci-length)
                                                 (if (> sub1-pci-length
                                                         TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 '()
                                                 (imsg-state-pda-color-pallete imsg-st)))))]
            [(and (equal? upci farthest-consumed-input)
                  (eq? machine-decision 'reject))
             (let* ([pci-length (length pci)]
                    [sub1-pci-length (sub1 pci-length)])
               (above/align
                'left
                (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                        (text "Word: " FONT-SIZE FONT-COLOR)
                        (make-tape-img entire-word
                                       (if (> (length entire-word) TAPE-SIZE)
                                           (imsg-state-pda-word-img-offset imsg-st)
                                           0)
                                       (if (empty? pci)
                                           '()
                                           (list (list sub1-pci-length FADED-WORD-COLOR)
                                                 (list sub1-pci-length REJECT-COLOR)))
                                       (imsg-state-pda-color-pallete imsg-st)))
                (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                        (if (empty? pci)
                            (text "" FONT-SIZE FONT-COLOR)
                            (make-tape-img (take pci sub1-pci-length)
                                           (if (> pci-length TAPE-SIZE)
                                               (imsg-state-pda-word-img-offset imsg-st)
                                               0)
                                           '()
                                           (imsg-state-pda-color-pallete imsg-st))))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? pci)
                                                          '()
                                                          (list (list (length pci) FADED-WORD-COLOR) '()))
                                                      (imsg-state-pda-color-pallete imsg-st)))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img pci
                                                      (if (> (length pci) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      '()
                                                      (imsg-state-pda-color-pallete imsg-st))))])
      (cond [(zipper-empty? (imsg-state-pda-stack imsg-st)) (text "aaaC" FONT-SIZE BLANK-COLOR)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                                            (text "Stack: " FONT-SIZE FONT-COLOR))]
            [else (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                          (text "Stack: " FONT-SIZE FONT-COLOR)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         '()
                                         (imsg-state-pda-color-pallete imsg-st)))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (hash-ref (imsg-state-pda-computation-lengths imsg-st)
                                              upci
                                              0)))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (zipper-at-end? (imsg-state-pda-shown-accepting-trace imsg-st))
                  (equal? upci farthest-consumed-input)
                  computation-has-cut-off?)
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-pda-max-cmps imsg-st)) FONT-SIZE CUT-OFF-COLOR)]
            [(and (empty? upci)
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (equal? upci farthest-consumed-input)
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [(and (empty? upci)
                  (zipper-at-end? (imsg-state-pda-stack imsg-st))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))

(define (tm-create-draw-informative-message imsg-st)
  (let ([FONT-COLOR (color-palette-font-color (imsg-state-tm-color-pallete imsg-st))]
        [BLANK-COLOR (color-palette-blank-color (imsg-state-tm-color-pallete imsg-st))]
        [REJECT-COLOR (color-palette-imsg-reject-color (imsg-state-tm-color-pallete imsg-st))]
        [ACCEPT-COLOR (color-palette-imsg-accept-color (imsg-state-tm-color-pallete imsg-st))]
        [FADED-WORD-COLOR (color-palette-faded-word-color (imsg-state-tm-color-pallete imsg-st))]
        [COMPUTATION-LENGTH-COLOR (color-palette-computation-length-color (imsg-state-tm-color-pallete imsg-st))]
        [CUT-OFF-COLOR (color-palette-ismg-cut-off-color (imsg-state-tm-color-pallete imsg-st))])
    (above/align
      'left
      (if (zipper-empty? (imsg-state-tm-rules-used imsg-st))
          (text "Head position is not updated when there are multiple rejecting computations." FONT-SIZE FONT-COLOR)
          (beside (text "Last rule used: " FONT-SIZE FONT-COLOR)
                  (text (format "~a" (if (or (equal? (zipper-current (imsg-state-tm-rules-used imsg-st)) DUMMY-TM-RULE)
                                             (zipper-empty? (imsg-state-tm-rules-used imsg-st)))
                                         ""
                                         (zipper-current (imsg-state-tm-rules-used imsg-st))))
                        FONT-SIZE
                        (if (equal? (imsg-state-tm-machine-decision imsg-st) 'accept)
                            ACCEPT-COLOR
                            REJECT-COLOR))))
              
      (text "Tape: " 1 BLANK-COLOR)
      (draw-imsg imsg-st)  
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (zipper-current (imsg-state-tm-computation-lengths imsg-st))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (zipper-at-end? (imsg-state-tm-shown-accepting-trace imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'reject)
                  (>= (get-tm-config-index-frm-trace (imsg-state-tm-shown-accepting-trace imsg-st)) (imsg-state-tm-max-cmps imsg-st))
                  (not (equal? (tm-config-state (trace-config (zipper-current (imsg-state-tm-shown-accepting-trace imsg-st))))
                               (tm-accepting-final (imsg-state-tm-M imsg-st)))))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-tm-max-cmps imsg-st)) FONT-SIZE CUT-OFF-COLOR)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'accept)
                  (eq? (tm-type (imsg-state-tm-M imsg-st)) 'tm-language-recognizer))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'reject)
                  (eq? (tm-type (imsg-state-tm-M imsg-st)) 'tm-language-recognizer))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'reached-final)
                  (eq? (tm-type (imsg-state-tm-M imsg-st)) 'tm))
             (text "The machine reaches a final state and halts." FONT-SIZE ACCEPT-COLOR)]
            [(and (zipper-at-end? (imsg-state-tm-tape imsg-st))
                  (zipper-at-end? (imsg-state-tm-head-position imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'halted)
                  (eq? (tm-type (imsg-state-tm-M imsg-st)) 'tm))
             (text "The machine did not reach a halting state." FONT-SIZE ACCEPT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))

(define (mttm-create-draw-informative-message imsg-st)
  (let ([FONT-COLOR (color-palette-font-color (imsg-state-mttm-color-pallete imsg-st))]
        [BLANK-COLOR (color-palette-blank-color (imsg-state-mttm-color-pallete imsg-st))]
        [REJECT-COLOR (color-palette-imsg-reject-color (imsg-state-mttm-color-pallete imsg-st))]
        [ACCEPT-COLOR (color-palette-imsg-accept-color (imsg-state-mttm-color-pallete imsg-st))]
        [FADED-WORD-COLOR (color-palette-faded-word-color (imsg-state-mttm-color-pallete imsg-st))]
        [COMPUTATION-LENGTH-COLOR (color-palette-computation-length-color (imsg-state-mttm-color-pallete imsg-st))]
        [CUT-OFF-COLOR (color-palette-ismg-cut-off-color (imsg-state-mttm-color-pallete imsg-st))])
    (define (draw-tape tape head-pos)
      (let [(start-index (if (> (length tape) TM-TAPE-SIZE)
                             (imsg-state-mttm-word-img-offset imsg-st)
                             0))]
        (define (make-tape-img loi start-index)
          (if (empty? (rest loi))
              (above (first loi)
                     (square 5 'solid BLANK-COLOR)
                     (text (number->string start-index) 10 FONT-COLOR))
              (beside (above (first loi)
                             (square 5 'solid BLANK-COLOR)
                             (text (number->string start-index) 10 FONT-COLOR))
                      (make-tape-img (rest loi) (add1 start-index)))))
        (let [(letter-imgs (build-list TM-TAPE-SIZE
                                       (λ (i) (if (< (+ start-index i) (length tape))
                                                  (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                                 20 #;24
                                                                 (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                       [else FONT-COLOR]))
                                                           (overlay (square 40 #;50 'solid BLANK-COLOR)
                                                                    (square (add1 40 #;50) 'solid
                                                                            FONT-COLOR)))
                                                  (overlay (text (symbol->string BLANK)
                                                                 24
                                                                 (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   
                                                                       [else FONT-COLOR]))
                                                           (square 40 #;50 'solid BLANK-COLOR)
                                                           (square (add1 40 #;50) 'solid
                                                                   FONT-COLOR))))))]
          (make-tape-img letter-imgs start-index))))

  
    (define (make-tapes aux-tape-index max-aux-tapes-index)
      (let ([tapes (zipper-current (imsg-state-mttm-tapes imsg-st))]
            [head-positions (zipper-current (imsg-state-mttm-head-positions imsg-st))])
        (if (or (= aux-tape-index max-aux-tapes-index) (= aux-tape-index (sub1 (mttm-tape-amount (imsg-state-mttm-M imsg-st)))))
            (beside
             (text (format "T~s: " aux-tape-index) 20 FONT-COLOR)
             (draw-tape (list-ref tapes aux-tape-index) (list-ref head-positions aux-tape-index)))
            (above
             (beside
              (text (format "T~s: " aux-tape-index) 20 FONT-COLOR)
              (draw-tape (list-ref tapes aux-tape-index) (list-ref head-positions aux-tape-index)))
             (make-tapes (add1 aux-tape-index) max-aux-tapes-index)))))
  
    (let ([main-tape-img (beside (text "T0: " 20 'black)
                                 (draw-tape (first (zipper-current (imsg-state-mttm-tapes imsg-st)))
                                            (let ([head-pos (zipper-current (imsg-state-mttm-head-positions imsg-st))])
                                              (if (empty? head-pos)
                                                  1
                                                  (first (zipper-current (imsg-state-mttm-head-positions imsg-st)))))))])
      (above/align
       'left
       (beside (text "Last rule used: " FONT-SIZE FONT-COLOR)
               (text (format "~a" (if (or (equal? (zipper-current (imsg-state-mttm-rules-used imsg-st)) DUMMY-TM-RULE)
                                          (zipper-empty? (imsg-state-mttm-rules-used imsg-st)))
                                      ""
                                      (zipper-current (imsg-state-mttm-rules-used imsg-st))))
                     FONT-SIZE
                     (if (equal? (imsg-state-mttm-machine-decision imsg-st) 'accept)
                         ACCEPT-COLOR
                         REJECT-COLOR)))         
       (text "Tape: " 1 BLANK-COLOR)
       (above main-tape-img
              (make-tapes (imsg-state-mttm-aux-tape-index imsg-st)
                          (+ (imsg-state-mttm-aux-tape-index imsg-st) MAX-AUX-TAPE-AMOUNT))) 
       (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                     (number->string (zipper-current (imsg-state-mttm-computation-lengths imsg-st))))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
       (cond [(and (not (zipper-empty? (imsg-state-mttm-shown-rejecting-trace imsg-st)))
                   (zipper-at-end? (imsg-state-mttm-shown-rejecting-trace imsg-st))
                   (>= (get-mttm-config-index-frm-trace (imsg-state-mttm-shown-rejecting-trace imsg-st)) (imsg-state-mttm-max-cmps imsg-st))
                   (not (equal? (mttm-config-state (trace-config (zipper-current (imsg-state-mttm-shown-rejecting-trace imsg-st))))
                                (mttm-accepting-final (imsg-state-mttm-M imsg-st)))))
              (text (format "There are computations that exceed the cut-off limit (~a)."
                            (imsg-state-mttm-max-cmps imsg-st)) FONT-SIZE CUT-OFF-COLOR)]
             [(and (zipper-at-end? (imsg-state-mttm-tapes imsg-st))
                   (zipper-at-end? (imsg-state-mttm-head-positions imsg-st))
                   (eq? (imsg-state-mttm-machine-decision imsg-st) 'accept)
                   (eq? (mttm-type (imsg-state-mttm-M imsg-st)) 'mttm-language-recognizer))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (zipper-at-end? (imsg-state-mttm-tapes imsg-st))
                   (zipper-at-end? (imsg-state-mttm-head-positions imsg-st))
                   (eq? (imsg-state-mttm-machine-decision imsg-st) 'reject)
                   (eq? (mttm-type (imsg-state-mttm-M imsg-st)) 'mttm-language-recognizer))
              (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [(and (zipper-at-end? (imsg-state-mttm-tapes imsg-st))
                   (zipper-at-end? (imsg-state-mttm-head-positions imsg-st))
                   (eq? (imsg-state-mttm-machine-decision imsg-st) 'reject)
                   (eq? (mttm-type (imsg-state-mttm-M imsg-st)) 'mttm))
              (text "The machine reaches a final state and halts." FONT-SIZE ACCEPT-COLOR)]
             [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)])))))