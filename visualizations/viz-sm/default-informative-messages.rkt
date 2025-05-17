
#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/tl-zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         racket/treelist
         "david-imsg-state.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/constants.rkt")

(provide ndfa-create-draw-informative-message
         pda-create-draw-informative-message
         tm-create-draw-informative-message
         mttm-create-draw-informative-message
         trace trace-config trace-rules
         ndfa-config ndfa-config-state ndfa-config-word ndfa-config-index
         pda-config pda-config-state pda-config-word pda-config-stack pda-config-index
         tm-config tm-config-state tm-config-head-position tm-config-tape tm-config-index
         tm tm-states tm-sigma tm-rules tm-start tm-finals tm-accepting-final tm-type
         mttm-config mttm-config-state mttm-config-lotc mttm-config-index
         mttm mttm-states mttm-sigma mttm-rules mttm-start mttm-finals mttm-accepting-final mttm-tape-amount mttm-type
         tape-config tape-config-head-position tape-config-tape
         ci ci-upci ci-pci)


#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules) #:transparent)
(struct pda-config (state word stack index) #:transparent)
(struct ndfa-config (state word index) #:transparent)
(struct tm-config (state head-position tape index) #:transparent)
;;head-position -> the head position of the tape | natnum
;;tape -> the listof letters being read/written  | (listof symbol)
(struct tape-config (head-position tape) #:transparent)
;;state -> the state that the ocnfiguration is in                               | symbol
;;lotc  -> all of the tape configurations associated with current configuration | (listof tape-config)
;;index -> the number associated with the configuration                         | natnum
(struct mttm-config (state lotc index) #:transparent)
;;state -> all of the states that the tm has    | (listof symbol
;;sigma -> all of letters the tm can read/write | (listof symbol)
;;rules -> the transition rules for the tm      | (treelistof tm-rule)
;;start -> the starting state                   | symbol
;;finals -> the final states                    | (listof symbol)
;;accepting-final -> the accepting final state  | symbol
;;type -> the type of tm                        | symbol
(struct tm (states sigma rules start finals accepting-final type) #:transparent)
;;state -> all of the states that the mttm has    | (listof symbol
;;sigma -> all of letters the mttm can read/write | (listof symbol)
;;start -> the starting state                     | symbol
;;finals -> the final states                      | (listof symbol)
;;rules -> the transition rules for the mttm      | (treelistof mttm-rule)
;;tape-amount -> the number of tapes the mttm has | natnum
;;accepting-final -> the accepting final state    | symbol
;;type -> the type of mttm                        | symbol
(struct mttm (states sigma start finals rules tape-amount accepting-final type) #:transparent)
(struct ci (upci pci) #:transparent)

(define FONT-SIZE 20)

(define DARKGOLDENROD2 (make-color 238 173 14))

(define ACCEPT-COLOR (make-color 34 139 34)) ;;forestgreen

(define REJECT-COLOR 'red)

(define REJECT-COMPUTATION-COLOR 'violetred)

(define BLANK-COLOR 'white)

(define FONT-COLOR 'black)

(define COMPUTATION-LENGTH-COLOR 'brown)

(define DUMMY-TM-RULE '(@ @))

(define MAX-AUX-TAPE-AMOUNT 2)

(define accessor-func (compose tm-config-index (compose trace-config zipper-current)))
(define mttm-accessor-func (compose mttm-config-index (compose trace-config zipper-current)))
(define pda-accessor-func (compose pda-config-index (compose trace-config zipper-current)))
(define ndfa-accessor-func (compose third (compose trace-config zipper-current)))

(define get-index (compose fourth zipper-current))
(define get-index-ndfa (compose third zipper-current))

(define get-next-index (compose fourth (compose zipper-current zipper-next)))
(define get-next-index-pda (compose pda-config-index (compose zipper-current zipper-next)))
(define get-next-index-ndfa (compose third (compose zipper-current zipper-next)))

(define get-prev-index (compose fourth (compose zipper-current zipper-prev)))
(define get-prev-index-ndfa (compose third (compose zipper-current zipper-prev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
(define (tm-getstates m) (m '() 0 'get-states))
  
(define (tm-getfinals m) (m '() 0 'get-finals))

(define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules

(define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules

(define (tm-getstart m) (m '() 0 'get-start))
  
(define (tm-getaccept m) (m '() 0 'get-accept))

(define (tm-whatami? m) (m 'whatami 0 'whatami))

(define (make-tape-img tape start-index color-pair)
  (define (make-tape-img loi start-index)
    (if (empty? (rest loi))
        (first loi)
        (beside (first loi) (make-tape-img (rest loi) (add1 start-index)))))
  (let ([letter-imgs
         (build-list
          TAPE-SIZE
          (λ (i)
            (if (< (+ start-index i) (length tape))
                (let [(tape-element (list-ref tape (+ start-index i)))]
                  (overlay (text (if (symbol? tape-element)
                                   (symbol->string tape-element)
                                   (number->string tape-element))
                               20
                               (cond [(empty? color-pair) FONT-COLOR]
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
                       (zipper-current (imsg-state-tm-head-position imsg-st))))]
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
         
         
         [machine-decision (if (not (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)])
   (above/align
      'left
      (cond [(and (empty? pci)
                  (empty? upci))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
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
                                         (list (list sub1-pci-length 'gray)
                                               (list sub1-pci-length REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? pci)
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img (take pci sub1-pci-length)
                                         (if (> sub1-pci-length TAPE-SIZE)
                                             (imsg-state-ndfa-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? pci)
                                                          '()
                                                          (list (list (length pci) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img pci
                                                      (if (> (length pci) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0) 
                                                      (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st))
                                                          '()
                                                          (list (list (length pci) ACCEPT-COLOR)
                                                                '())))))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (hash-ref (imsg-state-ndfa-computation-lengths imsg-st)
                                              upci
                                              0)))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
      (cond [(and (empty? upci) (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (empty? upci) (equal? machine-decision 'reject)
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
                          (text (format "~a" EMP) FONT-SIZE 'gray)
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
                                                     (list (list sub1-pci-length 'gray)
                                                           (list sub1-pci-length DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img (take pci sub1-pci-length)
                                                 (if (> sub1-pci-length
                                                         TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 '()))))]
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
                                           (list (list sub1-pci-length 'gray)
                                                 (list sub1-pci-length REJECT-COLOR)))))
                (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                        (if (empty? pci)
                            (text "" FONT-SIZE FONT-COLOR)
                            (make-tape-img (take pci sub1-pci-length)
                                           (if (> pci-length TAPE-SIZE)
                                               (imsg-state-pda-word-img-offset imsg-st)
                                               0)
                                           '())))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                       (text "Word: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? pci)
                                                          '()
                                                          (list (list (length pci) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img pci
                                                      (if (> (length pci) TAPE-SIZE)
                                                          (imsg-state-pda-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (cond [(zipper-empty? (imsg-state-pda-stack imsg-st)) (text "aaaC" FONT-SIZE BLANK-COLOR)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                                            (text "Stack: " FONT-SIZE FONT-COLOR))]
            [else (beside (text "aaak" FONT-SIZE BLANK-COLOR)
                          (text "Stack: " FONT-SIZE FONT-COLOR)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-pda-word-img-offset imsg-st)
                                             0)
                                         '()))])
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
                           (imsg-state-pda-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
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
                  (zipper-at-end? (imsg-state-pda-stack imsg-st)) #;(zipper-empty? (imsg-state-pda-stack imsg-st))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))

(define (tm-create-draw-informative-message imsg-st)
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
                            REJECT-COMPUTATION-COLOR))))
              
      (text "Tape: " 1 BLANK-COLOR)
      (draw-imsg imsg-st)  
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (zipper-current (imsg-state-tm-computation-lengths imsg-st))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (zipper-at-end? (imsg-state-tm-shown-accepting-trace imsg-st))
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'reject)
                  (>= (accessor-func (imsg-state-tm-shown-accepting-trace imsg-st)) (imsg-state-tm-max-cmps imsg-st))
                  (not (equal? (tm-config-state (trace-config (zipper-current (imsg-state-tm-shown-accepting-trace imsg-st))))
                               (tm-accepting-final (imsg-state-tm-M imsg-st)))))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-tm-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
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
                  (eq? (imsg-state-tm-machine-decision imsg-st) 'reject)
                  (eq? (tm-type (imsg-state-tm-M imsg-st)) 'tm))
             (text "The machine reaches a final state and halts." FONT-SIZE ACCEPT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)])))

(define (mttm-create-draw-informative-message imsg-st)
  
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
           (text (format "T~s: " aux-tape-index) 20 'black)
           (draw-tape (list-ref tapes aux-tape-index) (list-ref head-positions aux-tape-index)))
          (above
           (beside
            (text (format "T~s: " aux-tape-index) 20 'black)
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
                           REJECT-COMPUTATION-COLOR)))         
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
                 (>= (mttm-accessor-func (imsg-state-mttm-shown-rejecting-trace imsg-st)) (imsg-state-mttm-max-cmps imsg-st))
                 (not (equal? (mttm-config-state (trace-config (zipper-current (imsg-state-mttm-shown-rejecting-trace imsg-st))))
                              (mttm-accepting-final (imsg-state-mttm-M imsg-st)))))
            (text (format "There are computations that exceed the cut-off limit (~a)."
                          (imsg-state-mttm-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
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
           [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))



;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
