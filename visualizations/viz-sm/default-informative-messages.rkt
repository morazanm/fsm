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
         trace trace-config trace-rules
         ndfa-config ndfa-config-state ndfa-config-word ndfa-config-index
         pda-config pda-config-state pda-config-word pda-config-stack pda-config-index
         tm-config tm-config-state tm-config-head-position tm-config-tape tm-config-index
         tm tm-states tm-sigma tm-rules tm-start tm-finals tm-accepting-final tm-type)


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
(struct tm (states sigma rules start finals accepting-final type) #:transparent)


(define FONT-SIZE 20)

(define DARKGOLDENROD2 (make-color 238 173 14))

(define ACCEPT-COLOR (make-color 34 139 34)) ;;forestgreen

(define REJECT-COLOR 'red)

(define REJECT-COMPUTATION-COLOR 'violetred)

(define BLANK-COLOR 'white)

(define FONT-COLOR 'black)

(define COMPUTATION-LENGTH-COLOR 'brown)

(define accessor-func (compose tm-config-index (compose trace-config zipper-current)))
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
  (let* [(tape (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)) '(_)
                   (zipper-current (imsg-state-tm-tape imsg-st))))
         (start-index 0)
         (head-pos (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)) -1
                       (zipper-current (imsg-state-tm-head-position imsg-st))))
         (TAPE-SIZE 24)]
    (define (make-tape-img loi start-index)
      (if (empty? (rest loi))
          (above (first loi)
                 (square 5 'solid BLANK-COLOR)
                 (text (number->string start-index) 10 (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                           BLANK-COLOR
                                                           FONT-COLOR)))
          (beside (above (first loi)
                         (square 5 'solid BLANK-COLOR)
                         (text (number->string start-index) 10 (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                   BLANK-COLOR
                                                                   FONT-COLOR)))
                  (make-tape-img (rest loi) (add1 start-index)))))
    (let [(letter-imgs (build-list TAPE-SIZE
                                   (λ (i) (if (< (+ start-index i) (length tape))
                                              (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                                                             24
                                                             (cond [(zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                    BLANK-COLOR]
                                                                   [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   [else FONT-COLOR]))
                                                       (overlay (square 50 'solid BLANK-COLOR)
                                                                (square (add1 50) 'solid
                                                                        (if (zipper-empty?
                                                                             (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                            BLANK-COLOR
                                                                            FONT-COLOR))))
                                              (overlay (text (symbol->string BLANK)
                                                             24
                                                             (cond [(= i (- head-pos start-index)) REJECT-COLOR]
                                                                   [(zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                    BLANK-COLOR]
                                                                   [else FONT-COLOR]))
                                                       (square 50 'solid BLANK-COLOR)
                                                       (square (add1 50) 'solid
                                                               (if (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st))
                                                                 BLANK-COLOR
                                                                 FONT-COLOR)))))))]
      (make-tape-img letter-imgs start-index))))


;;X -> X
;;Purpose: Returns X
(define (id x) x)

;;image-state -> image
;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-ndfa-pci imsg-st) (imsg-state-ndfa-upci imsg-st))]
         [pci-length (length (imsg-state-ndfa-pci imsg-st))]
         [sub1-pci-length (sub1 pci-length)]
         
         
         [machine-decision (if (not (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)]) 

   (above/align
      'left
      (cond [(and (empty? (imsg-state-ndfa-pci imsg-st))
                  (empty? (imsg-state-ndfa-upci imsg-st)))
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
            [(and (not (empty? (imsg-state-ndfa-upci imsg-st)))
                  (equal? (imsg-state-ndfa-upci imsg-st) (imsg-state-ndfa-farthest-consumed-input imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                      (text "Word: " FONT-SIZE FONT-COLOR)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-ndfa-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-ndfa-pci imsg-st))
                                         '()
                                         (list (list sub1-pci-length 'gray)
                                               (list sub1-pci-length REJECT-COLOR)))))
              (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                      (if (empty? (imsg-state-ndfa-pci imsg-st))
                          (text "" FONT-SIZE FONT-COLOR)
                          (make-tape-img (take (imsg-state-ndfa-pci imsg-st) sub1-pci-length)
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
                                                      (if (empty? (imsg-state-ndfa-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-ndfa-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-ndfa-pci imsg-st)
                                                      (if (> (length (imsg-state-ndfa-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-ndfa-word-img-offset imsg-st)
                                                          0) 
                                                      (if (zipper-empty? (imsg-state-ndfa-shown-accepting-trace imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-ndfa-pci imsg-st)) ACCEPT-COLOR)
                                                                '())))))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (hash-ref (imsg-state-ndfa-computation-lengths imsg-st)
                                              (imsg-state-ndfa-upci imsg-st)
                                              0)))
             FONT-SIZE
             COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (empty? (imsg-state-ndfa-upci imsg-st)))
                  (equal? (imsg-state-ndfa-upci imsg-st) (imsg-state-ndfa-farthest-consumed-input imsg-st)))
              (text "All computations do not consume the entire word and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [(and (empty? (imsg-state-ndfa-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
             [(and (empty? (imsg-state-ndfa-upci imsg-st))
                   (equal? machine-decision 'reject))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE REJECT-COLOR)]
             [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))


(define (pda-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pda-pci imsg-st) (imsg-state-pda-upci imsg-st))]
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-pda-stack imsg-st)) 
                            (imsg-state-pda-stack imsg-st)
                            (pda-config-stack (zipper-current (imsg-state-pda-stack imsg-st))))]
         [machine-decision (if (not (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st)))
                               'accept
                               'reject)]
         [computation-has-cut-off (let ([res (when (zipper-empty? (imsg-state-pda-shown-accepting-trace imsg-st))
                                               (for/or ([config (map
                                                                 treelist-last
                                                                 (imsg-state-pda-computations imsg-st)
                                                                 )])
                                                  (>= (pda-config-index config) (imsg-state-pda-max-cmps imsg-st))))])
                                    (if (void? res)
                                        #f
                                        res))]
         
         [FONT-SIZE 20])
    (above/align
      'left
      (cond [(and (empty? (imsg-state-pda-pci imsg-st))
                  (empty? (imsg-state-pda-upci imsg-st)))
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
            [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (equal? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  computation-has-cut-off)
             (let* ([pci-length (length (imsg-state-pda-pci imsg-st))]
                    [sub1-pci-length (sub1 pci-length)])
               (above/align 'left
                          (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                                  (text "Word: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pda-pci imsg-st))
                                                     '()
                                                     (list (list sub1-pci-length 'gray)
                                                           (list sub1-pci-length DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                  (make-tape-img (take (imsg-state-pda-pci imsg-st) sub1-pci-length)
                                                 (if (> sub1-pci-length
                                                         TAPE-SIZE)
                                                     (imsg-state-pda-word-img-offset imsg-st)
                                                     0)
                                                 '()))))]
            [(and (equal? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (eq? machine-decision 'reject))
             (let* ([pci-length (length (imsg-state-pda-pci imsg-st))]
                   [sub1-pci-length (sub1 pci-length)])
               (above/align
                'left
                (beside (text "aaaK" FONT-SIZE BLANK-COLOR)
                        (text "Word: " FONT-SIZE FONT-COLOR)
                        (make-tape-img entire-word
                                       (if (> (length entire-word) TAPE-SIZE)
                                           (imsg-state-pda-word-img-offset imsg-st)
                                           0)
                                       (if (empty? (imsg-state-pda-pci imsg-st))
                                           '()
                                           (list (list sub1-pci-length 'gray)
                                                 (list sub1-pci-length REJECT-COLOR)))))
                (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                        (if (empty? (imsg-state-pda-pci imsg-st))
                            (text "" FONT-SIZE FONT-COLOR)
                            (make-tape-img (take (imsg-state-pda-pci imsg-st) sub1-pci-length)
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
                                                      (if (empty? (imsg-state-pda-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pda-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE FONT-COLOR)
                                       (make-tape-img (imsg-state-pda-pci imsg-st)
                                                      (if (> (length (imsg-state-pda-pci imsg-st)) TAPE-SIZE)
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
                                              (imsg-state-pda-upci imsg-st)
                                              0)))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (empty? (imsg-state-pda-upci imsg-st)))
                  (equal? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  computation-has-cut-off)
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-pda-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(and (empty? (imsg-state-pda-upci imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE ACCEPT-COLOR)]
            [(and (equal? (imsg-state-pda-upci imsg-st) (imsg-state-pda-farthest-consumed-input imsg-st))
                  (or (zipper-empty? (imsg-state-pda-stack imsg-st))
                      (zipper-at-end? (imsg-state-pda-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE REJECT-COLOR)]
            [else (text "Word Status: accept " FONT-SIZE BLANK-COLOR)]))))

(define (tm-create-draw-informative-message imsg-st)
  (above/align
      'left
      (if (zipper-empty? (imsg-state-tm-rules-used imsg-st))
          (text "Tape: " 1 BLANK-COLOR)
          (beside (text "Last rule used: " FONT-SIZE FONT-COLOR)
                  (text (format "~a" (if (or (equal? (zipper-current (imsg-state-tm-rules-used imsg-st)) '(_ _))
                                             (zipper-empty? (imsg-state-tm-rules-used imsg-st)))
                                         ""
                                         (zipper-current (imsg-state-tm-rules-used imsg-st))))
                        FONT-SIZE
                        (if (equal? (imsg-state-tm-machine-decision imsg-st) 'accept)
                            ACCEPT-COLOR
                            REJECT-COMPUTATION-COLOR))))
              
      (text "Tape: " 1 BLANK-COLOR)
      (if (not (zipper-empty? (imsg-state-tm-tape imsg-st)))
          (draw-imsg imsg-st)
          (text "Tape is not shown when there are multiple rejecting computations." FONT-SIZE FONT-COLOR))
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string #;(hash-ref ((imsg-state-tm-computation-lengths imsg-st) imsg-st)
                                              (imsg-state-ndfa-upci imsg-st)
                                              0)
                                    (zipper-current (imsg-state-tm-computation-lengths imsg-st))))
            FONT-SIZE
            COMPUTATION-LENGTH-COLOR)
      (cond [(and (not (zipper-empty? (imsg-state-tm-shown-accepting-trace imsg-st)))
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

;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
