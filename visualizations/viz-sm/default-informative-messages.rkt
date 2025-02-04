#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-constants.rkt"
         "david-imsg-state.rkt"
         "../../fsm-core/private/constants.rkt")

(provide ndfa-create-draw-informative-message
         pda-create-draw-informative-message
         tm-create-draw-informative-message)

(define FONT-SIZE 18)

(define DARKGOLDENROD2 (make-color 238 173 14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                (overlay (text (symbol->string (list-ref tape (+ start-index i)))
                               20
                               (cond [(empty? color-pair) 'black]
                                     [(and (not (empty? (first color-pair)))
                                           (< (+ start-index i) (first (first color-pair))))
                                      (second (first color-pair))]
                                     [(and (not (empty? (second color-pair)))
                                           (= (+ start-index i) (first (second color-pair))))
                                      (second (second color-pair))]
                                     [else 'black]))
                         (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))
                (overlay (square 21 'solid 'white) (square (add1 21) 'solid 'white)))))])
    (make-tape-img letter-imgs start-index)))

;; X (listof X) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst)
  (ormap (λ (L) (equal? x L)) lst))

(struct computation (LoC LoR visited) #:transparent)


;;Purpose: Determines which informative message is displayed to the user
(define (ndfa-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         
         ;;boolean
         ;;Purpose: Determines if the the machine should accept or reject the given word
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20]) 

   (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE 'red)))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'black)
                          (text (format "~a" EMP) FONT-SIZE 'white))))]
            [(and (not (empty? (imsg-state-pci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-pci imsg-st) ) 'gray)
                                               (list (length (imsg-state-pci imsg-st) ) 'red)))))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (empty? (imsg-state-pci imsg-st))
                          (text "" FONT-SIZE 'black)
                          (make-tape-img (imsg-state-pci imsg-st) 
                                         (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE 'white)
                                       (text "Word: " FONT-SIZE 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (text (format "The current number of possible computations is ~a (without repeated configurations). "
                     (number->string (list-ref (imsg-state-comps-len imsg-st)
                                               (length (imsg-state-pci imsg-st)))))
             FONT-SIZE
             'brown)
      (cond [(and (empty? (imsg-state-upci imsg-st))
                   (equal? machine-decision 'accept))
              (text "There is a computation that accepts." FONT-SIZE 'forestgreen)]
             [(and (equal? machine-decision 'reject)
                   (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st)))
              (text "All computations end in a non-final state and the machine rejects." FONT-SIZE 'red)]
             [else (text "Word Status: accept " FONT-SIZE 'white)]))))



(define (pda-create-draw-informative-message imsg-st)
  (let* (;;(listof symbols)
         ;;Purpose: The entire given word
         [entire-word (append (imsg-state-pci imsg-st) (imsg-state-upci imsg-st))]
         ;;(listof symbols)
         ;;Purpose: Holds what needs to displayed for the stack based off the upci
         [current-stack (if (zipper-empty? (imsg-state-stack imsg-st)) 
                            (imsg-state-stack imsg-st)
                            (third (zipper-current (imsg-state-stack imsg-st))))]
         [machine-decision (if (not (zipper-empty? (imsg-state-acpt-trace imsg-st)))
                               'accept
                               'reject)]
         [FONT-SIZE 20])
    (above/align
      'left
      (cond [(and (empty? (imsg-state-pci imsg-st))
                  (empty? (imsg-state-upci imsg-st)))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'gray)
                          (text (format "~a" EMP) FONT-SIZE 'red)))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (equal? machine-decision 'accept)
                          (text (format "~a" EMP) FONT-SIZE 'black)
                          (text (format "~a" EMP) FONT-SIZE 'white))))]
            [(and (not (empty? (imsg-state-upci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-comps imsg-st)))
             (above/align 'left
                          (beside (text "aaaK" FONT-SIZE 'white)
                                  (text "Word: " FONT-SIZE 'black)
                                  (make-tape-img entire-word
                                                 (if (> (length entire-word) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 (if (empty? (imsg-state-pci imsg-st))
                                                     '()
                                                     (list (list (length (imsg-state-pci imsg-st)) 'gray)
                                                           (list (length (imsg-state-pci imsg-st)) DARKGOLDENROD2)))))
                          (beside (text "Consumed: " FONT-SIZE 'black)
                                  (make-tape-img (imsg-state-pci imsg-st)
                                                 (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                     (imsg-state-word-img-offset imsg-st)
                                                     0)
                                                 '())))]
            [(and #;(not (empty? (imsg-state-pci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (eq? machine-decision 'reject))
             (above/align
              'left
              (beside (text "aaaK" FONT-SIZE 'white)
                      (text "Word: " FONT-SIZE 'black)
                      (make-tape-img entire-word
                                     (if (> (length entire-word) TAPE-SIZE)
                                         (imsg-state-word-img-offset imsg-st)
                                         0)
                                     (if (empty? (imsg-state-pci imsg-st))
                                         '()
                                         (list (list (length (imsg-state-pci imsg-st)) 'gray)
                                               (list (length (imsg-state-pci imsg-st)) 'red)))))
              (beside (text "Consumed: " FONT-SIZE 'black)
                      (if (empty? (imsg-state-pci imsg-st))
                          (text "" FONT-SIZE 'black)
                          (make-tape-img (imsg-state-pci imsg-st)
                                         (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))))]
            [else (above/align 'left
                               (beside (text "aaaK" FONT-SIZE 'white)
                                       (text "Word: " FONT-SIZE 'black)
                                       (make-tape-img entire-word
                                                      (if (> (length entire-word) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      (if (empty? (imsg-state-pci imsg-st))
                                                          '()
                                                          (list (list (length (imsg-state-pci imsg-st)) 'gray) '()))))
                               (beside (text "Consumed: " FONT-SIZE 'black)
                                       (make-tape-img (imsg-state-pci imsg-st)
                                                      (if (> (length (imsg-state-pci imsg-st)) TAPE-SIZE)
                                                          (imsg-state-word-img-offset imsg-st)
                                                          0)
                                                      '())))])
      (cond [(zipper-empty? (imsg-state-stack imsg-st)) (text "aaaC" FONT-SIZE 'white)]
            [(empty? current-stack) (beside (text "aaak" FONT-SIZE 'white)
                                            (text "Stack: " FONT-SIZE 'black))]
            [else (beside (text "aaak" FONT-SIZE 'white)
                          (text "Stack: " FONT-SIZE 'black)
                          (make-tape-img current-stack
                                         (if (> (length current-stack) TAPE-SIZE)
                                             (imsg-state-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string (if (= (length (imsg-state-pci imsg-st)) (imsg-state-max-cmps imsg-st))
                                        (list-ref (imsg-state-comps-len imsg-st)
                                                  (sub1 (length (imsg-state-pci imsg-st))))
                                        (list-ref (imsg-state-comps-len imsg-st)
                                                  (length (imsg-state-pci imsg-st))))))
            FONT-SIZE
            'brown)
      (cond [(and (not (empty? (imsg-state-upci imsg-st)))
                  (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (ormap (λ (comp) (>= (length comp) (imsg-state-max-cmps imsg-st)))
                         (imsg-state-comps imsg-st)))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-max-cmps imsg-st)) FONT-SIZE DARKGOLDENROD2)]
            [(and (empty? (imsg-state-upci imsg-st))
                  (or (zipper-empty? (imsg-state-stack imsg-st))
                      (zipper-at-end? (imsg-state-stack imsg-st)))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." FONT-SIZE 'forestgreen)]
            [(and (eq? (imsg-state-upci imsg-st) (imsg-state-farthest-consumed imsg-st))
                  (or (zipper-empty? (imsg-state-stack imsg-st))
                      (zipper-at-end? (imsg-state-stack imsg-st)))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." FONT-SIZE 'red)]
            [else (text "Word Status: accept " FONT-SIZE 'white)]))))

(define (tm-create-draw-informative-message imsg-st)
  (let ([machine-decision (if (not (zipper-empty? (imsg-state-tm-acpt-trace imsg-st)))
                               'accept
                               'reject)])
    (overlay/align
     'left 'middle
     (above/align
      'left
      
      (cond [(ormap (λ (comp) (>= (length comp) (imsg-state-tm-max-cmps imsg-st)))
                         (imsg-state-tm-comps imsg-st))
             (beside (text "aaaa" 20 'white)
                                  (text "Tape: " 20 'black)
                                  (make-tape-img (imsg-state-tm-tape imsg-st)
                                                 (if (> (length (imsg-state-tm-tape imsg-st)) TAPE-SIZE)
                                                     (imsg-state-tm-word-img-offset imsg-st)
                                                     0)
                                                 '()))]
            [else (beside (text "aaaa" 20 'white)
                          (text "Tape: " 20 'black)
                          (make-tape-img (imsg-state-tm-tape imsg-st)
                                         (if (> (length (imsg-state-tm-tape imsg-st)) TAPE-SIZE)
                                             (imsg-state-tm-word-img-offset imsg-st)
                                             0)
                                         '()))])
      (text (format "The current number of possible computations is: ~a (without repeated configurations)."
                    (number->string 0
                                    #;(if (= (length (imsg-state-tm-pci imsg-st)) (imsg-state-tm-max-cmps imsg-st))
                                        (list-ref (imsg-state-tm-comps-len imsg-st)
                                                  (sub1 (length (imsg-state-tm-pci imsg-st))))
                                        (list-ref (imsg-state-tm-comps-len imsg-st)
                                                  (length (imsg-state-tm-pci imsg-st))))))
            20
            'brown)
      (cond [(ormap (λ (comp) (>= (length comp) (imsg-state-tm-max-cmps imsg-st)))
                         (imsg-state-tm-comps imsg-st))
             (text (format "There are computations that exceed the cut-off limit (~a)."
                           (imsg-state-tm-max-cmps imsg-st)) 20 DARKGOLDENROD2)]
            [(and (empty? (imsg-state-tm-tape imsg-st))
                  (equal? machine-decision 'accept))
             (text "There is a computation that accepts." 20 'forestgreen)]
            [(and (empty? (imsg-state-tm-tape imsg-st))
                  (equal? machine-decision 'reject))
             (text "All computations end in a non-final configuration and the machine rejects." 20 'red)]
            [else (text "Word Status: accept " 20 'white)]))
     (rectangle 1250 50 'solid 'white))))

;"notes to self:"
;"scroll thru word instead of jumping to end"
;"highlight which rule is being used when there are multiple rules on an edge"
