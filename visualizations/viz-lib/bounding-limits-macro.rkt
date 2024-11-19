#lang racket/base
(require (for-syntax syntax/parse
                     racket/base
                     racket/syntax)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         2htdp/image
         "bounding-limits.rkt")

(provide create-bounding-limits)

(define-syntax (create-bounding-limits stx)
  (define-syntax-class
    pair
    (pattern (key-name key-text:str)
      #:with name #'key-name
      #:with text #'key-text))
  
  (syntax-parse stx
    [(_  E-SCENE-WIDTH E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH INFORMATIVE-MSG-HEIGHT FONT-SIZE KEY-BUFFER-SIZE INS-TOOLS-BUFFER
        ((~var first-pair pair) (~seq (~var rest-pairs pair) ...)))
     (with-syntax ([new-id (format-id #'first-pair.name "~a-DIMS" (syntax-e #'first-pair.name))])
       #'(begin
           (define new-id (bounding-limits
                           (+ (/ (- E-SCENE-WIDTH E-SCENE-TOOLS-WIDTH) 2)
                              (if (>= (image-width first-pair.name) (image-width (text first-pair.text (- FONT-SIZE 2) 'black)))
                                  0
                                  (/ (- (image-width (text first-pair.text (- FONT-SIZE 2) 'black)) (image-width first-pair.name)) 2)))
                           (+ (/ (- E-SCENE-WIDTH E-SCENE-TOOLS-WIDTH) 2)
                              (if (>= (image-width first-pair.name) (image-width (text first-pair.text (- FONT-SIZE 2) 'black)))
                                  0
                                  (/ (- (image-width (text first-pair.text (- FONT-SIZE 2) 'black)) (image-width first-pair.name)) 2))
                              (image-width first-pair.name))
                           (+ E-SCENE-HEIGHT
                              (bounding-limits-height INFORMATIVE-MSG-HEIGHT)
                              INS-TOOLS-BUFFER)
                           (+ E-SCENE-HEIGHT
                              (bounding-limits-height INFORMATIVE-MSG-HEIGHT)
                              INS-TOOLS-BUFFER
                              (image-height first-pair.name))))
           (create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH INFORMATIVE-MSG-HEIGHT FONT-SIZE KEY-BUFFER-SIZE INS-TOOLS-BUFFER
                                   (rest-pairs ...) (first-pair))))]
    [(_ E-SCENE-WIDTH E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH INFORMATIVE-MSG-HEIGHT FONT-SIZE KEY-BUFFER-SIZE INS-TOOLS-BUFFER
        ((~var first-pair pair)  (~seq (~var rest-pairs pair) ...)) ((~var processed-pairs pair) ...))
     (with-syntax ([new-id (format-id #'first-pair.name "~a-DIMS" (syntax-e #'first-pair.name))])
       #'(begin
         (define new-id (let* ([min-x (foldr
                                       (lambda (key-name key-text accum)
                                         (+ (image-width (text key-text (- FONT-SIZE 2) 'black))
                                            KEY-BUFFER-SIZE
                                            accum))
                                       (+ (/ (- E-SCENE-WIDTH E-SCENE-TOOLS-WIDTH) 2)
                                          (if (>= (image-width first-pair.name) (image-width (text first-pair.text (- FONT-SIZE 2) 'black)))
                                              0
                                              (/ (- (image-width (text first-pair.text (- FONT-SIZE 2) 'black)) (image-width first-pair.name)) 2)))
                                       (list processed-pairs.name ...)
                                       (list processed-pairs.text ...)
                                       )]
                               [max-x (+ min-x (image-width first-pair.name))]
                               [min-y (+ E-SCENE-HEIGHT
                                         (bounding-limits-height INFORMATIVE-MSG-HEIGHT)
                                         INS-TOOLS-BUFFER)]
                               [max-y (+ min-y (image-height first-pair.name))])
                          (bounding-limits min-x
                                           max-x
                                           min-y
                                           max-y)))
         (create-bounding-limits E-SCENE-WIDTH E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH INFORMATIVE-MSG-HEIGHT FONT-SIZE KEY-BUFFER-SIZE INS-TOOLS-BUFFER
                                 (rest-pairs ...) (first-pair processed-pairs ...))))]
    [(_  E-SCENE-WIDTH E-SCENE-HEIGHT E-SCENE-TOOLS-WIDTH INFORMATIVE-MSG-HEIGHT FONT-SIZE KEY-BUFFER-SIZE INS-TOOL-BUFFER
         () ((~var processed-pairs pair) ...))
     #'(void)]))