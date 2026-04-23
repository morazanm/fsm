#lang racket/base

(require "../2htdp/image.rkt"
         "../viz-lib/colors.rkt"
         racket/treelist)

(provide (all-defined-out))


(define MIN-AMOUNT-TO-COMPARE 2)

(define BLANK-SPACE 'blank)

(define BLACK 'black)

(define WHITE 'white)

(define GRAY 'gray)

(define RED 'red)

(define DESTINATION-PAIR-COLOR 'blue)

(define FINAL-STATE-COLOR 'orange)

(define START-STATE-COLOR 'darkgreen)

(define SOLID 'solid)

(define BOLD 'bold)

(define X-MARK-SIZE 38)

(define OUTLINE-SQUARE-SIZE 45)

(define BASE-SQUARE-SIZE 40)

(define INIT-COL-IDX 0)

(define INIT-ROW-IDX 0)

(define NEW-MARK 'new-mark)

(define MARK 'mark)

(define PHASE--1 -1)

(define PHASE-0 0)

(define PHASE-1 1)

(define PHASE-2 2)

(define PHASE-3 3)

(define PHASE-4 4)

(define PHASE-5 5)

(define PHASE-6 6) 

(struct color-palette (select-color destination-pair-color start-state-color final-state-color outline-color
                       select-graph-color destination-graph-color fs-graph-color) #:transparent)

(define default-color-scheme (color-palette RED DESTINATION-PAIR-COLOR START-STATE-COLOR FINAL-STATE-COLOR GRAY
                                            RED DESTINATION-PAIR-COLOR FINAL-STATE-COLOR))
;;no red
(define prot-color-scheme (color-palette (hash-ref X11-AS-RACKET-HASH 'lightslateblue)
                                          DESTINATION-PAIR-COLOR
                                          (hash-ref X11-AS-RACKET-HASH 'red4)
                                          (hash-ref X11-AS-RACKET-HASH 'darkorange2)
                                          (hash-ref X11-AS-RACKET-HASH 'gray34)
                                          'lightslateblue
                                          DESTINATION-PAIR-COLOR
                                          'darkorange2))
;;no green
(define deut-color-scheme (color-palette (hash-ref X11-AS-RACKET-HASH 'deepskyblue1)
                                          DESTINATION-PAIR-COLOR
                                          'cornflowerblue
                                          FINAL-STATE-COLOR
                                          (hash-ref X11-AS-RACKET-HASH 'antiquewhite4)
                                          'deepskyblue1
                                          DESTINATION-PAIR-COLOR
                                          FINAL-STATE-COLOR))
;;no blue
(define trit-color-scheme (color-palette RED
                                          (hash-ref X11-AS-RACKET-HASH 'mediumspringgreen)
                                          'palegreen4
                                          FINAL-STATE-COLOR
                                          (hash-ref X11-AS-RACKET-HASH 'purple3)
                                          RED
                                          'mediumspringgreen
                                          FINAL-STATE-COLOR))

#|
(define default-color-scheme
  (above
   (square BASE-SQUARE-SIZE SOLID RED)
   (square BASE-SQUARE-SIZE SOLID DESTINATION-PAIR-COLOR)
   (square BASE-SQUARE-SIZE SOLID START-STATE-COLOR)
   (square BASE-SQUARE-SIZE SOLID FINAL-STATE-COLOR)
   (square BASE-SQUARE-SIZE SOLID GRAY)
   (square BASE-SQUARE-SIZE SOLID BLACK)
   (square BASE-SQUARE-SIZE SOLID WHITE)))

(define prot-color-scheme
  (above
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'palevioletred4))
   (square BASE-SQUARE-SIZE SOLID DESTINATION-PAIR-COLOR)
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'red4))
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'salmon4))
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'plum4))
   (square BASE-SQUARE-SIZE SOLID BLACK)
   (square BASE-SQUARE-SIZE SOLID WHITE)))

(define deut-color-scheme
  (above
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'plum4))
   (square BASE-SQUARE-SIZE SOLID DESTINATION-PAIR-COLOR)
   (square BASE-SQUARE-SIZE SOLID START-STATE-COLOR)
   (square BASE-SQUARE-SIZE SOLID FINAL-STATE-COLOR)
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'palevioletred4))
   (square BASE-SQUARE-SIZE SOLID BLACK)
   (square BASE-SQUARE-SIZE SOLID WHITE)))

(define trit-color-scheme
  (above
   (square BASE-SQUARE-SIZE SOLID RED)
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'mediumspringgreen))
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'palegreen4))
   (square BASE-SQUARE-SIZE SOLID FINAL-STATE-COLOR)
   (square BASE-SQUARE-SIZE SOLID (hash-ref X11-AS-RACKET-HASH 'purple3))
   (square BASE-SQUARE-SIZE SOLID BLACK)
   (square BASE-SQUARE-SIZE SOLID WHITE)))

'default-color-scheme
default-color-scheme
'prot-color-scheme
prot-color-scheme
'deut-color-scheme
deut-color-scheme
'trit-color-scheme
trit-color-scheme|#