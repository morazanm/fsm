#lang racket
(require "../../2htdp/image.rkt"
         racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path S-KEY-PATH "KEYBOARD-S-KEY.png")
(define-runtime-path W-KEY-PATH "KEYBOARD-W-KEY.png")
(define-runtime-path R-KEY-PATH "KEYBOARD-R-KEY.png")
(define-runtime-path F-KEY-PATH "KEYBOARD-F-KEY.png")
(define-runtime-path E-KEY-PATH "KEYBOARD-E-KEY.png")
(define-runtime-path A-KEY-PATH "KEYBOARD-A-KEY.png")
(define-runtime-path D-KEY-PATH "KEYBOARD-D-KEY.png")
(define-runtime-path ARROW-RIGHT-KEY-PATH "KEYBOARD-RIGHT-KEY.png")
(define-runtime-path ARROW-LEFT-KEY-PATH "KEYBOARD-LEFT-KEY.png")
(define-runtime-path ARROW-UP-KEY-PATH "KEYBOARD-UP-KEY.png")
(define-runtime-path ARROW-DOWN-KEY-PATH "KEYBOARD-DOWN-KEY.png")

(define S-KEY (bitmap/file S-KEY-PATH))
(define W-KEY (bitmap/file W-KEY-PATH))
(define R-KEY (bitmap/file R-KEY-PATH))
(define F-KEY (bitmap/file F-KEY-PATH))
(define E-KEY (bitmap/file E-KEY-PATH))
(define A-KEY (bitmap/file A-KEY-PATH))
(define D-KEY (bitmap/file D-KEY-PATH))
(define ARROW-RIGHT-KEY (bitmap/file ARROW-RIGHT-KEY-PATH))
(define ARROW-LEFT-KEY (bitmap/file ARROW-LEFT-KEY-PATH))
(define ARROW-UP-KEY (bitmap/file ARROW-UP-KEY-PATH))
(define ARROW-DOWN-KEY (bitmap/file ARROW-DOWN-KEY-PATH))

(define CURSOR
  (scale 0.9 (let ([cursor-rect (let ([inner-white (rectangle 5 17.5 'solid 'white)]
                           [outer-black (rectangle 9 20 'solid 'black)]
                           [white-triangle-infill (rectangle 9 5 'solid 'white)])
                       (above white-triangle-infill (overlay/xy inner-white -2 0 outer-black)))]
        [cursor-tri
         (let ([inner-white (overlay/align/offset
                             "right"
                             "middle"
                             (rotate 250
                                     (overlay/align/offset "middle"
                                                           "bottom"
                                                           (triangle/aas 30 30 44 'solid 'white)
                                                           0
                                                           3
                                                           (triangle/aas 30 30 48 'solid 'black)))
                             -2
                             -1
                             (triangle/aas 38.94 70.54 74 'solid 'white))]
               [outer-black (overlay/align/offset "right"
                                                  "middle"
                                                  (rotate 250 (triangle/aas 30 30 60 'solid 'white))
                                                  -1
                                                  -1
                                                  (triangle/sss 60 90 90 'solid 'black))])
           (scale 0.5 (rotate 310 (overlay/xy inner-white -9 -3 outer-black))))])
    (overlay/xy (rotate 25 cursor-rect) -7 -26 cursor-tri))))