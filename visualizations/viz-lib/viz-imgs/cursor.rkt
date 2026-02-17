#lang racket/base
(require "../../2htdp/image.rkt")
(provide cursor)

(define cursor
  (let [(cursor-rect (let [(inner-white (rectangle 5 17.5 'solid 'white))
                           (outer-black (rectangle 9 20 'solid 'black))
                           (white-triangle-infill (rectangle 9 5 'solid 'white))]
                       (above white-triangle-infill (overlay/xy inner-white -2 0 outer-black))))
        (cursor-tri (let [(inner-white
                           (overlay/align/offset "right"
                                                 "middle"
                                                 (rotate 250
                                                         (overlay/align/offset "middle"
                                                                               "bottom"
                                                                               (triangle/aas 30
                                                                                             30
                                                                                             44
                                                                                             'solid
                                                                                             'white)
                                                                               0
                                                                               3
                                                                               (triangle/aas 30
                                                                                             30
                                                                                             48
                                                                                             'solid
                                                                                             'black)))
                                                 -2
                                                 -1
                                                 (triangle/aas 38.94
                                                               70.54
                                                               74
                                                               'solid
                                                               'white)))
                          (outer-black (overlay/align/offset "right"
                                                             "middle"
                                                             (rotate 250
                                                                     (triangle/aas 30
                                                                                   30
                                                                                   60
                                                                                   'solid
                                                                                   'white))
                                                             -1
                                                             -1
                                                             (triangle/sss 60
                                                                           90
                                                                           90
                                                                           'solid
                                                                           'black)))]
                      (scale 0.5 (rotate 310 (overlay/xy inner-white
                                                         -9
                                                         -3
                                                         outer-black)))))]
    (overlay/xy (rotate 25 cursor-rect) -7 -26 cursor-tri)))