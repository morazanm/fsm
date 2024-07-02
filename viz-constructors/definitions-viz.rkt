#lang racket

;; definitions

(provide viz-state-pimgs viz-state-upimgs viz-state viz-state-up-low viz-state-p-low)

;; upimgs are unprocessed graphs
;; pimgs are processed graph images
;; up-low are unprocessed etc structs
;; p-low are processed etc structs
(struct viz-state (upimgs pimgs up-low p-low))

