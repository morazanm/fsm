#lang fsm

;; definitions

(provide viz-state-pimgs viz-state-upimgs viz-state)

;; upimgs are unprocessed graphs
;; pimgs are processed graph images
(struct viz-state (upimgs pimgs))

