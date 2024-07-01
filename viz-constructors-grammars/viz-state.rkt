#lang racket
(provide (struct-out viz-state)
         (struct-out informative-messages)
         (struct-out instructions-graphic))
(struct viz-state (imgs curr-image image-posn
                        ;; physical-screen-width physical-screen-height
                        ;; scale to screen size and finally stop having windows being too big or small?
                        ;; seems mostly harmless since we could default back to original values if shell commands fail for any reason
                        scale-factor scale-factor-cap scale-factor-floor
                        prev-mouse-posn curr-mouse-posn mouse-pressed
                        
                        ;rules yield input-word word-img-offset word-img-offset-cap
                        ;scroll-accum broken-invariants
                        
                        click-buffer
                        
                        informative-messages instructions-graphic
                        ))
(struct informative-messages (process-key process-tick draw-component component-state bounding-limits))

(struct instructions-graphic (clickable-bounding-limits img bounding-limits process-tick))
