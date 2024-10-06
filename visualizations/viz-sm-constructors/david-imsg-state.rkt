#lang racket
(provide (all-defined-out))
(struct imsg-state (M
                    upci pci upstck pstck invs-zipper
                      inv-amt comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)