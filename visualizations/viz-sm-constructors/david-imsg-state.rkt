#lang racket
(provide (all-defined-out))
(struct imsg-state (M
                    upci pci acpt-trace stck invs-zipper
                    inv-amt comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)