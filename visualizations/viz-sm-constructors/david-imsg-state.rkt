#lang racket
(provide (all-defined-out))
(struct imsg-state (M
                    upci pci acpt-trace stck farthest-consumed invs-zipper
                    inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)