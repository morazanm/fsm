#lang racket
(provide (all-defined-out))
(struct imsg-state (M  ;new-M
                    upci ;;a-word
                    pci ;;'()
                    acpt-trace ;;'N/A
                    stack ;;'N/A
                    farthest-consumed ;;'N/A
                    invs-zipper ;;(list->zipper inv-configs)
                    inv-amt ;;(sub1 (length inv-configs))
                    comps-len ;computation-lens
                    comps ;'N/A
                    max-cmps
                    word-img-offset
                    word-img-offset-cap
                    scroll-accum) #:transparent)