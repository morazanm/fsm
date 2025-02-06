#lang racket
(provide (all-defined-out))
#;(struct imsg-state (M  ;new-M
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

#;(struct imsg-state-tm (M tape
                         acpt-trace 
                         invs-zipper ;;(list->zipper inv-configs)
                         inv-amt ;;(sub1 (length inv-configs))
                         comps-len ;computation-lens
                         comps 
                         max-cmps
                         head-pos
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum) #:transparent)


(struct imsg-state (M upci pci tape head-pos acpt-trace stack farthest-consumed invs-zipper inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)
;(struct imsg-state-tm  (M tape     acpt-trace                         invs-zipper inv-amt comps-len comps max-cmps head-pos word-img-offset word-img-offset-cap scroll-accum) #:transparent)
;(struct new-imsg-state (M upci pci tape head-pos acpt-trace stack farthest-consumed invs-zipper inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)