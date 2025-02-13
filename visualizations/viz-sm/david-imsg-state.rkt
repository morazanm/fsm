#lang racket
(provide (all-defined-out))
(struct imsg-state (M
                    upci
                    pci
                    tape
                    head-pos
                    acpt-trace
                    stack
                    farthest-consumed
                    invs-zipper
                    inv-amt
                    comps-len
                    comps
                    max-cmps
                    word-img-offset
                    word-img-offset-cap
                    scroll-accum) #:transparent)

;(struct imsg-state      (M upci pci tape head-pos acpt-trace stack farthest-consumed invs-zipper inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)
;(struct imsg-state-ndfa (M upci pci               acpt-trace       farthest-consumed invs-zipper inv-amt comps-len comps          word-img-offset word-img-offset-cap scroll-accum) #:transparent)
;(struct imsg-state-pda  (M upci pci               acpt-trace stack farthest-consumed invs-zipper inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)
;(struct imsg-state-tm   (M          tape head-pos acpt-trace                         invs-zipper inv-amt comps-len comps max-cmps word-img-offset word-img-offset-cap scroll-accum) #:transparent)
(struct imsg-state-ndfa (M  ;new-M
                         upci ;;a-word
                         pci ;;'()
                         shown-accepting-trace ;;'N/A
                         farthest-consumed-input ;;'N/A
                         invs-zipper ;;(list->zipper inv-configs)
                         inv-amount ;;(sub1 (length inv-configs))
                         computation-lengths ;computation-lens
                         computations ;'N/A
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum)
  #:transparent)
(struct imsg-state-pda (M  ;new-M
                        upci ;;a-word
                        pci ;;'()
                        shown-accepting-trace ;;'N/A
                        stack ;;'N/A
                        farthest-consumed-input ;;'N/A
                        invs-zipper ;;(list->zipper inv-configs)
                        inv-amount ;;(sub1 (length inv-configs))
                        computation-lengths ;computation-lens
                        computations ;'N/A
                        max-cmps
                        word-img-offset
                        word-img-offset-cap
                        scroll-accum)
  #:transparent)

(struct imsg-state-tm (M ;;TM
                       tape ;;(zipperof symbol)
                       head-position ;;natnum
                       rules ;;(zipperof tm-rule)
                       shown-accepting-trace ;;(zipperof trace)
                       invs-zipper ;;(zipperof inv-configs)
                       inv-amount ;;natnum
                       computation-lengths ;(zipperof natnum)
                       computations ;;(listof computation)
                       max-cmps ;;natnum
                       word-img-offset
                       word-img-offset-cap
                       scroll-accum)
  #:transparent)
