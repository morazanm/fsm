#lang racket
(provide (all-defined-out))



(struct imsg-state-ndfa (M                       ;;ndfa
                         upci                    ;;(listof symbol)/a-word
                         pci                     ;;(listof symbol)
                         shown-accepting-trace   ;;(zipperof trace)
                         farthest-consumed-input ;;(listof symbol)
                         invs-zipper             ;;(list->zipper inv-configs)
                         inv-amount              ;;(sub1 (length inv-configs))
                         computation-lengths     ;;natnum
                         computations            ;;(listof computation)
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum)
  #:transparent)

(struct imsg-state-pda (M                       ;;PDA
                        upci                    ;;(listof symbol)/word
                        pci                     ;;(listof symbol)
                        shown-accepting-trace   ;;(zipperof trace)
                        stack                   ;;(zipperof configuration)
                        farthest-consumed-input ;;(listof symbol)
                        invs-zipper             ;;(zipperof inv-config)
                        inv-amount              ;;natnum
                        computation-lengths     ;;natnum
                        computations            ;;(listof computation)
                        max-cmps                ;;natnum
                        word-img-offset
                        word-img-offset-cap
                        scroll-accum)
  #:transparent)

(struct imsg-state-tm (M                     ;;TM
                       tape                  ;;(zipperof symbol)
                       head-position         ;;(zipperof natnum)
                       rules-used            ;;(zipperof tm-rule)
                       shown-accepting-trace ;;(zipperof trace)
                       invs-zipper           ;;(zipperof inv-configs)
                       inv-amount            ;;natnum
                       computation-lengths   ;;(zipperof natnum)
                       computations          ;;(listof computation)
                       max-cmps              ;;natnum
                       machine-decision      ;;symbol
                       word-img-offset
                       word-img-offset-cap
                       scroll-accum)
  #:transparent)
