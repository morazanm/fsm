#lang racket
(provide (all-defined-out))



(struct imsg-state-ndfa (M                       ;;ndfa
                         ci                      ;;(zipperof ci)
                         shown-accepting-trace   ;;(zipperof trace)
                         farthest-consumed-input ;;(listof symbol)
                         invs-zipper             ;;(list->zipper inv-configs)
                         computation-lengths     ;;natnum
                         computations            ;;(listof computation)
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum)
  #:transparent)

(struct imsg-state-pda (M                       ;;PDA
                        ci                      ;;(zipperof ci)
                        shown-accepting-trace   ;;(zipperof trace)
                        stack                   ;;(zipperof configuration)
                        farthest-consumed-input ;;(listof symbol)
                        invs-zipper             ;;(zipperof inv-config)
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
                       computation-lengths   ;;(zipperof natnum)
                       computations          ;;(listof computation)
                       max-cmps              ;;natnum
                       machine-decision      ;;symbol
                       word-img-offset
                       word-img-offset-cap
                       scroll-accum)
  #:transparent)
