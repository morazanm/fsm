#lang racket/base
(provide (all-defined-out))

(struct imsg-state-ndfa (M                       ;;NDFA
                         ci                      ;;(zipperof ci)
                         shown-accepting-trace   ;;(zipperof trace)
                         farthest-consumed-input ;;(listof symbol)
                         invs-zipper             ;;(zipperof inv-configs)
                         computation-lengths     ;;natnum
                         accepted?               ;;boolean
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum
                         color-pallete)
  #:transparent)

(struct imsg-state-pda (M                        ;;PDA
                        ci                       ;;(zipperof ci)
                        shown-accepting-trace    ;;(zipperof trace)
                        stack                    ;;(zipperof configuration)
                        farthest-consumed-input  ;;(listof symbol)
                        invs-zipper              ;;(zipperof inv-config)
                        computation-lengths      ;;natnum
                        computation-has-cut-off? ;;boolean
                        accepted?                ;;boolean
                        max-cmps                 ;;natnum
                        word-img-offset
                        word-img-offset-cap
                        scroll-accum
                        color-pallete)
  #:transparent)

(struct imsg-state-tm (M                     ;;TM
                       tape                  ;;(zipperof symbol)
                       head-position         ;;(zipperof natnum)
                       rules-used            ;;(zipperof tm-rule)
                       shown-accepting-trace ;;(zipperof trace)
                       invs-zipper           ;;(zipperof inv-configs)
                       computation-lengths   ;;(zipperof natnum)
                       max-cmps              ;;natnum
                       machine-decision      ;;symbol
                       word-img-offset
                       word-img-offset-cap
                       scroll-accum
                       color-pallete)
  #:transparent)


(struct imsg-state-mttm (M                      ;;MTTM
                         tapes                  ;;(zipperof (listof tapes))
                         head-positions         ;;(zipperof (listof natnums))
                         rules-used             ;;(zipperof mttm-rule)
                         shown-accepting-trace  ;;(zipperof trace)
                         shown-rejecting-trace  ;;(zipperof trace)
                         invs-zipper            ;;(zipperof inv-configs)
                         computation-lengths    ;;(zipperof natnum)
                         max-cmps               ;;natnum
                         machine-decision       ;;symbol
                         aux-tape-index         ;;natnum
                         word-img-offset
                         word-img-offset-cap
                         scroll-accum
                         color-pallete)
  #:transparent)