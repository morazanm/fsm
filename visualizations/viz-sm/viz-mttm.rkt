#lang racket

(require "../../fsm-gviz/private/lib.rkt"
         "../2htdp/image.rkt"
         "../viz-lib/viz.rkt"
         "../viz-lib/zipper.rkt"
         racket/treelist
         "../viz-lib/bounding-limits.rkt"
         "../viz-lib/viz-state.rkt"
         "../viz-lib/viz-macros.rkt"
         "../viz-lib/vector-zipper.rkt"
         (except-in "../viz-lib/viz-constants.rkt"
                    INS-TOOLS-BUFFER)
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../../fsm-core/private/constants.rkt"
         "../../fsm-core/private/tm.rkt" 
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         "default-informative-messages.rkt")


(struct mttm (states alphabet start finals transition number-tape accepting-final type) #:transparent)

(struct tape-config (head-position tape))

(struct mttm-config (state lotc))

(struct mttm-rule (source-rule destination-rule))

#|
state -> the state at which the actions are applied | symbol
loa -> all the actions to applied to each tape      | (listof TM-actions)
|#
(struct rule (state loa))