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
         "../../fsm-core/private/mtape-tm.rkt" 
         "david-imsg-state.rkt"
         (except-in "david-viz-constants.rkt"
                    FONT-SIZE)
         "default-informative-messages.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct mttm (states alphabet start finals transition number-tape accepting-final type) #:transparent)

(struct tape-config (head-position tape))

(struct mttm-config (state lotc))

(struct mttm-rule (source-rule destination-rule))

#|
state -> the state at which the actions are applied | symbol
loa -> all the actions to applied to each tape      | (listof TM-actions)
|#
(struct half-rule (state loa) #:transparent)
#|
source -> the first of a mttm rule     | half-rule
destination -> the rest of a mttm rule | half-rule
|#
(struct rule (source destination) #:transparent)

;;tape is the input the tape
;;computations is a (listof computation) that attempt to consume the ci
;;accepting computations is (listof computation) for all accepting computations
;;accept-traces is a (listof configuration)
;;reject-traces is a (listof configuration)
;;M is the given machine
;;inv is a the (listof (state (listof symbol -> boolean)))
;;max-cmps is the max amount of transitions the machine can make
;;head-pos is the beginning head position of the tape=
(struct building-viz-state (tape
                            computations
                            tracked-accept-trace
                            all-accept-traces
                            all-reject-traces
                            M
                            inv
                            max-cmps
                            head-pos
                            machine-decision)
  #:transparent)

(define DUMMY-RULE (list (list BLANK BLANK) (list BLANK BLANK)))

(define (remake-mttm M)
  (define (remake-rules a-lor)
    (for/treelist ([mttm-rule a-lor])
      (rule (half-rule (first (first mttm-rule)) (second (first mttm-rule)))
            (half-rule (first (second mttm-rule)) (second (second mttm-rule))))))
  (mttm (mttm-get-states M)
        (mttm-get-sigma M)
        (mttm-get-start M)
        (mttm-get-finals M)
        (remake-rules (mttm-get-rules M))
        (M 'get-numtapes)
        (if (eq? (mttm-what-am-i M) 'mttm-language-recognizer) (mttm-get-accept M) 'none)
        (mttm-what-am-i M)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;Pre-Condition: '(LM BLANK w) AND t0h = 1 AND tapes 1-3 are empty AND t1h-t3h = 0
;;L = {w | w ∈ a^nb^nc^n}
(define a^nb^nc^n (make-unchecked-mttm '(K H R E C O M T F)
                                       '(a b c)
                                       'K
                                       '(F)
                                       (list
                                        (list (list 'K (list BLANK BLANK BLANK BLANK));; <-- Starting 
                                              (list 'H (list RIGHT RIGHT RIGHT RIGHT))) 
                                        (list (list 'H (list 'a BLANK BLANK BLANK)) ;;<-- Phase 1, reads a's
                                              (list 'R (list 'a 'a BLANK BLANK)))
                                        (list (list 'R (list 'a 'a BLANK BLANK))
                                              (list 'H (list RIGHT RIGHT BLANK BLANK))) 
                                        (list (list 'H (list 'b BLANK BLANK BLANK)) ;;<-- phase 2, read b's
                                              (list 'E (list 'b BLANK 'b BLANK)))
                                        (list (list 'E (list 'b BLANK 'b BLANK))
                                              (list 'C (list RIGHT BLANK RIGHT BLANK)))
                                        (list (list 'C (list 'b BLANK BLANK BLANK))
                                              (list 'E (list 'b BLANK 'b BLANK))) 
                                        (list (list 'C (list 'c BLANK BLANK BLANK)) ;;<-- phase 3, read c's
                                              (list 'O (list 'c BLANK BLANK 'c)))
                                        (list (list 'O (list 'c BLANK BLANK 'c))
                                              (list 'M (list RIGHT BLANK BLANK RIGHT)))
                                        (list (list 'M (list 'c BLANK BLANK BLANK)) 
                                              (list 'O (list 'c BLANK BLANK 'c)))
                                        (list (list 'M (list BLANK BLANK BLANK BLANK)) ;;<-- phase 4, matching as, bs, cs
                                              (list 'T (list BLANK LEFT LEFT LEFT)))
                                        (list (list 'T (list BLANK 'a 'b 'c))
                                              (list 'T (list BLANK LEFT LEFT LEFT)))
                                        (list (list 'T (list BLANK BLANK BLANK BLANK)) ;;<-phase 5, accept (if possible)
                                              (list 'F (list BLANK BLANK BLANK BLANK)))
                                        )
                                       4
                                       'F))


;;Pre-Condition: '(LM BLANK w) AND t0h = 1 AND tape 1 is empty AND t1h = 0
;;compute f(w) = ww
(define ww (make-unchecked-mttm '(K H T F  B W D U M)
                      '(a b)
                      'K
                      '(M)
              (list
                (list (list 'K (list BLANK BLANK)) ;;<--- start
                      (list 'H (list RIGHT RIGHT))) 
                (list (list 'H (list 'a BLANK)) ;;<---- PHASE 1: read a in w 
                      (list 'T (list 'a 'a)))
                (list (list 'T (list 'a 'a))
                      (list 'H (list RIGHT RIGHT)))
                (list (list 'H (list 'b BLANK)) ;;<---- PHASE 1: read b in w
                      (list 'F (list 'b 'b)))
                (list (list 'F (list 'b 'b))
                      (list 'H (list RIGHT RIGHT)))
                (list (list 'H (list BLANK BLANK)) ;;<--- PHASE 2: Go to beginning of t1
                      (list 'B (list BLANK LEFT)))
                (list (list 'B (list BLANK 'a))
                      (list 'B (list BLANK LEFT)))
                (list (list 'B (list BLANK 'b))
                      (list 'B (list BLANK LEFT)))
                (list (list 'B (list BLANK BLANK)) ;;<---- PHASE 3: read w on t1 AND write w on t0
                      (list 'W (list BLANK RIGHT)))
                (list (list 'W (list BLANK 'a))
                      (list 'D (list 'a 'a)))
                (list (list 'D (list 'a 'a))
                      (list 'W (list RIGHT RIGHT)))
                (list (list 'W (list BLANK 'b))
                      (list 'U (list 'b 'b)))
                (list (list 'U (list 'b 'b))
                      (list 'W (list RIGHT RIGHT)))
                (list (list 'W (list BLANK BLANK))
                      (list 'M (list BLANK BLANK)))                                    
                )        
            2))
