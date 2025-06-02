#lang racket

(require "../2htdp/image.rkt"
         "../viz-lib/zipper.rkt"
         "../viz-lib/viz-imgs/keyboard_bitmaps.rkt"
         "../viz-lib/default-viz-function-generators.rkt"
         "../../fsm-core/private/constants.rkt"
         "../viz-lib/zipper.rkt"
         racket/treelist
         "../viz-lib/viz-constants.rkt"
         "../../fsm-core/private/fsa.rkt"
         "../../fsm-core/private/pda.rkt"
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/mtape-tm.rkt")

(provide (all-defined-out))

#|
A trace is a structure:
(make-trace config rules)
config is a single configuration
rules are a (listof rule-structs)
|#
(struct trace (config rules) #:transparent)
;;state -> the state the configuration is in                | symbol
;;word  -> the consumed input once reaching the given state | (listof symbol)
;;stack -> the stack once reaching the given state          | (listof symbol)
;;index -> the number associated with the configuration     | natnum
(struct pda-config (state word stack index) #:transparent)
;;state -> the state the configuration is in                | symbol
;;word  -> the consumed input once reaching the given state | (listof symbol)
;;index -> the number associated with the configuration     | natnum
(struct ndfa-config (state word index) #:transparent)
;;state -> the state that the configuration is in                                | symbol
;;head-position -> the current head position under the tape at the configuration | natnum
;;tape -> the turing machine's tape                                              | (listof symbol)
;;index -> the number associated with the configuration                          | natnum
(struct tm-config (state head-position tape index) #:transparent)
;;head-position -> the head position of the tape | natnum
;;tape -> the listof letters being read/written  | (listof symbol)
(struct tape-config (head-position tape) #:transparent)
;;state -> the state that the configuration is in                               | symbol
;;lotc  -> all of the tape configurations associated with current configuration | (listof tape-config)
;;index -> the number associated with the configuration                         | natnum
(struct mttm-config (state lotc index) #:transparent)
;;state -> all of the states that the tm has    | (listof symbol
;;sigma -> all of letters the tm can read/write | (listof symbol)
;;rules -> the transition rules for the tm      | (treelistof tm-rule)
;;start -> the starting state                   | symbol
;;finals -> the final states                    | (listof symbol)
;;accepting-final -> the accepting final state  | symbol
;;type -> the type of tm                        | symbol
(struct tm (states sigma rules start finals accepting-final type) #:transparent)
;;state -> all of the states that the mttm has    | (listof symbol
;;sigma -> all of letters the mttm can read/write | (listof symbol)
;;start -> the starting state                     | symbol
;;finals -> the final states                      | (listof symbol)
;;rules -> the transition rules for the mttm      | (treelistof mttm-rule)
;;tape-amount -> the number of tapes the mttm has | natnum
;;accepting-final -> the accepting final state    | symbol
;;type -> the type of mttm                        | symbol
(struct mttm (states sigma start finals rules tape-amount accepting-final type) #:transparent)
;;upci -> the unprocessed consumed input | (listof symbol)
;;pci  -> the processed consumed input   | (listof symbol)
(struct ci (upci pci) #:transparent)
;; a structure containing the color scheme used in the vizs 
(struct color-palette (shown-accept-color other-accept-color shown-reject-color other-reject-color cut-off-color inv-hold-color inv-fail-color
                       split-inv-color split-accept-color split-accept-reject-color split-reject-color bi-accept-reject-color
                       font-color blank-color computation-length-color imsg-accept-color imsg-reject-color ismg-cut-off-color start-state-color
                       faded-word-color))

(define DARKGOLDENROD2 (make-color 238 173 14))
(define DEEPBLUE (make-color 6 77 115))

;;no color blind color sheme
(define standard-color-scheme (color-palette
                               'forestgreen 'green 'chocolate1 'violetred 'darkgoldenrod2 'chartreuse4 'red2
                               "red:chartreuse4" "forestgreen:green" "forestgreen:violetred"  "chocolate1:violetred" "forestgreen:green:violetred"
                               'black 'white 'brown (make-color 34 139 34) 'red DARKGOLDENROD2 'green 'gray))
;;green color blind color scheme
(define deuteranopia-color-scheme (color-palette
                                   "#56B4E9" "#208FCD" "#999AA7" "#057F5E" 'darkgoldenrod2 "#064D73" "#585A5E"
                                   "#585A5E:#064D73" "#56B4E9:#208FCD" "#56B4E9:#057F5E" "#999AA7:#057F5E"  "#585A5E:#064D73:#057F5E"
                                   'black 'white 'brown DEEPBLUE (make-color 88 90 94) DARKGOLDENROD2 "#208FCD" 'gray))
;;red color blind color scheme
(define protanopia-color-scheme (color-palette
                                 "#56B4E9" "#208FCD" "#417360" "#1CBB90" 'darkgoldenrod2 "#064D73" "#057F5E"
                                 "#057F5E:#064D73" "#56B4E9:#208FCD" "#56B4E9:#1CBB90" "#417360:#1CBB90" "#56B4E9:#208FCD:#1CBB90"
                                 'black 'white 'brown DEEPBLUE (make-color 5 127 94) DARKGOLDENROD2 "#208FCD" 'gray))
;;blue color blind color scheme
(define tritanopia-color-scheme standard-color-scheme)

;;node-attributes -> all the attributes needed to create the node graphs for the viz | node-data
;;edge-attributes -> all the attributes needed to create the edge graphs for the viz | edge-data
(struct graph-attributes (node-attributes edge-attributes))

(struct node-data (inv-node bi-inv-node dead-node regular-node final-state accepting-final-state regular-state bi-inv-font regular-font))

(define default-node-attributes (node-data 'filled 'wedged 'dashed 'solid 'doublecircle 'doubleoctagon 'circle "times-bold" "Times-Roman"))

(struct edge-data (accept-edge reject-edge dead-edge regular-edge))

(define default-edge-attributes (edge-data 'bold 'dashed 'dotted 'solid))

(define default-graph-attributes (graph-attributes default-node-attributes default-edge-attributes))

#|
A computation is a structure: (computation LoC LoR visited length)
LoC -> all of the configurations that make up this computation              | (treelistof configuration)
LoR -> all of the rules used to reach the current configuration             | (treelistof rule)
visited -> all of previously visited configurations for this comptuation    | (hashof configuration)
length -> the current number of configurations visited for this computation | positive integer
|#
(struct computation (LoC LoR visited length) #:transparent)
;;accepting -> all of the computations that the machine accepts      | (treelistof computation)
;;rejecting -> all of the computations that the machine rejects      | (treelistof computation)
;;reached-final? -> has any of the computations reached a final state| boolean
;;cut-off? -> has any of the computations been cut off               | boolean 
(struct paths (accepting rejecting reached-final? cut-off?) #:transparent)  
#|
A rule is a structure: (rule source read destination action)
source is the source state                       | symbol
read is the element to be read on the tape       | symbol
destination is the destination state             | symbol
action is the action to be performed on the tape | TM-ACTION
|#
(struct rule (source read destination action) #:transparent)

;; TM observers
(define (tm-getalphabet m) (m '() 0 'get-alphabet)) 
  
(define (tm-getstates m) (m '() 0 'get-states))
  
(define (tm-getfinals m) (m '() 0 'get-finals))

(define (tm-getdelta m) (m '() 0 'get-delta)) ;;; parsed rules

(define (tm-getrules m) (m '() 0 'get-rules))  ;;; unparsed rules

(define (tm-getstart m) (m '() 0 'get-start))
  
(define (tm-getaccept m) (m '() 0 'get-accept))

(define (tm-whatami? m) (m 'whatami 0 'whatami))

;; X (listof X) (X -> boolean) -> boolean
;;Purpose: Determine if X is in the given list
(define (member? x lst eq-func) (for/or ([L lst]) (eq-func x L)))
;;(X -> boolean) (listof X) -> boolean
;;Purpose: Determines if X is is the given list
(define (ormap f lst) (for/or ([L lst]) (f L)))
;;(X -> Y) (listof X) -> (listof Y)
;;Purpose: maps the given functon over the list
(define (map2 f lst) (for/list ([L lst]) (f L)))
;;(X -> boolean) (listof X) -> (listof X)
;;Purpose: filters the list using the given predicate
(define (filter f lst) (for/list ([L lst] #:when (f L)) L))

;;(X -> Y) (X -> Y) (X -> Y) (X -> Y) (listof (listof X)) -> (listof (listof X))
;;Purpose: filtermaps the given f-on-x on the given (listof (listof X))
(define (filter-map-acc filter-func map-func bool-func accessor a-lolox)
  (filter-map (λ (x)
                (and (bool-func (filter-func x))
                     (map-func (accessor x))))
              a-lolox))

;;(X -> Y) :Purpose: A function to retrieve the index for a tm-config from a trace
(define get-mttm-config-index-frm-trace (compose1 mttm-config-index trace-config zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a tm-config from a trace
(define get-tm-config-index-frm-trace (compose1 tm-config-index trace-config zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a pda-config from a trace
(define get-pda-config-index-frm-trace (compose1 pda-config-index trace-config zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a ndfa-config from a trace
(define get-ndfa-config-index-frm-trace (compose1 ndfa-config-index trace-config zipper-current))

;;(X -> Y) :Purpose: A function to retrieve the index for a mttm-config from the invs-zipper
(define get-mttm-config-index-frm-invs (compose1 mttm-config-index zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a tm-config from the invs-zipper
(define get-tm-config-index-frm-invs (compose1 tm-config-index zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a pda-config from the invs-zipper
(define get-pda-config-index-frm-invs (compose1 pda-config-index first zipper-current))
;;(X -> Y) :Purpose: A function to retrieve the index for a ndfa-config from the invs-zipper
(define get-ndfa-config-index-frm-invs (compose1 ndfa-config-index first zipper-current))

(define SM-VIZ-FONT-SIZE 18)

(define qempty? treelist-empty?)

(define E-QUEUE empty-treelist) 

;; (qof X) → X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (treelist-first a-qox)))

;; (listof X) (qof X) → (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (treelist-append a-qox a-lox))

;; (qof X) → (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (treelist-rest a-qox)))
;;tm -> tm-struct
;;Purpose: Converts the tm into a tm structure
(define (remake-tm M)
  ;;(listof rules) -> (treelistof rule-struct)
  ;;Purpose: Converts the rules from the given tm to rule-structs
  (define (remake-rules a-lor)
    (for/treelist ([tm-rule a-lor])
      (rule (first (first tm-rule)) (second (first tm-rule))
            (first (second tm-rule)) (second (second tm-rule)))))
  (tm (tm-getstates M)
      (tm-getalphabet M)
      (remake-rules (tm-getrules M))
      (tm-getstart M)
      (tm-getfinals M)
      (if (eq? (tm-whatami? M) 'tm-language-recognizer) (tm-getaccept M) 'none)
      (tm-whatami? M)))

;;Mttm -> mttm-struct
;;Purpose: Converts a mttm interface into the mttm structure
(define (remake-mttm M)
  ;;(listof rule) -> (treelistof rule-struct)
  ;;Purose: Converts a rule into a rule-struct
  (define (remake-rules a-lor)
    (for/treelist ([mttm-rule a-lor])
      (rule (first (first mttm-rule)) (second (first mttm-rule))
            (first (second mttm-rule)) (second (second mttm-rule)))))
  (mttm (mttm-get-states M)
        (mttm-get-sigma M)
        (mttm-get-start M)
        (mttm-get-finals M)
        (remake-rules (mttm-get-rules M))
        (M 'get-numtapes)
        (if (eq? (mttm-what-am-i M) 'mttm-language-recognizer) (mttm-get-accept M) 'none)
        (mttm-what-am-i M)))


(define AB*B*UAB*
  (make-unchecked-ndfa '(S K B C H)
             '(a b)
             'S
             '(H)
             `((S ,EMP K) (S a C)
                          (K a B) (K ,EMP H)
                          (B b K)
                          (C ,EMP H)
                          (H b H))))

(define a* (make-unchecked-ndpda '(K H)
                                 '(a b)
                                 '(a)
                                 'K
                                 '(H)
                                 `(((K ,EMP ,EMP)(H ,EMP))
                                   ((H a ,EMP)(H ,EMP)))))

(define EVEN-AS-&-BS (remake-tm (make-unchecked-tm '(K H I B S)
                                                   '(a b)
                                                   `(((K ,BLANK) (S ,BLANK))
                                                     ((K a) (H ,RIGHT)) ((H a) (K ,RIGHT)) ((H b) (B ,RIGHT)) ((B b) (H ,RIGHT))
                                                     ((K b) (I ,RIGHT)) ((I b) (K ,RIGHT)) ((I a) (B ,RIGHT)) ((B a) (I ,RIGHT)))
                                                   'K
                                                   '(S)
                                                   'S)))

(define ww (remake-mttm (make-unchecked-mttm '(K H T F E B W D M)
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
                                       (list 'E (list BLANK LEFT)))
                                 (list (list 'E (list BLANK 'a))
                                       (list 'E (list BLANK LEFT)))
                                 (list (list 'E (list BLANK 'b))
                                       (list 'E (list BLANK LEFT)))
                                 (list (list 'E (list BLANK BLANK)) ;;<---- PHASE 3: read w on t1 AND write w on t0
                                       (list 'W (list BLANK RIGHT)))
                                 (list (list 'W (list BLANK 'a))
                                       (list 'D (list 'a 'a)))
                                 (list (list 'D (list 'a 'a))
                                       (list 'W (list RIGHT RIGHT)))
                                 (list (list 'W (list BLANK 'b))
                                       (list 'B (list 'b 'b)))
                                 (list (list 'B (list 'b 'b))
                                       (list 'W (list RIGHT RIGHT)))
                                 (list (list 'W (list BLANK BLANK))
                                       (list 'M (list BLANK BLANK)))
                                 )        
                                2)))

(define a^nb^n (remake-mttm (make-unchecked-mttm '(K H R E C O M T)
                                       '(a b)
                                       'K
                                       '(T)
                                       (list
                                        (list (list 'K (list BLANK BLANK BLANK));; <-- Starting 
                                              (list 'H (list RIGHT RIGHT RIGHT))) 
                                        (list (list 'H (list 'a BLANK BLANK)) ;;<-- Phase 1, reads a's
                                              (list 'R (list 'a 'a BLANK)))
                                        (list (list 'R (list 'a 'a BLANK))
                                              (list 'H (list RIGHT RIGHT BLANK))) 
                                        (list (list 'H (list 'b BLANK BLANK)) ;;<-- phase 2, read b's
                                              (list 'E (list 'b BLANK 'b)))
                                        (list (list 'E (list 'b BLANK 'b))
                                              (list 'C (list RIGHT BLANK RIGHT)))
                                        (list (list 'C (list 'b BLANK BLANK))
                                              (list 'E (list 'b BLANK 'b))) 
                                        (list (list 'C (list 'b BLANK BLANK))
                                              (list 'O (list RIGHT BLANK BLANK)))
                                        (list (list 'O (list BLANK BLANK BLANK)) ;;<-- phase 4, matching as, bs, cs
                                              (list 'M (list BLANK LEFT LEFT)))
                                        (list (list 'M (list BLANK 'a 'b))
                                              (list 'M (list BLANK LEFT LEFT)))
                                        (list (list 'M (list BLANK BLANK BLANK)) ;;<-phase 5, accept (if possible)
                                              (list 'T (list BLANK BLANK BLANK)))
                                        )
                                       3
                                       'T)))

(define a^nb^nc^n (remake-mttm (make-unchecked-mttm '(K H R E C O M T F)
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
                                       'F)))

(define E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER SM-VIZ-FONT-SIZE
                                                   (list (list ARROW-UP-KEY "Restart")
                                                         (list ARROW-RIGHT-KEY "Forward")
                                                         (list ARROW-LEFT-KEY "Backward")
                                                         (list ARROW-DOWN-KEY "Finish")
                                                         (list CURSOR "Hold to drag")
                                                         (list W-KEY "Zoom in")
                                                         (list S-KEY "Zoom out")
                                                         (list R-KEY "Min zoom")
                                                         (list E-KEY "Mid zoom")
                                                         (list F-KEY "Max zoom")
                                                         (list A-KEY "Word start")
                                                         (list D-KEY "Word end")
                                                         (list J-KEY "Prv not inv")
                                                         (list L-KEY "Nxt not inv"))))

(define MTTM-E-SCENE-TOOLS (e-scene-tools-generator HEIGHT-BUFFER LETTER-KEY-WIDTH-BUFFER SM-VIZ-FONT-SIZE
                                                    (list (list ARROW-UP-KEY "Restart")
                                                          (list ARROW-RIGHT-KEY "Forward")
                                                          (list ARROW-LEFT-KEY "Backward")
                                                          (list ARROW-DOWN-KEY "Finish")
                                                          (list CURSOR "Hold to drag")
                                                          (list W-KEY "Zoom in")
                                                          (list S-KEY "Zoom out")
                                                          (list R-KEY "Min zoom")
                                                          (list E-KEY "Tape up")
                                                          (list F-KEY "Tape down")
                                                          (list A-KEY "Word start")
                                                          (list D-KEY "Word end")
                                                          (list J-KEY "Prv not inv")
                                                          (list L-KEY "Nxt not inv"))))