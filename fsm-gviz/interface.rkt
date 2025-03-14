#lang racket/base
(require "private/adapter.rkt"
         "private/lib.rkt"
         "private/dot.rkt"
         "../fsm-core/private/sm-getters.rkt"
         "../fsm-gui/structs/machine.rkt"
         "../fsm-gui/structs/state.rkt"
         "../visualizations/2htdp/image.rkt"
         racket/match
         racket/contract/base)

; *Color Blind States*
; 0 -> default colors
; 1 -> Deuteranopia (Red-Green colorblindness)
; 2 -> Deuteranopia (alt colors)
; 3 -> Blue-Red
(define (colorblind-opt? n)
  (and (>= n 0) (<= n 2)))

(define SAVE-DIR (find-tmp-dir))

(provide
 (except-out (all-from-out "private/lib.rkt") stringify-value)
 (all-from-out "private/dot.rkt")
 machine->graph ;; Used for testing
 fsa->graph ;; Used for testing
 (contract-out
  [fsa->bitmap (-> any/c colorblind-opt? image?)]
  [machine->bitmap (-> machine?
                       colorblind-opt?
                       (or/c dfa/ndfa-rule? pda/tm-rule? boolean?)
                       (or/c symbol? boolean?)
                       inv-state?
                       image?)]))

(define (make-color-palette opt)
  (match opt
    (0 (color-palette "forestgreen" "#00ab037F" "#f523147F" "#215dde"))
    (1 (color-palette "#ede209" "#7b32947F" "#ffea037F" "#215dde"))
    (2 (color-palette "#ac8421" "#ac8421" "#8f6d00" "#0090ee"))
    (_ (error "Invalid color blind option supplied"))))


(define (is-tm-lang-rec? type) (equal? type 'tm-language-recognizer))

;; fsa->graph :: fsa -> graph
;; converts the fsa to a graphviz graph
(define (fsa->graph fsa color-blind-mode)
  (define type (sm-type fsa))
  (when (or (equal? type 'mttm) (equal? type 'mttm-language-recognizer))
    (error 'sm-graph "Graphing a Multi-tape Turing Machine is not supported"))
  (define adapter (fsa-adapter (sm-states fsa)
                               (sm-start fsa)
                               (sm-finals fsa)
                               (sm-rules fsa)
                               (sm-type fsa)
                               (if (is-tm-lang-rec? (sm-type fsa))
                                   (sm-accept fsa)
                                   #f)
                               #f
                               #f
                               'none
                               (make-color-palette color-blind-mode)))     
  (fsa-adapter->graph adapter))


;; fsa->graph :: fsa -> image
;; converts the fsa to a image
(define (fsa->bitmap fsa color-blind-mode)
  (graph->bitmap (fsa->graph fsa color-blind-mode) #:directory SAVE-DIR))


;; machine->graph :: machine -> symbol -> symbol -> symbol -> graph
;; converts the machine to a graphviz graph
(define (machine->graph machine color-blind-mode cur-rule cur-state inv-state)
  (define adapter (fsa-adapter
                   (map (lambda (s) (fsm-state-name s))
                        (machine-state-list machine))
                   (machine-start-state machine)
                   (machine-final-state-list machine)
                   (machine-rule-list machine)
                   (machine-type machine)
                   (if (is-tm-lang-rec? (machine-type machine))
                       (lang-rec-machine-accept-state machine)
                       #f)
                   cur-state
                   cur-rule
                   inv-state
                   (make-color-palette color-blind-mode)))
  (fsa-adapter->graph adapter))

;; machine->graph :: machine -> symbol -> symbol -> symbol -> image
;; coverts the machine to a image
(define (machine->bitmap machine color-blind-mode cur-rule cur-state inv-state)
  (graph->bitmap (machine->graph
                  machine
                  color-blind-mode
                  cur-rule
                  cur-state
                  inv-state)
                 #:directory SAVE-DIR
                 #:filename "vizTool"))
