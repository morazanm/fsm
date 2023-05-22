#lang racket
(require "private/adapter.rkt"
         "private/lib.rkt"
         "private/dot.rkt"
         "../fsm-core/interface.rkt"
         "../fsm-gui/legacy-gui/structs/machine.rkt"
         "../fsm-gui/legacy-gui/structs/state.rkt")

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
 electron-machine->svg
 (contract-out
  [fsa->bitmap (-> any/c colorblind-opt? image?)]
  [fsa->svg (-> any/c colorblind-opt? path?)]
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
    (2 (color-palette "#d48217" "TODO" "TODO" "TODO"))
    (_ (error "Invalid color blind option supplied"))))


(define (is-tm-lang-rec? type) (equal? type 'tm-language-recognizer))

;; fsa->graph :: fsa -> graph
;; converts the fsa to a graphviz graph
(define (fsa->graph fsa color-blind-mode)
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
  (graph->bitmap (fsa->graph fsa color-blind-mode) SAVE-DIR "vizTool"))

;; fsa-graph :: fsa -> filepath
;; returns the filepath the the svg image
(define (fsa->svg fsa color-blind-mode)
  (graph->svg (fsa->graph fsa color-blind-mode) SAVE-DIR "vizTool"))

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
                 SAVE-DIR
                 "vizTool"))

(define (electron-machine->svg states start finals rules type t accept cb-mode index)
  ;; converts the hash to a fsm-core representation of a rule
  (define (hash-rule->fsa-rule r)
    (match type
      [(or 'dfa 'ndfa) (list (string->symbol (hash-ref r 'start))
                             (string->symbol (hash-ref r 'input))
                             (string->symbol (hash-ref r 'end)))]
      ['pda
       (define popped (hash-ref r 'popped))
       (define pushed (hash-ref r 'pushed))
       (list (list (string->symbol (hash-ref r 'start))
                   (string->symbol (hash-ref r 'input))
                   (if (empty? popped) EMP (map string->symbol popped)))
             (list (string->symbol (hash-ref r 'end))
                   (if (empty? pushed) EMP (map string->symbol pushed))))]
      [(or 'tm 'tm-language-recognizer)
       (define start-tape (hash-ref r 'startTape))
       (define end-tape (hash-ref r 'endTape))
       ;; NOTE: We represent TM-actions (LM, _, @,) as a string, but a input as a single
       ;; value in a list for parsing reason on the GUI end. Ideally this should be cleaned
       ;; up at a later date.
       (list (list (string->symbol (hash-ref r 'start))
                   (string->symbol (if (list? start-tape) (list-ref start-tape 0) start-tape)))
             (list (string->symbol (hash-ref r 'end))
                   (string->symbol (if (list? end-tape) (list-ref end-tape 0) end-tape))))]
      [_ (error 'parse-rules "Unsupported machine type ~a" type)]))
  (define cur-rule (hash-ref t 'rule #f))
  (define cur-state (if (hash-has-key? t 'rule)
                        (hash-ref (hash-ref t 'rule) 'end)
                        (hash-ref t 'start (hash-ref t 'end #f))))
  (define inv-pass (match (hash-ref t 'invPass)
                     [#t 'pass]
                     [#f 'fail]
                     [_ 'none]))
  (define adaptor (fsa-adapter
                   states
                   start
                   finals
                   rules
                   type
                   accept
                   (string->symbol cur-state)
                   (if cur-rule (hash-rule->fsa-rule cur-rule) cur-rule)
                   inv-pass
                   (make-color-palette cb-mode)))
  (graph->svg (fsa-adapter->graph adaptor) SAVE-DIR (format "viztool_~a" index)))
