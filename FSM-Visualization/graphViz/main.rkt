#lang racket
(require 2htdp/image
         "../inv.rkt"
         "../globals.rkt"
         "../structs/state.rkt"
         "../structs/machine.rkt"
         "../../GraphViz/interface.rkt")

(provide
 scaled-graph
 create-gql-png
 resize-image)

;; resize-image :: image -> int -> int -> image
;; Scales a image to the given dimentions. This solution was adapted from
;; one of the answers found here: https://stackoverflow.com/questions/3008772/how-to-smart-resize-a-displayed-image-to-original-aspect-ratio
(define (resize-image img max-width max-height)
  (define src-width (image-width img))
  (define src-height (image-height img))
  (define resize-width src-width)
  (define resize-height src-height)
  (define aspect (/ resize-width resize-height))
  (define scale (min
                 (/ max-width src-width) ; scale-x
                 (/ max-height src-height))) ;scale-y

  (set! resize-width (* resize-width scale))
  (set! resize-height (* resize-height scale))
  
  (when (> resize-width max-width)
    (set! resize-width max-width)
    (set! resize-height (/ resize-width aspect)))

  (when (> resize-height max-height)
    (set! aspect (/ resize-width resize-height))
    (set! resize-height max-height)
    (set! resize-width (* resize-height aspect)))

  (scale/xy
   (/ resize-width src-width)
   (/ resize-height src-height)
   img))

;; scaled-graph :: image -> machine-type -> image
;; scales a image base on the given machine type
(define (scaled-graph img type)
  (define image-area-width (if (eq? 'pda type) w-pda w))
  (define image-area-height (if (eq? 'pda type) h-pda h))
  ;; if the image fits on the screen by default we will not resize
  ;; becase it can effect the aspect ratio. In the future we may want to change this.
  ;; see: https://graphviz.org/docs/attrs/ratio/ for more details on img resizing
  (if (or (> (image-width img) image-area-width)
          (> (image-height img) image-area-height))
      (resize-image img image-area-width image-area-height)
      img))

;; create-gql-png :: machine -> bool -> symbol -> rule -> image
;; converts a machine to a graphviz internal representaion of a machine and
;; calls the gviz library to convert it to a png. Then we scale it to the viztool
;; if necessary.
(define (create-gql-png machine hasRun? cur-state cur-rule)
  (define inv-type (if hasRun? (determin-inv machine cur-state #:graphViz true) 'none))
  (scaled-graph (graph->bitmap
                 (machine->graph
                  machine
                  0 ;;TODO use color blind option
                  cur-rule
                  cur-state
                  inv-type)
                 (current-directory)
                 "vizTool")
                MACHINE-TYPE))