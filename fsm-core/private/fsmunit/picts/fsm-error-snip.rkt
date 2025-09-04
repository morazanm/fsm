#lang racket/base
(require racket/class
         racket/snip
         racket/format)
   
  (provide fsm-error-snip%
           #;(rename-out [fsm-error-snip-class snip-class]))
   
  (define fsm-error-snip%
    (class snip%
      (inherit set-snipclass
               get-flags set-flags
               get-admin)
      (init-field [size 20.0])
      
      (super-new)
      (set-snipclass fsm-error-snip-class)
      (send (get-the-snip-class-list) add fsm-error-snip-class)
      (set-flags (cons 'handles-events (get-flags)))
      
      (define/override (get-extent dc x y	 	 	 	 
                                   [w #f]
                                   [h #f]
                                   [descent #f]
                                   [space #f]
                                   [lspace #f]
                                   [rspace #f])
        (define (maybe-set-box! b v) (when b (set-box! b v)))
        (maybe-set-box! w (+ 2.0 size))
        (maybe-set-box! h (+ 2.0 size))
        (maybe-set-box! descent 1.0)
        (maybe-set-box! space 1.0)
        (maybe-set-box! lspace 1.0)
        (maybe-set-box! rspace 1.0))

      (define/override (resize w h)
        (send (send this get-admin) resized this #t))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (send dc draw-ellipse (+ x 1.0) (+ y 1.0) size size))
      
      (define/override (copy)
        (new fsm-error-snip% [size size]))

      (define/override (write f)
        (send f put size))
      
      (define/override (on-event dc x y editorx editory e)
        (when (send e button-down?)
          (set! size (+ 1.0 size))
          (displayln "working?")
          (define admin (get-admin))
          (when admin
            (send admin resized this #t))))))
   
  (define fsm-error-snip-class
    (class snip-class%
      (inherit set-classname)
      
      (super-new)
      (set-classname (~s '((lib "main.rkt" "fsm-error-snip")
                           (lib "wxme-fsm-error-snip.rkt" "fsm-error-snip"))))
      
      (define/override (read f)
        (define size-b (box 0.0))
        (send f get size-b)
        (new fsm-error-snip% [size (unbox size-b)]))))
#;(send (get-the-snip-class-list) add fsm-error-snip-class)
   
  #;(define fsm-error-snip-class (new fsm-error-snip-class%))