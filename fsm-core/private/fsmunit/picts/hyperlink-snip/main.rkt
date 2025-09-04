#lang racket/base
(require racket/class
         racket/snip
         racket/format
         browser/external
         "../../../../../visualizations/viz-grammar-constructors/rg-viz.rkt"
         "../../../../../fsm-core/private/regular-grammar.rkt"
         "../../../../../fsm-core/private/constants.rkt"
         )
   
(provide hyperlink-snip%
         (rename-out [hyperlink-snip-class snip-class]))

(define hyperlink-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init text-str)
    (init url-str)
    (define current-text-str text-str)
    (define current-url-str url-str)
      
    (super-new)
    (set-snipclass hyperlink-snip-class)
    (send (get-the-snip-class-list) add hyperlink-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
      
    (define/override (get-extent dc x y	 	 	 	 
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define-values (text-w text-h text-d text-a) (send dc get-text-extent current-text-str))
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w text-w)
      (maybe-set-box! h text-h))
      
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text current-text-str x y))
      
    (define/override (copy)
      (new hyperlink-snip% [text-str current-text-str] [url-str current-url-str]))
      
    (define/override (write f)
      (define text-byte-str (string->bytes/utf-8 current-text-str))
      (define url-byte-str (string->bytes/utf-8 current-url-str))
      (send f put (bytes-length text-byte-str) text-byte-str)
      (send f put (bytes-length url-byte-str) url-byte-str))
      
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (define even-bs-odd-as
          (make-unchecked-rg '(S A B C)
                             '(a b)
                             `((S ,ARROW aA) (S ,ARROW bB)
                                             (S ,ARROW a)
                                             (A ,ARROW aS)
                                             (A ,ARROW bC)
                                             (B ,ARROW aC)
                                             (B ,ARROW bS)
                                             (C ,ARROW aB)
                                             (C ,ARROW bA)
                                             (C ,ARROW b))
                             'S))
        (rg-viz even-bs-odd-as '(b a b))
        #;(send-url current-url-str)))))
   
(define hyperlink-snip-class%
  (class snip-class%
    (inherit set-classname)
      
    (super-new)
    (set-classname (~s '((lib "main.rkt" "hyperlink-snip")
                         (lib "wxme-hyperlink-snip.rkt" "hyperlink-snip"))))
      
    (define/override (read f)
      (define text-str (send f get-unterminated-bytes))
      (define url-str (send f get-unterminated-bytes))
      
      (new hyperlink-snip%
           [text-str (bytes->string/utf-8 text-str)]
           [url-str (bytes->string/utf-8 url-str)]))))
   
(define hyperlink-snip-class (new hyperlink-snip-class%))