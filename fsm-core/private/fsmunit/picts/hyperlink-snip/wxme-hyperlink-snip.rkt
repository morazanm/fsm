#lang racket/base
  (require racket/class
           racket/format
           wxme
           pict)
   
  (provide reader)
   
  (define hyperlink-reader%
    (class* object% (snip-reader<%>)
      (define/public (read-header version stream) (void))
      (define/public (read-snip text-only? version stream)
        (define text-str (send stream read-unterminated-bytes "hyperlink-snip"))
        (define url-str (send stream read-unterminated-bytes "hyperlink-snip"))
        (cond
          [text-only?
           (string->bytes/utf-8 (~s `(text ,(bytes->string/utf-8 text-str))))]
          [else
           (new hyperlink-readable [text-str text-str] [url-str url-str])]))
      (super-new)))
   
  (define hyperlink-readable
    (class* object% (readable<%>)
      (init-field text-str)
      (init-field url-str)
      (define/public (read-special source line column position)
        ;; construct a syntax object holding a 3d value that
        ;; is a circle from the pict library with an appropriate
        ;; source location
        (datum->syntax #f
                       (text text-str)
                       (vector source line column position 1)
                       #f))
      (super-new)))
   
  (define reader (new hyperlink-reader%))