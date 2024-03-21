#lang racket

;(#%declare #:unsafe)

(require ;"../fsm-gviz/private/lib.rkt"
         ;2htdp/universe
         ;2htdp/image
         racket/draw
         ;racket/unsafe/ops
         )

(define TEST-SAVE-DIR "/home/sora/Documents/demo-folder/")
(define SCRIPT-LOCATION "/home/sora/Documents/demo-folder/create-image-p.sh")

(define image-vec (make-vector 92 0))

;; nat lengthof listofgraphs
;; make this a for loop TODO
#;(define (collect-images2 accum cap) (begin
                                      (for/list ([i (range accum cap)])
                                        (vector-set! image-vec i (future (lambda () (bitmap/file (string->path (format "~adot~s.png" TEST-SAVE-DIR i))))))
                                        )
                                      (for/list ([i (range accum cap)])
                                        (vector-set! image-vec i (if (future? (vector-ref image-vec i))
                                                                     (touch (vector-ref image-vec i))
                                                                     (vector-ref image-vec i)
                                                                     )
                                                     )
                                        )
                                      )
  )

#;(define (collect-future-images accum cap) (if (> accum cap)
                                              '()
                                              (cons (lambda () (bitmap/file (string->path (format "~adot~s.png" TEST-SAVE-DIR accum))))
                                                    (collect-images (add1 accum) cap)
                                                    )
                                              )
  )

#;(define (collect-images accum cap) (let
                                       [
                                        (future-imgs (collect-future-images accum cap))
                                        ]
                                     (map (lambda (x) (if (future? x)
                                                          (touch x)
                                                          x
                                                          )
                                            )
                                          future-imgs)
                                     )
  )

(define (while-func cond thnk) (if (cond)
                                   (thnk)
                                   '()
                                   )
  )

(define (parallel-dot2 graphs) (let* [
                                    (graphs-length 75)
                                    (list-dot-files (for/list ([i (range 1 (add1 graphs-length))])
                                                      (format "~adot~s" TEST-SAVE-DIR i)
                                                      )
                                                    )
                                    ;(fh-dot-files (take (/ graphs-length 2)list-dot-files))
                                    ;(lh-dot-files (drop (length fh-dot-files) list-dot-files))
                                    
                                    ]
                                (begin
                                  #;(foldl (lambda (value accum)
                                           (begin (graph->dot value (string->path TEST-SAVE-DIR) (format "dot~s" accum))
                                                  (add1 accum)
                                                  )
                                           )
                                         1
                                         graphs)
                                  
                                  #;(system (format "~s ~s ~s" SCRIPT-LOCATION 1 graphs-length))
                                  
                                  (define processes (map (lambda (dot-path) (process (format "~a -T~s ~s -o ~s"
                                                                                             ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
                                                                                             ;; using the absolute path to the executable. For unknown reasons this does not
                                                                                             ;; work on Windows so we will still use the PATH to call the dot executable
                                                                                             "/run/current-system/sw/bin/dot"
                                                                                             'png
                                                                                             (string-append dot-path ".dot")
                                                                                             (string-append dot-path ".png")
                                                                                             )
                                                                                     )
                                                           )
                                                         list-dot-files)
                                        )
                                  (displayln "got through 1")
                                  (while-func (lambda () (andmap (lambda (process) (eq? 'done-ok ((last process) 'status))) processes)) (lambda () (sleep 1)))
                                  (displayln "got through 2")
                                  (map (lambda (process) (begin (close-input-port (first process))
                                                                (close-output-port (second process))
                                                                (close-input-port (fourth process))
                                                                )
                                         )
                                       processes)
                                  (displayln "got through 3")
                                  ;(collect-images2 1 graphs-length)
                                   
                                  ;(map graph->bitmap graphs)
                                  )
                                )
  )

(define result
  (time (parallel-dot2 '()))
  )