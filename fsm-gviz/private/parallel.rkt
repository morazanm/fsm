#lang racket/base

(require "lib.rkt"
         "../../visualizations/2htdp/image.rkt"
         "for-vector-concurrent.rkt"
         "dot.rkt"
         racket/list
         racket/future
         racket/port
         racket/system
         racket/promise)

(provide graphs->bitmap-thunks)

;; Listof graph -> Listof Thunk
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
(define (graphs->bitmap-thunks graphs
                               graphic-formatter-vec
                               #:rank-node-lst [rank-node-lst '()]
                               #:graph-type [graph-type 'rg]
                               #:cpu-cores [cpu-cores (void)])
  (define dot-executable-path (find-dot))
  (if (path? dot-executable-path)
      (let ([system-os (system-type)]
            [cpu-cores-avail (make-semaphore (if (void? cpu-cores)
                                                 (quotient (processor-count) 2)
                                                 cpu-cores))]
            [graphs-len (length graphs)]
            [dot-executable-path-string (path->string dot-executable-path)]
            [SAVE-DIR (find-tmp-dir)])
        
        ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
        ;; using the absolute path to the executable. For unknown reasons this does not
        ;; work on Windows so we will still use the PATH to call the dot executable
        ;; Additionally, we need to use a different shell command for windows systems in order to get a status code back 
        (define (make-process file-path)
          (process (if (eq? system-os 'windows)
                       (format "~a -T~s ~s -o ~s & echo %errorlevel%"
                               "dot"
                               'png
                               (string-append file-path ".dot")
                               (string-append file-path ".png"))
                       (format "~a -T~s ~s -o ~s; echo $?"
                               dot-executable-path-string
                               'png
                               (string-append file-path ".dot")
                               (string-append file-path ".png")))))

        (define img-vec
          (cond [(eq? 'rg graph-type)
                 (for/vector/concurrent
                     #:length graphs-len
                   ([graph (in-list graphs)]
                    [idx (in-naturals)])
                   (if (list? graph)
                       (for/list/concurrent ([inner-idx (in-range (length graph))]
                                             [inner-graph (in-list graph)])
                         (graph->dot inner-graph SAVE-DIR (format "dot~s_~s" idx inner-idx))
                         (format "~adot~s_~s" SAVE-DIR idx inner-idx))
                       (begin
                         (graph->dot graph SAVE-DIR (format "dot~s" idx))
                         (format "~adot~s" SAVE-DIR idx))))]
                [(eq? 'cfg graph-type)
                 (for/vector/concurrent
                     #:length graphs-len
                   ([graph (in-list graphs)]
                    [idx (in-naturals)]
                    [a-rank-node-lst (in-list rank-node-lst)])
                   (if (list? graph)
                       (for/list/concurrent ([inner-idx (in-naturals)]
                                             [inner-graph (in-list graph)])
                         (cfg-graph->dot inner-graph a-rank-node-lst SAVE-DIR (format "dot~s_~s" idx inner-idx))
                         (format "~adot~s_~s" SAVE-DIR idx inner-idx))
                       (begin
                         (cfg-graph->dot graph a-rank-node-lst SAVE-DIR (format "dot~s" idx))
                         (format "~adot~s" SAVE-DIR idx))))]
                [(eq? 'csg graph-type)
                 (for/vector/concurrent
                     #:length graphs-len
                   ([graph (in-list graphs)]
                    [idx (in-naturals)]
                    [a-rank-node-lst (in-list rank-node-lst)])
                   (if (list? graph)
                       (for/list/concurrent ([inner-idx (in-naturals)]
                                             [inner-graph (in-list graph)])
                         (special-graph->dot inner-graph a-rank-node-lst SAVE-DIR (format "dot~s_~s" idx inner-idx))
                         (format "~adot~s_~s" SAVE-DIR idx inner-idx))
                       (begin
                         (special-graph->dot graph a-rank-node-lst SAVE-DIR (format "dot~s" idx))
                         (format "~adot~s" SAVE-DIR idx))))]))
        
        (define graphviz-thread-group (make-thread-group))

        (define (make-thread a-file-path)
          (delay/thread
           #:group graphviz-thread-group
           (semaphore-wait cpu-cores-avail)
           (let* ([shell-process (make-process a-file-path)]
                  [result (sync (if (eq? system-os 'windows)
                                    (read-line-evt (first shell-process) 'any)
                                    (read-line-evt (first shell-process))))])
               
             (close-input-port (first shell-process))
             (close-output-port (second shell-process))
             (close-input-port (fourth shell-process))
             (when (not (string=? "0" result))
               (error (format "Graphviz produced an error while compiling the graphs: ~a" result)))
             (semaphore-post cpu-cores-avail)
             ;; TODO figure out how to get multiple graphs into singular thunk here
             (string->path (format "~a.png" a-file-path)))))
        
        (define (compile-single-graph a-file-path graphic-formatter idx)
          (define compiled-img-path
            (make-thread a-file-path))
          (vector-set! img-vec idx (lambda () (graphic-formatter (bitmap/file (force compiled-img-path))))))

        (define (compile-multiple-graphs a-lst-of-file-paths graphic-formatter idx)
          (define compiled-img-paths
            (map (lambda (a-file-path)
                   (make-thread a-file-path))
                 a-lst-of-file-paths))
          (vector-set! img-vec idx
                       (lambda () (apply graphic-formatter
                                   (map (lambda (img-path)  
                                     (bitmap/file (force img-path))) compiled-img-paths)))))
        (for ([idx (in-range (vector-length img-vec))])
          (if (list? (vector-ref img-vec idx))
              (compile-multiple-graphs (vector-ref img-vec idx)
                                       (vector-ref graphic-formatter-vec idx)
                                       idx)
              (begin
                (displayln "wtf")
                (compile-single-graph (vector-ref img-vec idx)
                                    (vector-ref graphic-formatter-vec idx)
                                    idx))))
        img-vec)
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")))