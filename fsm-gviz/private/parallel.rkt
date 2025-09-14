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

;; On Mac/Linux we can bypass having to look at the systems PATH by instead
;; using the absolute path to the executable. For unknown reasons this does not
;; work on Windows so we will still use the PATH to call the dot executable
;; Additionally, we need to use a different shell command for windows systems in order to get a status code back 
(define (make-process-windows a-file-path)
  (process (format "~a -T~s ~s -o ~s & echo %errorlevel%"
                       "dot"
                       'png
                       (string-append a-file-path ".dot")
                       (string-append a-file-path ".png"))))

(define ((make-process-unix dot-executable-path) a-file-path)
  (process (format "~a -T~s ~s -o ~s; echo $?"
                       (path->string dot-executable-path)
                       'png
                       (string-append a-file-path ".dot")
                       (string-append a-file-path ".png"))))

(define (get-shell-process-result-windows shell-process-output)
  (sync (read-line-evt shell-process-output 'any)))

(define (get-shell-process-result-unix shell-process-output)
  (sync (read-line-evt shell-process-output)))

;; Listof graph -> Listof Thunk
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
(define (graphs->bitmap-thunks graphs
                               graphic-formatter-vec
                               #:rank-node-lst [rank-node-lst '()]
                               #:graph-type [graph-type 'rg]
                               #:cpu-cores [cpu-cores (void)])
  (define dot-executable-path (find-dot))
  (cond [(path? dot-executable-path)
         (define cpu-cores-avail (make-semaphore (if (void? cpu-cores)
                                                     (quotient (processor-count) 2)
                                                     cpu-cores)))
         
         (define graphs-len (length graphs))
         (define-values (get-shell-process-result make-process)
           (if (eq? (system-type) 'windows)
               (values get-shell-process-result-windows make-process-windows)
               (values get-shell-process-result-unix (make-process-unix dot-executable-path))))
         (define SAVE-DIR (find-tmp-dir))
         
         (define img-vec
           (cond [(eq? 'rg graph-type)
                  (for/vector/concurrent
                      #:length graphs-len
                    ([graph (in-list graphs)]
                     [idx (in-naturals)])
                    (if (list? graph)
                        (for/list/concurrent ([inner-graph (in-list graph)]
                                              [inner-idx (in-range (length graph))])
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
                        (for/list/concurrent ([inner-graph (in-list graph)]
                                              [inner-idx (in-naturals)])
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
                        (for/list/concurrent ([inner-graph (in-list graph)]
                                              [inner-idx (in-naturals)])
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
            (define shell-process (make-process))
            (define result (get-shell-process-result (first shell-process)))
            (close-input-port (first shell-process))
            (close-output-port (second shell-process))
            (close-input-port (fourth shell-process))
            (when (not (string=? "0" result))
              (error (format "Graphviz produced an error while compiling the graphs: ~a" result)))
            (semaphore-post cpu-cores-avail)
            (string->path (format "~a.png" a-file-path))))
        
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
               (compile-single-graph (vector-ref img-vec idx)
                                     (vector-ref graphic-formatter-vec idx)
                                     idx)))
         img-vec]
        [else (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")]))