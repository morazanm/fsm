#lang racket/base

(require "lib.rkt"
         "../../visualizations/2htdp/image.rkt"
         "dot.rkt"
         racket/list
         racket/future
         racket/port
         racket/system
         racket/promise
         )

(provide (all-defined-out))

(define NUM-PRELOAD 2)
(define SAVE-DIR (find-tmp-dir))

;; num num -> Listof Thunk
;; Creates a list of functions which when called will load its respective graph img from disk
(define (pngs->bitmap-thunks enumerated-graphs)
  (for/list ([i enumerated-graphs])
    (if (list? (second i))
        (for/list ([j (range 0 (length (second i)))])
          (lambda () (bitmap/file (string->path (format "~adot~s_~s.png" SAVE-DIR (first i) j))))
          )
        (lambda () (bitmap/file (string->path (format "~adot~s.png" SAVE-DIR (first i)))))
        )
    )
  )

;; (listof any) (listof any) -> (listof (list any any))
;; Combines two lists into a list of pairs
(define (make-pairs lst0 lst1) (if (empty? lst0)
                                   '()
                                   (cons (list (first lst0) (first lst1)) (make-pairs (rest lst0) (rest lst1)))
                                   )
  )

;; -> nat
;; Produces the number of cpus cores on the system
(define (find-number-of-cores) (processor-count))

;; Procedure Listof Any -> Void
;; Runs a given function in parallel based on information gathered from the system
(define (parallel-shell func args cpu-cores)
  (let [
        (cpu-cores-avail (make-semaphore cpu-cores))
        (system-os (system-type 'os))
        ]
    (define (make-thread a)
      (semaphore-wait cpu-cores-avail)
      (define shell-process (func a))
      (thread (lambda () (let [
                               (result (sync (if (eq? system-os 'windows)
                                                 (read-line-evt (first shell-process) 'any)
                                                 (read-line-evt (first shell-process))
                                                 )
                                             )
                                       )
                               ]
                           (if (= 0 (string->number result))
                               ;; This is thrown away, just doing this for the error check
                               result
                               (error (format "Graphviz produced an error while compiling the graphs: ~a" result))
                               )
                           )
                (close-input-port (first shell-process))
                (close-output-port (second shell-process))
                (close-input-port (fourth shell-process))
                (semaphore-post cpu-cores-avail)
                )
              )
      )
    (define graphviz-threads (map make-thread args))
    (for-each thread-wait graphviz-threads)
    )
  )

;; Listof String -> Void
;; Creates all the graphviz images
(define (parallel-dots->pngs dot-files cpu-cores)
  (define dot-executable-path (find-dot))
  ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
  ;; using the absolute path to the executable. For unknown reasons this does not
  ;; work on Windows so we will still use the PATH to call the dot executable
  ;; Additionally, we need to use a different shell command for windows systems in order to get a status code back 
  (define (make-process file-path) (process (if (equal? (system-type) 'windows)
                                                (format "~a -T~s ~s -o ~s & echo %errorlevel%"
                                                        "dot"
                                                        'png
                                                        (string-append file-path ".dot")
                                                        (string-append file-path ".png")
                                                        )
                                                (format "~a -T~s ~s -o ~s; echo $?"
                                                        (path->string dot-executable-path)
                                                        'png
                                                        (string-append file-path ".dot")
                                                        (string-append file-path ".png")
                                                        )
                                                )
                                            )
    )
  (if (path? dot-executable-path)
      (parallel-shell make-process dot-files cpu-cores)
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")
      )
                                          
  )

;; Listof graph -> Num
;; Creates all of the dotfiles based on the graph structs given
(define (graphs->dots enumerated-graphs)
  (for ([i enumerated-graphs])
    (if (list? (second i))
        (for ([j (range 0 (length (second i)))]
              [k (second i)])
          (graph->dot k SAVE-DIR (format "dot~s_~s" (first i) j))
          )
        (graph->dot (second i) SAVE-DIR (format "dot~s" (first i)))
        )
    )
  )


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
                          (format "~adot~s" SAVE-DIR idx))))]))
        
         (define graphviz-thread-group (make-thread-group))
         
         (define (make-thread a-file-path)
           (delay/thread
            #:group graphviz-thread-group
            (semaphore-wait cpu-cores-avail)
            (define shell-process (make-process a-file-path))
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
         #|
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
        |#

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
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")))
