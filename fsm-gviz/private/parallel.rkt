#lang racket/base

(require "lib.rkt"
         2htdp/image
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
(define (parallel-graphs->bitmap-thunks graphs #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  (begin
    (define enumerated-graphs (make-pairs (range 0 (length graphs)) graphs))
    (define list-dot-files (for/list ([i enumerated-graphs])
                             (if (list? (second i))
                                 (for/list ([j (range 0 (length (second i)))])
                                   (format "~adot~s_~s" SAVE-DIR (first i) j)
                                   )
                                 (format "~adot~s" SAVE-DIR (first i))
                                 )
                             )
      )
    (graphs->dots enumerated-graphs)
    (parallel-dots->pngs (flatten list-dot-files) cpu-cores)
    (pngs->bitmap-thunks enumerated-graphs)
    )
  )

;; Procedure Listof Any -> Void
;; Runs a given function in parallel based on information gathered from the system
(define (streaming-parallel-shell func args min-idx max-idx cpu-cores system-os)
  (let [
        (cpu-cores-avail (make-semaphore cpu-cores))
        ]
    (define (make-thread a)
      (delay/sync (begin
                    (semaphore-wait cpu-cores-avail)
                    (let ([shell-process (func a)])
                      (let [
                            (result (sync (if (eq? system-os 'windows)
                                              (read-line-evt (first shell-process) 'any)
                                              (read-line-evt (first shell-process)))))
                            ]
                        (when (not (string=? "0" result))
                          (error (format "Graphviz produced an error while compiling the graphs: ~a" result)))
                        )
                      (close-input-port (first shell-process))
                      (close-output-port (second shell-process))
                      (close-input-port (fourth shell-process))
                      (semaphore-post cpu-cores-avail)
                      (lambda () (bitmap/file (string->path (format "~a.png" a))))
                      )
                    )
                  )
      )
    (for ([i (in-range min-idx max-idx)])
      (let ([cache (vector-ref args i)])
        (vector-set! args i (if (list? cache)
                                (map make-thread cache)
                                (make-thread cache)))
        )
      )
    )
  )


;; Listof String -> Void
;; Creates all the graphviz images
(define (streaming-parallel-dots->pngs dot-files min-idx max-idx cpu-cores)
  (define dot-executable-path (find-dot))
  (define dot-executable-path-string (path->string dot-executable-path))
  (define system-os (system-type))
  ;; On Mac/Linux we can bypass having to look at the systems PATH by instead
  ;; using the absolute path to the executable. For unknown reasons this does not
  ;; work on Windows so we will still use the PATH to call the dot executable
  ;; Additionally, we need to use a different shell command for windows systems in order to get a status code back 
  (define (make-process file-path) (process (if (eq? system-os 'windows)
                                                (format "~a -T~s ~s -o ~s & echo %errorlevel%"
                                                        "dot"
                                                        'png
                                                        (string-append file-path ".dot")
                                                        (string-append file-path ".png")
                                                        )
                                                (format "~a -T~s ~s -o ~s; echo $?"
                                                        dot-executable-path-string
                                                        'png
                                                        (string-append file-path ".dot")
                                                        (string-append file-path ".png")
                                                        )
                                                )
                                            )
    )
  (if (path? dot-executable-path)
      (streaming-parallel-shell make-process dot-files min-idx max-idx cpu-cores system-os)
      (error "Error caused when creating png file. This was probably due to the dot environment variable not existing on the path")))

(define (force-promises vec min-idx max-idx)
  (for ([i (in-range min-idx max-idx)])
    (if (list? (vector-ref vec i))
        (map force (vector-ref vec i))
        (force (vector-ref vec i)))))

;; Listof graph -> Listof Thunk
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
(define (streaming-parallel-graphs->bitmap-thunks graphs #:rank-node-lst [rank-node-lst '()] #:graph-type [graph-type 'rg] #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  
  
  
  
  
  (begin
    (define enumerated-graphs (make-pairs (range 0 (length graphs)) graphs))
    (define list-dot-files (for/list ([i enumerated-graphs])
                             (if (list? (second i))
                                 (for/list ([j (in-range 0 (length (second i)))])
                                   (format "~adot~s_~s" SAVE-DIR (first i) j))
                                 (format "~adot~s" SAVE-DIR (first i)))))
    (cond [(eq? 'rg graph-type) (graphs->dots enumerated-graphs)]
          [(eq? 'cfg graph-type) (cfg-graphs->dots enumerated-graphs rank-node-lst)]
          [(eq? 'csg graph-type) (special-graphs->dots enumerated-graphs rank-node-lst)]
          [else (error "invalid graph type")])

    (define flattened-list-dot-files (list->vector list-dot-files))
    #;(displayln (format "~s ~s" (length flattened-list-dot-files) (length (flatten graphs))))

    (if (> (vector-length flattened-list-dot-files) (* NUM-PRELOAD 2))
        (begin
          (streaming-parallel-dots->pngs flattened-list-dot-files 0 (vector-length flattened-list-dot-files) cpu-cores)
          (force-promises flattened-list-dot-files 0 NUM-PRELOAD)
          #;(for ([i (in-range 0 NUM-PRELOAD)])
              (force (vector-ref flattened-list-dot-files i)))
          (force-promises flattened-list-dot-files (- (vector-length flattened-list-dot-files) NUM-PRELOAD) (vector-length flattened-list-dot-files))
          #;(for ([i (in-range (- (vector-length flattened-list-dot-files) NUM-PRELOAD) (vector-length flattened-list-dot-files))])
              (force (vector-ref flattened-list-dot-files i)))

          (thread (lambda () (for ([i (in-range NUM-PRELOAD (- (vector-length flattened-list-dot-files) NUM-PRELOAD))])
                           (force (vector-ref flattened-list-dot-files i)))))
          )
        (begin
          (streaming-parallel-dots->pngs flattened-list-dot-files 0 (vector-length flattened-list-dot-files) cpu-cores)
          (for ([i (in-range 0 (vector-length flattened-list-dot-files))])
            (force (vector-ref flattened-list-dot-files i)))
          ))
    flattened-list-dot-files))

;; Listof graph -> Num
;; Creates all of the dotfiles based on the graph structs given
(define (special-graphs->dots enumerated-graphs rank-node-lst)
  (for ([i enumerated-graphs]
        [z rank-node-lst])
    (if (list? (second i))
        (for ([j (range 0 (length (second i)))]
              [k (second i)])
          (special-graph->dot k z SAVE-DIR (format "dot~s_~s" (first i) j))
          )
        (special-graph->dot (second i) z SAVE-DIR (format "dot~s" (first i)))
        )
    )
  )

;; Listof graph -> Num
;; Creates all of the dotfiles based on the graph structs given
(define (cfg-graphs->dots enumerated-graphs rank-node-lst) ;(displayln rank-node-lst)
  (for/list/concurrent ([i enumerated-graphs]
                        [z rank-node-lst])
    (if (list? (second i))
        (for ([j (range 0 (length (second i)))]
              [k (second i)])
          (cfg-graph->dot k z SAVE-DIR (format "dot~s_~s" (first i) j))
          )
        (cfg-graph->dot (second i) z SAVE-DIR (format "dot~s" (first i)))
        )
    )
  )


;; (listof graph) (listof (listof symbol)) nat -> (listof thunk)
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
(define (parallel-special-graphs->bitmap-thunks graphs rank-node-lst #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  (begin
    (define enumerated-graphs (make-pairs (range 0 (length graphs)) graphs))
    (define list-dot-files (for/list ([i enumerated-graphs])
                             (if (list? (second i))
                                 (for/list ([j (range 0 (length (second i)))])
                                   (format "~adot~s_~s" SAVE-DIR (first i) j)
                                   )
                                 (format "~adot~s" SAVE-DIR (first i))
                                 )
                             )
      )
    (special-graphs->dots enumerated-graphs rank-node-lst)
    (parallel-dots->pngs (flatten list-dot-files) cpu-cores)
    (pngs->bitmap-thunks enumerated-graphs)
    )
  )

;; (listof graph) (listof (listof symbol)) nat -> (listof thunk)
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
(define (parallel-cfg-graphs->bitmap-thunks graphs rank-node-lst #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  (begin
    (define enumerated-graphs (make-pairs (range 0 (length graphs)) graphs))
    (define list-dot-files (for/list ([i enumerated-graphs])
                             (if (list? (second i))
                                 (for/list ([j (range 0 (length (second i)))])
                                   (format "~adot~s_~s" SAVE-DIR (first i) j)
                                   )
                                 (format "~adot~s" SAVE-DIR (first i))
                                 )
                             )
      )
    (cfg-graphs->dots enumerated-graphs rank-node-lst)
    (parallel-dots->pngs (flatten list-dot-files) cpu-cores)
    (pngs->bitmap-thunks enumerated-graphs)
    )
  )