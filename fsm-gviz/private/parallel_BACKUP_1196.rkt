#lang racket
(require "lib.rkt"
         2htdp/image
         )

(provide parallel-graphs->bitmap-thunks
         parallel-special-graphs->bitmap-thunks)

(define SAVE-DIR (find-tmp-dir))


;; ImmutableHashTable formatter string -> Bytes
;; Converts a hash table to a byte string
(define (hash->bytes hash fmtr (spacer ", "))
  ;; Purpose: Formats a possibly boolean value to a symbol
  (define (fmt-val val)
    (if (boolean? val)
        (if val
            'true
            'false)
        val
        )
    )

  ;; Any Any -> Bytes
  ;; Converts a key value pair within the hash table to a byte string
  (define (key-val->bytes key value)
    (define fmtr-fun (hash-ref fmtr key #f))
    (if (procedure? fmtr-fun)
        (let ([o (open-output-bytes)])
          (fprintf o "~s=~s" key (fmt-val value))
          (get-output-bytes o))
        (let ([o (open-output-bytes)])
          (fprintf o "~s=~s" key (if (equal? key 'label)
                                     (let ([o (open-output-string)])
                                       (fprintf o "~a" (fmt-val value))
                                       (get-output-string o))
                                     (fmt-val value))
                   )
          (get-output-bytes o))
        )
    )
  
  (bytes-join (hash-map hash key-val->bytes) (string->bytes/locale spacer))
  )

;; edge formatter -> Bytes
;; Converts an edge to a byte string
(define (edge->bytes edge fmtr)
  (let ([o (open-output-bytes)])
    (fprintf o "    ~s -> ~s [~a];\n" (edge-start-node edge) (edge-end-node edge) (hash->str (edge-atb edge) fmtr))
    (get-output-bytes o))
  )

;; node formatter -> Bytes
;; Converts a node to a byte string
(define (node->bytes node fmtr-node)
  (let ([o (open-output-bytes)])
    (fprintf o "    ~s [~a];\n" (node-name node) (hash->str (node-atb node) fmtr-node))
    (get-output-bytes o))
  )

;; subgraph formatter -> Bytes
;; Converts a subgraph to a byte string
(define (subgraph->bytes sg fmtrs)
  (define name (let [
                     (sg-name (subgraph-name sg))
                     ]
                 (if (null? sg-name)
                     ""
                     (symbol->string sg-name)
                     )
                 )
    )
  (let [
        (fourspaces (string->bytes/locale "    "))
        ]
    (bytes-append (let ([o (open-output-bytes)])
                    (fprintf o "    subgraph ~s {\n" name)
                    (get-output-bytes o))
                  (let ([o (open-output-bytes)])
                    (fprintf o  "        ~a;\n" (hash->str (subgraph-atb sg) (formatters-graph fmtrs) ";\n        "))
                    (get-output-bytes o))
                  (foldl (lambda (n a) (bytes-append a fourspaces (node->bytes n (formatters-node fmtrs))))
                         #""
                         (subgraph-node-list sg))
                  (foldl (lambda (e a) (bytes-append a fourspaces (edge->bytes e (formatters-edge fmtrs))))
                         #""
                         (subgraph-edge-list sg))
                  (foldl (lambda (e a) (bytes-append a fourspaces (subgraph->bytes e fmtrs)))
                         #""
                         (subgraph-subgraph-list sg))
                  (string->bytes/locale "    }\n")
                  )
    )
  )

;; graph -> Bytes
;; Converts a graph to a byte string
(define (graph->bytes g)
  (define name (let ([o (open-output-bytes)])
                 (fprintf o "digraph ~s {\n" (graph-name g))
                 (get-output-bytes o))
    )

  (define fmtrs (graph-fmtrs g))
  (bytes-append
   name
   (let ([o (open-output-bytes)])
     (fprintf o "    ~a;\n" (hash->str (graph-atb g) (formatters-graph fmtrs) ";\n    "))
     (get-output-bytes o))
   (foldl (lambda (n a) (bytes-append a (node->bytes n (formatters-node fmtrs))))
          #""
          (graph-node-list g))
   (foldl (lambda (e a) (bytes-append a (edge->bytes e (formatters-edge fmtrs))))
          #""
          (graph-edge-list g))
   (foldl (lambda (e a) (bytes-append a (subgraph->bytes e fmtrs)))
          #""
          (graph-subgraph-list g))
   (string->bytes/locale "}")
   )
  )

;; graph path string -> Listof path
;; Creates the dotfile used to create its respective graph image
(define (graph->dot-bytes graph save-dir filename)
  (define dot-path (build-path save-dir (format "~a.dot" filename)))
  (define dotfile (open-output-file #:exists 'replace dot-path))
  (write-bytes (graph->bytes graph) dotfile)
  (close-output-port dotfile)
  dot-path
  )

;; num num -> Listof Thunk
;; Creates a list of functions which when called will load its respective graph img from disk
(define (pngs->bitmap-thunks accum cap)
  (if (>= accum cap)
      '()
      (cons (thunk (bitmap/file (string->path (format "~adot~s.png" SAVE-DIR accum))))
            (pngs->bitmap-thunks (add1 accum) cap)
            )
      )
  )

(define (find-number-of-cores) (let [
                                     (system-os (system-type 'os))
                                     ]
                                 (cond [(eq? system-os 'unix) (begin
                                                                (define p (process "grep -c '^processor' /proc/cpuinfo"))
                                                                (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                (close-input-port (first p))
                                                                (close-output-port (second p))
                                                                (close-input-port (fourth p))
                                                                event-result
                                                                )
                                                              ]
                                       [(eq? system-os 'windows) (begin
                                                                   (define p (process "echo %NUMBER_OF_PROCESSORS%"))
                                                                   ;; Have to change read-line-evt mode since windows returns a carriage return rather than a linebreak
                                                                   (define event-result (string->number (sync (read-line-evt (first p) 'any))))
                                                                   (close-input-port (first p))
                                                                   (close-output-port (second p))
                                                                   (close-input-port (fourth p))
                                                                   event-result
                                                                   )
                                                                 ]
                                       [(eq? system-os 'macos) (begin
                                                                 (define p (process "sysctl -n hw.ncpu"))
                                                                 (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                 (close-input-port (first p))
                                                                 (close-output-port (second p))
                                                                 (close-input-port (fourth p))
                                                                 event-result
                                                                 )
                                                               ]
                                       [(eq? system-os 'macosx) (begin
                                                                  (define p (process "sysctl -n hw.ncpu"))
                                                                  (define event-result (string->number (sync (read-line-evt (first p)))))
                                                                  (close-input-port (first p))
                                                                  (close-output-port (second p))
                                                                  (close-input-port (fourth p))
                                                                  event-result
                                                                  )
                                                                ]
                                       [else (error "Unknown system type, unable to intialize GraphViz in system shell")]
                                       )
                                 )
  )

;; Procedure Listof Any -> Void
;; Runs a given function in parallel based on information gathered from the system
(define (parallel-shell func args cpu-cores) (let [
                                                   (cpu-cores-avail (make-semaphore cpu-cores))
                                                   (system-os (system-type 'os))
                                                   ]
                                               (for/list ([a args])
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
(define (graphs->dots graphs) (foldl (lambda (value accum) (begin (graph->dot value SAVE-DIR (format "dot~s" accum))
                                                                  (add1 accum)
                                                                  )
                                       )
                                     0
                                     graphs
                                     )
  )

;; Listof graph -> Listof Thunk
;; Creates all the graph images needed in parallel, and returns a list of thunks that will load them from disk
<<<<<<< HEAD
(define (parallel-graphs->bitmap-thunks graphs #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  (begin
    (define list-dot-files (for/list ([i (range 0 (length graphs))])
                             (format "~adot~s" SAVE-DIR i)
                             )
      )
    (graphs->dots graphs)
    (parallel-dots->pngs list-dot-files cpu-cores)
    (pngs->bitmap-thunks 0 (length graphs))
    )
  )


(define (make-pairs lst0 lst1) (if (empty? lst0)
                                   '()
                                   (cons (list (first lst0) (first lst1)) (make-pairs (rest lst0) (rest lst1)))
                                   )
  )

;; Listof graph -> Num
;; Creates all of the dotfiles based on the graph structs given
(define (special-graphs->dots graphs rank-node-lst) (foldl (lambda (value accum) (begin (special-graph->dot (first value) (second value) SAVE-DIR (format "dot~s" accum))
                                                                  (add1 accum)
                                                                  )
                                       )
                                     0
                                     (make-pairs graphs rank-node-lst)
                                     )
  )

(define (parallel-special-graphs->bitmap-thunks graphs rank-node-lst #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
  (begin
    (define list-dot-files (for/list ([i (range 0 (length graphs))])
                             (format "~adot~s" SAVE-DIR i)
                             )
      )
    (special-graphs->dots graphs rank-node-lst)
    (parallel-dots->pngs list-dot-files cpu-cores)
    (pngs->bitmap-thunks 0 (length graphs))
    )
=======
  (define (parallel-graphs->bitmap-thunks graphs #:cpu-cores [cpu-cores (quotient (find-number-of-cores) 2)])
    (begin
      (define list-dot-files (for/list ([i (range 0 (length graphs))])
                               (format "~adot~s" SAVE-DIR i)
                               )
        )
      (graphs->dots graphs)
      (parallel-dots->pngs list-dot-files cpu-cores)
      (pngs->bitmap-thunks 0 (length graphs))
      )
    >>>>>>> 4e5b9ba0c0fb3bd90c20e918deff38ea8e8ed830
    )