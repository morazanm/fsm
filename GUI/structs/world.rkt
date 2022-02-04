#lang racket
(require
  (for-syntax syntax/parse)
  "./machine.rkt"
  "./posn.rkt"
  "./state.rkt"
  "../globals.rkt")

(provide
 world%)

;; small macro to make debugging life easier :-)
(define-syntax (line-display! stx)
  (syntax-parse stx
    [(_ [value:id ...] dest-port)
     #`(begin
         (begin
           (display (quote value) dest-port)
           (display ": " dest-port)
           (display value dest-port)
           (display "\n" dest-port))...)]))

(define world%
  (class* object% (writable<%>)
    (init-field machine
                [tape-position -1]
                [mode 'idle] ;; Valid modes are: idle, active
                [view-mode 'control]
                [has-gviz (find-executable-path	"dot")]
                [type (machine-type machine)]
                [cur-rule CURRENT-RULE]
                [cur-state CURRENT-STATE]
                [processed-config-list '()]
                [unprocessed-config-list '()]
                [stack '()]
                [scroll-bar-index -1]) ;;TODO(jschappel): This will end up being used to tm, otherwise remove

    
    ;; setViewMode :: 'control | 'graphviz -> ()
    (define/public (setViewMode mode)
      (set! view-mode mode))

    ;; get-machine-alpha-list :: listOfSymbol
    (define/public (get-machine-alpha-list)
      (machine-alpha-list machine))

    ;; addAlpha :: symbol -> ()
    (define/public (addAlpha value)
      (set-machine-alpha-list! machine
                               (sort (remove-duplicates (cons value (machine-alpha-list machine)))
                                     symbol<?)))

    ;; addAlpha :: symbol -> ()
    (define/public (removeAlpha value)
      (set-machine-alpha-list! machine
                               (filter (lambda (v) (not (eq? v value)))
                                       (machine-alpha-list machine))))

    ;; addState :: symbol -> bool
    ;; returns true if we need to re-render
    (define/public (addState value)
      (define isDuplicate (isInStateList value))
      (unless isDuplicate
        (set-machine-state-list!
         machine
         (cons (fsm-state value (true-function) (posn 0 0))
               (machine-state-list machine))))
      (not isDuplicate))

    ;; removeState :: symbol -> bool
    ;; returns true if we need to re-render
    (define/public (removeState value)
      (define exists (isInStateList value))
      (when exists
        (set-machine-state-list!
         machine
         (remove
          value
          (machine-state-list machine)
          (lambda (target v2) (eq? target (fsm-state-name v2))))))
      exists)

    ;; addStart :: symbol -> ()
    ;; returns true if we need to re-render
    (define/public (addStart value)
      (addState value) ;; Update the state list
      (set-machine-start-state! machine value)) ;; Update the start state

    ;; removeStart :: symbol -> bool
    ;; returns true if we need to re-render
    (define/public (removeStart value)
      (define exists (removeState value))
      (when exists
        (set-machine-start-state! machine '()))
      exists)

    ;; addEnd :: symbol -> bool
    ;; returns true if we need to re-render
    (define/public (addEnd value)
      (define exists (member value (machine-final-state-list machine) eq?))
      (unless exists
        (addState value)
        (set-machine-final-state-list!
         machine
         (cons value (machine-final-state-list machine))))
      (not exists))

    ;; removeEnd :: symbol -> bool
    ;; returns true if we need to re-render
    (define/public (removeEnd value)
      (define exists (member value (machine-final-state-list machine) eq?))
      (when exists
        (set-machine-final-state-list!
         machine
         (filter (lambda (v) (not (eq? value v)))
                 (machine-final-state-list machine))))
      exists)

    ;; addRule :: rule -> bool
    ;; returns true if we need to re-render
    (define/public (addRule rule)
      (define exists (member rule (machine-rule-list machine) eqRule?))
      (unless exists
        (set-machine-rule-list!
         machine
         (cons rule (machine-rule-list machine))))
      exists)

    ;; addRule :: rule -> bool
    ;; returns true if we need to re-render
    (define/public (removeRule rule)
      (define exists (member rule (machine-rule-list machine) eqRule?))
      (when exists
        (set-machine-rule-list!
         machine
         (remove
          rule
          (machine-rule-list machine)
          eqRule?)))
      exists)

    ;; addTape :: listOfSymbol -> bool
    ;; To be a valid tape value it must exist in the alphabet, if not return false 
    (define/public (addTape values)
      (define allValid (empty? (filter (lambda (v)
                                         (not (member v (machine-alpha-list machine) eq?)))
                                       values)))
      (when allValid                     
        (set-machine-sigma-list!
         machine
         (append (machine-sigma-list machine) values)))
      allValid)

    ;; clearTape :: listOfSymbol -> ()
    (define/public (clearTape)
      (set-machine-sigma-list!
       machine
       '()))

    ;; add1TapePosition :: :: number -> ()
    (define/public (add1TapePosition)
      (set! tape-position (add1 tape-position))
      tape-position)

    ;; sub1TapePosition :: :: number -> ()
    (define/public (sub1TapePosition)
      (set! tape-position (sub1 tape-position))
      tape-position)

    ;; setCurRule :: number -> () 
    (define/public (setCurRule rule)
      (set! cur-rule rule))

    ;; setCurState :: number -> () 
    (define/public (setCurState state)
      (set! cur-state state))

    ;; setProcessedConfigList listOfTrans -> ()
    (define/public (setProcessedConfigList l)
      (set! processed-config-list l))

    ;; setUnprocessedConfigList listOfTrans -> ()
    (define/public (setUnprocessedConfigList l)
      (set! unprocessed-config-list l))

    ;; setMachine :: machine -> ()
    (define/public (setMachine m)
      (set! machine m))

    ;; setUnprocessedList :: listOfTransitions -> ()
    (define/public (setUnprocessedList l)
      (set! unprocessed-config-list l))

    ;; setProcessedList :: listOfTransitions -> ()
    (define/public (setProcessedList l)
      (set! processed-config-list l))

    ;; setScrollBarIndex :: number -> ()
    (define/public (setScrollBarIndex i)
      (set! scroll-bar-index i))

    ;; setMode :: symbol('idle | 'active) -> ()
    (define/public (setMode value)
      (set! mode value))

    ;; setWorldStack :: list -> ()
    (define/public (setWorldStack new-stack)
      (set! stack new-stack))

    ;; reset :: ()
    ;; reset the world values
    (define/public (reset)
      (set! tape-position -1)
      (set! cur-rule CURRENT-RULE)
      (set! cur-state CURRENT-STATE)
      (set! processed-config-list '())
      (set! unprocessed-config-list '())
      (set! stack '())
      (set! scroll-bar-index -1))

    ;; Needed for inherited interface
    (define/public (custom-display dest-port)
      (line-display! [tape-position
                      machine type
                      mode
                      cur-rule
                      cur-state
                      processed-config-list
                      unprocessed-config-list
                      stack
                      scroll-bar-index]
                     dest-port))

    ;; Needed for inherited interface
    (define/public (custom-write dest-port)
      (write "Currently Unsupported" dest-port))

    ;; isInStateList :: symbol -> bool
    (define/private (isInStateList value)
      (not (eq? #f
                (member value
                        (machine-state-list machine)
                        (lambda (target v2) (eq? target (fsm-state-name v2)))))))

    (define/match (eqRule? target actual)
      [(`(,s1 ,r1 ,f1) `(,s2 ,r2 ,f2)) ;; dfa/ndfa 
       (and (eq? s1 s2)
            (eq? r1 r2)
            (eq? f1 f2))])

    ;; true-function :: lambda
    (define/private (true-function)
      (match type
        [(or 'dfa 'ndfa) TRUE-FUNCTION]
        ['pda PDA-TRUE-FUNCTION]
        [_ TM-TRUE-FUNCTION]))
    
    (super-new)))