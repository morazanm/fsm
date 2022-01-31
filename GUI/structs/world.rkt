#lang racket
(require
  (for-syntax syntax/parse)
  "./machine.rkt"
  "./posn.rkt"
  "./state.rkt"
  "../globals.rkt")

(provide
 world%)

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
                [tape-position 0]
                [mode 'idle] ;; Valid modes are: idle, active
                [type (machine-type machine)]
                [cur-rule CURRENT-RULE]
                [cur-state CURRENT-STATE]
                [processed-config-list '()]
                [unprocessed-config-list '()]
                [scroll-bar-index 0])
   
    
    (define/public (custom-display dest-port)
      (line-display! [tape-position
                     machine type
                     mode
                     cur-rule
                     cur-state
                     processed-config-list
                     unprocessed-config-list
                     scroll-bar-index]
                    dest-port))

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

    ;; setMachine :: machine -> ()
    (define/public (setMachine m)
      (set! machine m))

    ;; setUnprocessedList :: listOfTransitions -> ()
    (define/public (setUnprocessedList l)
      (set! unprocessed-config-list l))

    ;; setProcessedList :: listOfTransitions -> ()
    (define/public (setProcessedList l)
      (set! processed-config-list l))

    ;; setMode :: symbol('idle | 'active) -> ()
    (define/public (setMode value)
      (set! mode value))
    
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