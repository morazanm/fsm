#lang racket


#|
Created by Joshua Schappel on 12/19/19
  This file contains all the functions associated with a button
|#


(require net/sendurl "../structs/input.rkt" "../structs/world.rkt" "../structs/state.rkt"
         "../structs/machine.rkt" "../structs/posn.rkt" "../globals.rkt" "stateTransitions.rkt"
         "../structs/msgWindow.rkt" "../structs/world.rkt" "../../fsm-main.rkt")

(provide addState removeState addRule removeRule addStart replaceStart
         addEnd rmvEnd addAlpha rmvAlpha addSigma clearSigma addGamma
         rmvGamma getScrollBarPosition showNext showPrev scrollbarRight
         scrollbarLeft NULL-FUNCTION openHelp send-url)



;; ------- Button Functions -------


;; addState: world -> world
;; Purpose: Adds a state to the world
(define addState (lambda (w)
                   (let ((state (string-trim (textbox-text (car (world-input-list w)))))
                         (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100))))
                     (cond[(equal? "" state) w]
                          [(ormap (lambda (x) (equal? (format-input state) (symbol->string (fsm-state-name x))))
                                  (machine-state-list (world-fsm-machine w)))
                           w]
                          [else
                           (begin
                             (set-machine-state-list! (world-fsm-machine w) (cons (fsm-state (format-input (string->symbol state))
                                                                                             TRUE-FUNCTION
                                                                                             (posn 0 0))
                                                                                  (machine-state-list (world-fsm-machine w))))
                             (reset-bottom-indices)
                             (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                    null (world-button-list w) new-input-list '()
                                    '() (world-error-msg w) (world-scroll-bar-index w)))]))))


;; removeState: world -> world
;; Purpose: Removes a state from the world
(define removeState (lambda(w)
                      (letrec ((state (string-trim (textbox-text (car (world-input-list w)))))
                               (new-input-list (list-set (world-input-list w) 0 (remove-text (car (world-input-list w)) 100)))

                               ;; remove-all: list-of-rules -> list-of-rules
                               ;; Purpose: Removes all rules from the machine that contain the current rule being removed
                               (remove-all (lambda (lor)
                                             (filter (lambda (x) (cond
                                                                   [(equal? (symbol->string (car x)) state) #f]
                                                                   [(equal?  (symbol->string (caddr x)) state) #f]
                                                                   [else #t]))
                                                     lor))))
                        
                        (if (equal? (string->symbol state) (world-cur-state w))
                            (begin
                              (set-machine-state-list! (world-fsm-machine w) (filter(lambda(x) (not(equal? (fsm-state-name x) (string->symbol state)))) (machine-state-list (world-fsm-machine w))))
                              (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w))))
                              (reset-bottom-indices)
                              (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                     null (world-button-list w) new-input-list
                                     '() '() (world-error-msg w) (world-scroll-bar-index w)))

                            (begin
                              (set-machine-state-list! (world-fsm-machine w) (filter (lambda(x) (not(equal? (fsm-state-name x) (string->symbol state)))) (machine-state-list (world-fsm-machine w))))
                              (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w))))
                              (reset-bottom-indices)
                              (create-new-world-input-empty w new-input-list))))))


;; addRule: world -> world
;; Purpose: Addes a rule to the world rule list
(define addRule (lambda (w)
                  (letrec ((input-list (world-input-list w))
                           (r1 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 4)))))
                           (r2 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 5)))))
                           (r3 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 6)))))

                           ;; add-pda: NONE -> world
                           ;; Addds a pda rule to the world if all imputs are valid
                           (add-pda (lambda ()
                                      (let ((r4 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 9)))))
                                            (r5 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 10)))))
                                            (new-input-list (list-set
                                                             (list-set
                                                              (list-set
                                                               (list-set
                                                                (list-set (world-input-list w) 10 (remove-text (list-ref (world-input-list w) 10) 100))
                                                                9 (remove-text (list-ref (world-input-list w) 9) 100))
                                                               6 (remove-text (list-ref (world-input-list w) 6) 100))
                                                              5 (remove-text (list-ref (world-input-list w) 5) 100))
                                                             4 (remove-text (list-ref (world-input-list w) 4) 100))))
                                        
                                        (cond
                                          [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||) (equal? r4 '||) (equal? r5 '||)) (redraw-world w)]
                                          [else
                                           (begin
                                             (reset-bottom-indices)
                                             (set-machine-rule-list! (world-fsm-machine w) (cons (list (list r1 (format-input r2) r3) (list r4 r5)) (machine-rule-list (world-fsm-machine w))))
                                             (create-new-world-input-empty w new-input-list))]))))

                           ;; add-dfa: NONE -> world
                           ;; Adds a dfa/ndfa rule to the world if all the inputs are valid
                           (add-dfa (lambda ()
                                      (let ((new-input-list (list-set (list-set (list-set (world-input-list w) 6 (remove-text (list-ref (world-input-list w) 6) 100)) 5 (remove-text (list-ref (world-input-list w) 5) 100)) 4 (remove-text (list-ref (world-input-list w) 4) 100))))
                                        (cond
                                          [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||)) (redraw-world w)]
                                          [else
                                           (begin
                                             (reset-bottom-indices)
                                             (set-machine-rule-list! (world-fsm-machine w) (cons (list  r1 (format-input r2) r3) (machine-rule-list (world-fsm-machine w))))
                                             (create-new-world-input-empty w new-input-list))])))))                           
                    (cond
                      [(equal? MACHINE-TYPE 'pda) (add-pda)]
                      [else (add-dfa)]))))


;; removeRule: world -> world
;; Purpose: Removes a world from the world list
(define removeRule (lambda (w)
                     (letrec ((input-list (world-input-list w))
                              (r1 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 4)))))
                              (r2 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 5)))))
                              (r3 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 6)))))

                              ;; rmv-dfa: NONE -> world
                              ;; Purpose: Removes a dfa/ndfa rule from the world as long as all input fields are filled in
                              (rmv-dfa (lambda ()
                                         (let ((new-input-list (list-set (list-set (list-set (world-input-list w) 6 (remove-text (list-ref (world-input-list w) 6) 100)) 5 (remove-text (list-ref (world-input-list w) 5) 100)) 4 (remove-text (list-ref (world-input-list w) 4) 100))))
                                           (cond
                                             [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||)) (redraw-world w)]
                                             [else
                                              (begin
                                                (reset-bottom-indices)
                                                (set-machine-rule-list! (world-fsm-machine w) (remove (list r1 (format-input r2) r3) (machine-rule-list (world-fsm-machine w))))
                                                (create-new-world-input-empty w new-input-list))]))))

                              ;; rmv-pda: NONE -> world
                              ;; Purpose: Removes a pda rule from the world as long as all input fields are filled in
                              (rmv-pda (lambda ()
                                         (let ((r4 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 9)))))
                                               (r5 (string->symbol (string-trim (textbox-text (list-ref (world-input-list w) 10)))))
                                               (new-input-list (list-set
                                                                (list-set
                                                                 (list-set
                                                                  (list-set
                                                                   (list-set (world-input-list w) 10 (remove-text (list-ref (world-input-list w) 10) 100))
                                                                   9 (remove-text (list-ref (world-input-list w) 9) 100))
                                                                  6 (remove-text (list-ref (world-input-list w) 6) 100))
                                                                 5 (remove-text (list-ref (world-input-list w) 5) 100))
                                                                4 (remove-text (list-ref (world-input-list w) 4) 100))))
                                           (cond
                                             [(or (equal? r1 '||) (equal? r2 '||) (equal? r3 '||) (equal? r4 '||) (equal? r5 '||)) (redraw-world w)]
                                             [else
                                              (begin
                                                (reset-bottom-indices)
                                                (set-machine-rule-list! (world-fsm-machine w) (remove (list (list r1 (format-input r2) r3) (list r4 r5)) (machine-rule-list (world-fsm-machine w))))
                                                (create-new-world-input-empty w new-input-list))])))))
                       (cond
                         [(equal? MACHINE-TYPE 'pda) (rmv-pda)]
                         [else (rmv-dfa)]))))


;; addState: world -> world
;; Purpose: Adds a start state to the world
(define addStart (lambda(w)
                   (let
                       ((start-state (string-trim(textbox-text(list-ref (world-input-list w) 2))))
                        (new-input-list (list-set (world-input-list w) 2 (remove-text (list-ref(world-input-list w) 2) 100))))
                    
                     (cond
                       [(equal? "" start-state) (redraw-world w)]
                       [(and (null? (machine-start-state (world-fsm-machine w))) (ormap (lambda(x) (equal? start-state (symbol->string (fsm-state-name x)))) (machine-state-list (world-fsm-machine w))))
                        (begin
                          (reset-bottom-indices)
                          (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                          (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                 null (world-button-list w) new-input-list
                                 '() '() (world-error-msg w) (world-scroll-bar-index w)))]
                       [ (null? (machine-start-state (world-fsm-machine w)))
                         (begin
                           (reset-bottom-indices)
                           (set-machine-state-list! (world-fsm-machine w) (cons (fsm-state (string->symbol start-state) TRUE-FUNCTION (posn 0 0)) (machine-state-list (world-fsm-machine w))))
                           (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                           (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                  null (world-button-list w) new-input-list
                                  '() '() (world-error-msg w) (world-scroll-bar-index w)))]
                       [ (ormap (lambda (x) (equal? start-state (symbol->string (fsm-state-name x)))) (machine-state-list (world-fsm-machine w)))
                         (begin
                           (reset-bottom-indices)
                           (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                           (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                  null (world-button-list w) new-input-list
                                  '() '() (world-error-msg w) (world-scroll-bar-index w)))]
                       [else w]))))

;; replaceStart: world -> world
;; Purpose: Replaces the start state in the world
(define replaceStart (lambda(w)
                       (let
                           ((start-state (string-trim(textbox-text(list-ref (world-input-list w) 2))))
                            (new-input-list (list-set (world-input-list w) 2 (remove-text (list-ref (world-input-list w) 2) 100))))
                         (cond
                           [(equal? "" start-state) (redraw-world w)]
                           
                           [ (ormap (lambda (x) (equal? (string->symbol start-state)(fsm-state-name x))) (machine-state-list (world-fsm-machine w)))
                             (begin
                               (reset-bottom-indices)
                               (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                               (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w)
                                      null (world-button-list w) new-input-list
                                      '() '() (world-error-msg w) (world-scroll-bar-index w)))]
                           
                           [else
                            (begin
                              (reset-bottom-indices)
                              (set-machine-state-list! (world-fsm-machine w) (cons (fsm-state (string->symbol start-state) TRUE-FUNCTION (posn 0 0))  (machine-state-list (world-fsm-machine w))))
                              (set-machine-start-state! (world-fsm-machine w) (string->symbol start-state))
                              (world (world-fsm-machine w)(world-tape-position w) (world-cur-rule w)
                                     null (world-button-list w) new-input-list
                                     '() '() (world-error-msg w) (world-scroll-bar-index w)))]))))

;; addEnd: world -> world
;; Purpose: Adds an end state to the world
(define addEnd (lambda(w)
                 (let
                     ((end-state (string-trim (textbox-text(list-ref (world-input-list w) 3))))
                      (new-input-list (list-set (world-input-list w) 3 (remove-text (list-ref (world-input-list w) 3) 100))))
                   (cond
                     [(equal? "" end-state) (redraw-world w)]
                     [(ormap (lambda (x) (equal? (fsm-state-name x) (string->symbol end-state))) (machine-state-list (world-fsm-machine w)))
                      (begin
                        (reset-bottom-indices)
                        (set-machine-final-state-list! (world-fsm-machine w) (remove-duplicates (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w)))))
                        (create-new-world-input-empty w new-input-list))]
                     [else
                      (begin
                        (reset-bottom-indices)
                        (set-machine-state-list! (world-fsm-machine w) (cons (fsm-state (string->symbol end-state) TRUE-FUNCTION (posn 0 0)) (machine-state-list (world-fsm-machine w))))
                        (set-machine-final-state-list! (world-fsm-machine w) (remove-duplicates (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w)))))
                        (create-new-world-input-empty w new-input-list))]))))
                

;; rmvEnd: world -> world
;; Purpose: removes a end state from the world-final-state-list
(define rmvEnd (lambda (w)
                 (let
                     ((end-state (string-trim(textbox-text(list-ref (world-input-list w) 3))))
                      (new-input-list (list-set (world-input-list w) 3 (remove-text (list-ref (world-input-list w) 3) 100))))
                   (cond
                     [(equal? "" end-state) (redraw-world w)]
                     [(ormap (lambda(x) (equal? (fsm-state-name x) (string->symbol end-state))) (machine-state-list (world-fsm-machine w)))
                      (begin
                        (reset-bottom-indices)
                        (set-machine-final-state-list! (world-fsm-machine w) (remove (string->symbol end-state) (machine-final-state-list (world-fsm-machine w))))
                        (create-new-world-input-empty w new-input-list))]
                     [else
                      (begin
                        (reset-bottom-indices)
                        (set-machine-final-state-list! (world-fsm-machine w) (cons (string->symbol end-state) (machine-final-state-list (world-fsm-machine w))))
                        (create-new-world-input-empty w new-input-list))]))))
                      

;; addAlpha: world -> world
;; Purpose: Adds a letter to the worlds alpha-list
(define addAlpha (lambda (w)
                   (let ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 1))))
                         (new-input-list (list-set (world-input-list w) 1 (remove-text (list-ref (world-input-list w) 1) 100))))

                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [(equal? 14 (add1 (length (machine-alpha-list (world-fsm-machine w)))))
                        (redraw-world-with-msg w "You have reached the maximum amount of characters for the alphabet" "Error" MSG-CAUTION)]
                       [else
                        (begin
                          (reset-bottom-indices)
                          (set-machine-alpha-list! (world-fsm-machine w) (sort (remove-duplicates (cons (string->symbol input-value) (machine-alpha-list (world-fsm-machine w)))) symbol<?))
                          (create-new-world-input-empty w new-input-list))]))))

;; rmvAlpha: world -> world
;; Purpose: Removes a letter from the worlds alpha-list
(define rmvAlpha (lambda (w)
                   (let ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 1))))
                         (new-input-list (list-set (world-input-list w) 1 (remove-text (list-ref (world-input-list w) 1) 100)))

                         ;; remove-all: list-of-rules symbol -> list-of-rules
                         ;; Purpose: Removes all rules that are associated with the alpha that is being removed.
                         (remove-all (lambda (lor alpha)
                                       (filter (lambda (x) (cond
                                                             [(equal? (symbol->string (caadr x)) alpha) #f]
                                                             [else #t]))
                                               lor))))
                     
                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [else
                        (begin
                          (reset-bottom-indices)
                          (set-machine-alpha-list! (world-fsm-machine w) (sort (remove (string->symbol input-value) (machine-alpha-list (world-fsm-machine w))) symbol<?))
                          (set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w)) input-value))
                          (create-new-world-input-empty w new-input-list))]))))
                   

;; addSigma: world -> world
;; Purpose: adds a letter or group of letters to the sigma list
(define addSigma (lambda (w)
                   (letrec ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 7))))

                            ;; real-string->list: string -> list-of-symbols
                            ;; Purpose: converts a string to a list. Unlike Racket's string->list, this function converts every element of the
                            ;; list to a string as opposed to a char.
                            (real-string->list (lambda (str)
                                                 (letrec (;; convert-to-list: string list -> list-of-symbols
                                                          ;; Purpose: this function uses an accumulator to accumulate all elements of the string converted to a list  
                                                          (convert-to-list (lambda (str accum) 
                                                                             (cond
                                                                               [(< (string-length str) 1) accum]
                                                                               [(equal? (substring str 0 1) " ") (convert-to-list (substring str 1) accum)]
                                                                               [else (convert-to-list (substring str 1) (cons (string->symbol (substring str 0 1)) accum))]))))
                                                   (convert-to-list str '()))))

                            ;; check-alpha: list-of-alpha list-of-sigma -> boolean
                            ;; Purpose: Determins if all elements of sigma are in alpha. If they are then returns true, otherwise retunrs false
                            (check-alpha (lambda (loa los)
                                           (letrec (;; check-lists: list list -> boolean
                                                    ;; Purpose: given two list will check to see if the elements of list2 are in list1
                                                    (check-lists (lambda (list1 list2)
                                                                   (cond
                                                                     [(empty? list2) #t]
                                                                     [(equal? (member (car list2) list1) #f) #f]
                                                                     [else (check-lists list1 (cdr list2))]))))
                                             (cond
                                               [(empty? loa) #f]
                                               [(empty? los) #f]
                                               [else (check-lists loa los)]))))
                            (new-input-list (list-set (world-input-list w) 7 (remove-text (list-ref (world-input-list w) 7) 100))) 
                            (sigma-list (reverse (real-string->list input-value))))
                     (cond
                       [(equal? (check-alpha (machine-alpha-list (world-fsm-machine w)) sigma-list) #f) (redraw-world w)]
                       [(equal? input-value "") (redraw-world w)]
                       [(> (string-length input-value) 0)

                        ;; Check if the unprocessed list and processed lists are empty... If they are we know run code was never pressed
                        ;; If they are not empty then run code was pressed. If run code was pressed then we know to update the gui to handle to new sigma
                        ;; so we will call run code from here.
                        (cond
                          [(empty? (and (world-unporcessed-config-list w) (world-processed-config-list w)))
                           (begin
                             (reset-bottom-indices)
                             (set-machine-sigma-list! (world-fsm-machine w) (append sigma-list (machine-sigma-list (world-fsm-machine w))))
                             (create-new-world-input-empty w new-input-list))]
                          [else
                           (begin
                             (reset-bottom-indices)
                             (set-machine-sigma-list! (world-fsm-machine w) (append sigma-list (machine-sigma-list (world-fsm-machine w))))
                             (create-new-world-input-empty w new-input-list))])]
                       [else (redraw-world w)]))))


;; clearSigma: world -> world
;; Purpose: Removes all elements of the sigma list
(define clearSigma (lambda (w)
                     (let ((new-input-list (list-set (world-input-list w) 7 (remove-text (list-ref (world-input-list w) 7) 100))))
                       (begin
                         (reset-bottom-indices)
                         (set-machine-sigma-list! (world-fsm-machine w) '())
                         (create-new-world-input-empty w new-input-list)))))


;; addGamma: world -> world
;; Purpose: Adds a value to the machines gamma list (only needed for pda)
(define addGamma (lambda (w)
                   (let (
                         (input-value (string-trim (textbox-text(list-ref (world-input-list w) 8))))
                         (new-input-list (list-set (world-input-list w) 8 (remove-text (list-ref (world-input-list w) 8) 100))))
                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [(equal? 14 (add1 (length (pda-machine-stack-alpha-list (world-fsm-machine w)))))
                        (redraw-world-with-msg w "You have reached the maximum amount of characters allowed" "Warning" MSG-CAUTION)]
                       [else
                        (begin
                          (reset-bottom-indices)
                          (set-pda-machine-stack-alpha-list! (world-fsm-machine w) (sort (remove-duplicates (cons (string->symbol input-value) (pda-machine-stack-alpha-list (world-fsm-machine w)))) symbol<?))
                          (create-new-world-input-empty w new-input-list))]))))


;; rmvGamma world -> world
;; Purpose: Removes a value from the machines gamma list (only needed for pda)
(define rmvGamma (lambda (w)
                   (let ((input-value (string-trim (textbox-text(list-ref (world-input-list w) 8))))
                         (new-input-list (list-set (world-input-list w) 8 (remove-text (list-ref (world-input-list w) 8) 100)))

                         ;; remove-all: list-of-rules symbol -> list-of-rules
                         ;; Purpose: Removes all rules that are associated with the alpha that is being removed.
                         (remove-all (lambda (lor alpha)
                                       (filter (lambda (x) (cond
                                                             [(equal? (symbol->string (cadr x)) alpha) #f]
                                                             [else #t]))
                                               lor))))
                     
                     (cond
                       [(equal? input-value "") (redraw-world w)]
                       [else
                        (begin
                          (reset-bottom-indices)
                          (set-pda-machine-stack-alpha-list! (world-fsm-machine w) (sort (remove (string->symbol input-value) (pda-machine-stack-alpha-list (world-fsm-machine w))) symbol<?))
                          ;;(set-machine-rule-list! (world-fsm-machine w) (remove-all (machine-rule-list (world-fsm-machine w)) input-value))
                          (create-new-world-input-empty w new-input-list))]))))





;; getScrollBarPosition: list-of-rules rule -> int
;; Purpose: Trys to place the currently highlighted rule at the beginning of the scrollbar. If not possiable moves to scrollbar to a position
;;      where the rule will be sceen by the user.
(define getScrollBarPosition (lambda (lor rule)
                               (let ((ruleIndex (index-of lor rule))
                                     (rule-num (determine-rule-number MACHINE-TYPE)))
                                 
                                 (cond
                                   ;; See if there is no current rule. If so return the starting index of the scrollbar
                                   [(equal? rule '(empty empty empty)) 0]
                                   ;; If true then we set the scroll index to max
                                   [(> (+ ruleIndex rule-num) (- (length lor) 1))
                                    (let* ((i (- (length lor) rule-num)))
                                      (if (< i 0) 0
                                          i))]
                                   ;; Otherwise return the rule index
                                   [else ruleIndex]))))

                  

;; showNext: world -> world
;; Purpose: shows the next state that the machine is in
(define showNext(lambda(w)
                  ;; Check if sigma list is empty
                  (cond
                    [(empty? (machine-sigma-list (world-fsm-machine w))) (redraw-world-with-msg w "Your Tape is currently empty! Please add variables to the Tap to continue." "Notice" MSG-CAUTION)]
                    [else
                     ;; Check if the unprocessed list is empty. If so then run was not yet pressed
                     (cond
                       [(empty? (world-unporcessed-config-list w)) (redraw-world-with-msg w "You must build your machine before you can continue. Please press 'Run' to proceed." "Error" MSG-CAUTION)]
                       [else
                        (letrec(
                                (nextState (car (world-unporcessed-config-list w))) ;; The next state the machine transitions to
                                ;; Determins the the next transition based on machine type
                                (determine-next-steps (lambda ()
                                                        (cond
                                                          [(eq? nextState 'accept)
                                                           (redraw-world-with-msg w "The input is accepted." "Success" MSG-SUCCESS)]
                                                          [(eq? nextState 'reject)
                                                           (redraw-world-with-msg w "The input is rejected." "Notice" MSG-CAUTION)]
                                                          [else
                                                           (case MACHINE-TYPE
                                                             [(pda) (go-next nextState w)]
                                                             [(tm) (println "TODO")]
                                                             [else
                                                              (go-next nextState w)])])))
                                
                                ;; update-Machine: None -> world
                                ;; Updates the world based on the given machine
                                (update-world (lambda ()
                                                (case MACHINE-TYPE
                                                  [(pda) (go-next nextState w)]
                                                  [(tm) (println "TODO")]
                                                  [else
                                                   (go-next nextState w)])))
                                )
                          (determine-next-steps))])])))



                          
;; go-next: symbol world -> world
;; Determins the next state that the machine needs to be in an then updates the world accordingly
(define (go-next nextState w)
  ;;(println (format "The nextState is ~s" (car nextState)))
  (letrec ((transitions (cdr (world-unporcessed-config-list w))) ;; The list-of-transitons

           (determin-cur-state (lambda ()
                                 (case MACHINE-TYPE
                                                  [(pda) (car nextState)]
                                                  [(tm) (println "TODO")]
                                                  [else (car (cdr nextState))])))

           )
    (println nextState)
    (cond
      [(eq? nextState 'accept)
       (redraw-world-with-msg w "The input is accepted." "Success" MSG-SUCCESS)]
      [(eq? nextState 'reject)
       (redraw-world-with-msg w "The input is rejected." "Notice" MSG-CAUTION)]
      [else
       (let* ((cur-rule (getCurRule (append (list nextState) (world-processed-config-list w)))))
         (if (equal? EMP (cadr cur-rule)) TAPE-INDEX-BOTTOM (set-tape-index-bottom (+ 1 TAPE-INDEX-BOTTOM)))                  
         (world (world-fsm-machine w) (world-tape-position w) (getCurRule (append (list nextState) (world-processed-config-list w)))
                (determin-cur-state) (world-button-list w) (world-input-list w)
                (append (list nextState) (world-processed-config-list w)) transitions (world-error-msg w)
                (getScrollBarPosition (reverse (machine-rule-list (world-fsm-machine w))) cur-rule)))])))


;; showPrev: world -> world
;; shows the previous state that the machine was in
(define showPrev (lambda(w)
                   (cond
                     [(empty? (world-processed-config-list w)) (redraw-world-with-msg w "The tape is currently empty. Please add variables to the tape, then press 'Run'" "Notice" MSG-CAUTION)]
                     [(empty? (cdr (world-processed-config-list w))) (redraw-world-with-msg w "You have reached the beginning of the machine! There are no more previous states." "Notice" MSG-CAUTION)]
                     [else
                      (let* (
                             (previousState (car (cdr (world-processed-config-list w))))
                             (cur-rule (getCurRule (cdr (world-processed-config-list w)))))
                        (if (equal? EMP (cadr cur-rule))
                            (set-tape-index-bottom TAPE-INDEX-BOTTOM)
                            (set-tape-index-bottom (- TAPE-INDEX-BOTTOM 1)))
                        
                        (world (world-fsm-machine w) (world-tape-position w) (getCurRule (cdr (world-processed-config-list w)))
                               (car (cdr previousState)) (world-button-list w) (world-input-list w)
                               (cdr (world-processed-config-list w)) (cons (car (world-processed-config-list w)) (world-unporcessed-config-list w)) (world-error-msg w)
                               (getScrollBarPosition (reverse (machine-rule-list (world-fsm-machine w))) cur-rule)))])))


;; scrollbarRight: world -> world
;; Purpose: moves the scroll bar over 1 place to the right
(define scrollbarRight (lambda (w)
                         (let ((index (world-scroll-bar-index w))
                               (rule-num (determine-rule-number MACHINE-TYPE)))
                           (cond
                             [(< (length (machine-rule-list (world-fsm-machine w))) rule-num) (redraw-world w)]
                             [(< (length (list-tail (machine-rule-list (world-fsm-machine w)) (add1 index))) rule-num) (redraw-world w)]
                             [else

                              (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w) (world-cur-state w) (world-button-list w)
                                     (world-input-list w) (world-processed-config-list w)(world-unporcessed-config-list w) (world-error-msg w) (add1 index))]))))

;; scrollbarLeft: world -> world
;; Purpose: moves the scroll bar over 1 place to the left
(define scrollbarLeft (lambda (w)
                        (let ((index (world-scroll-bar-index w)))
                          (cond
                            [(< (- index 1) 0)(redraw-world w)]
                            [else

                             (world (world-fsm-machine w) (world-tape-position w) (world-cur-rule w) (world-cur-state w) (world-button-list w)
                                    (world-input-list w) (world-processed-config-list w)(world-unporcessed-config-list w) (world-error-msg w) (sub1 index))]))))

;; oppenHelp; world -> world
;; Purpose: opens the help link in an external browser window
(define openHelp (lambda (w)
                   (send-url "https://github.com/jschappel/FSM-Visualization/blob/master/help.md" #t)
                   (redraw-world w)))



;; ------- Helper Functions -------

;; determine-rule-number: symbol -> int
;; Purpose: Determins the number of rules that can be displayed on the bottom
(define (determine-rule-number type)
  (case type
    [(pda) PDA_NUMBER]
    [(tm) (println "TODO")]
    [else DFA-NDFA_NUMBER]))


#|
;; getCurRule: processed-list -> rule
;; Purpose: get the rule that the machine just executed
(define getCurRule (lambda (pl)
                     
                     (cond
                       [(< (length pl) 2) (list 'empty 'empty 'empty)] ;; If the processed list doesn't have at least 2 items in it then no rule was followed...
                       [(= (length (caar pl)) (length (caadr pl)))
                        (list
                         (cadadr pl)
                         EMP
                         (cadar pl))]
                       [else
                        (list
                         (cadadr pl)
                         (caaadr pl)
                         (cadar pl))])))
|#

;; format-input: symbol -> symbol
;; Purpose: This is a helper function for addRule and removeRule that formats certine symbols into valid fsm symbols
;; EX: 'DEAD will become 'ds
(define format-input (lambda (s)
                       (case s
                         [(DEAD) 'ds]
                         [(EMP) 'e]
                         [else s])))


;; reset-bottom-indices: none -> none
;; Purpose: Resest the bottom indicies to there origional value
(define (reset-bottom-indices)
  (set-tape-index-bottom -1)
  (set-init-index-bottom 0))


;; THIS FUNCTION IS JUST A PLACEHOLDER
(define NULL-FUNCTION (lambda (w)
                        (redraw-world w)))