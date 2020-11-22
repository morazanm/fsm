#lang racket
(require 2htdp/image "globals.rkt")
#|
  This module handles the creation of the rule display at the bottom of the GUI
   To add another machine please add a case to the function create-text and a function to format the
     the rule.
  Created by joshua Schappel on 11/20/19
|#

(provide
 create-pda-rule
 create-dfa-ndfa-rule
 ruleFactory)


(define DISPLAY-HEIGHT 75)
(define DISPLAY-WIDTH 830)
(define DISPLAY (rectangle DISPLAY-WIDTH DISPLAY-HEIGHT "outline" "blue"))



;; ruleFactory: list symbol int rule -> image
;; Purpose: Creates the approperate rule Display based on the machine provided
(define (ruleFactory aList type scroll-index current-rule)
  (letrec (
           ;; List-2-image: list-of-rules image -> image
           ;; Purpose: converts the list or rules to an image that contains as many rules that can fit
           (list-2-img (lambda (lor img)
                         (letrec ((rule-number (get-num)) ;; The number of rules that can fit on the screen

                                  ;; convert-2-img: list-or-rules image int -> image
                                  ;; Purpose: a helper function that only renders the max rules that can fit on the screen
                                  (convert-2-img (lambda (lor img accum)
                                                  
                                                   (cond
                                                     [(empty? lor) img]
                                                     [(equal? accum 0) img]
                                                     [else (convert-2-img (cdr lor) (fit-img lor rule-number img) (sub1 accum))]))))
                           (convert-2-img lor img rule-number))))

           ;; get-num: Gets the number of rules that can be displayed on the screen
           (get-num (lambda ()
                      (case type
                        [(pda) PDA_NUMBER]
                        [(tm) TM_NUMBER]
                        [(tm-language-recognizer) TM_NUMBER]
                        [else DFA-NDFA_NUMBER])))
           
           ;; get-count list-of-rules int int -> int
           ;; Purpose: Determins the number of rules that can be added
           (get-count (lambda (lor accum count)
                        (cond
                          [(empty? lor) count]
                          [else (decide-size lor accum count)])))

           ;; decide-size list-of-rules image int -> int
           ;; Purpose: checks if another rule can be added if so the count is increased
           (decide-size (lambda (lor accum count)
                          (let((rule-img (beside (create-rule-image (car lor) BOX-PADDING #f)
                                                 accum)))
                            (cond
                              [(> (image-width rule-img) (image-width DISPLAY)) count]
                              [else (get-count (cdr lor) rule-img (add1 count))]))))
           

           ;; fit-img: list-of-rules int image -> image
           ;; Purpose: Formats the space between each rule to be even
           (fit-img (lambda (lor count img)
                      (let ((box-width (/ DISPLAY-WIDTH count)))
                        (cond
                          [(equal? 0 img) img]
                          [else (beside
                                 img
                                 (create-rule-image (car lor) box-width #t))]))))
                                 

           ;; create-rule-image: rule padding boolean -> image
           ;; Purpose: Places the rule in a box and determins if the rule should be hightlighted or not
           (create-rule-image (lambda (rule padding bool)
                                (letrec (
                                         ;; create-text: rule -> image
                                         ;; Purpose: creates the text image for a rule. If the rule is equal to the current rule then the
                                         ;;   the rule is highlighted in a different color
                                         (create-text (lambda (rule)
                                                        (letrec (;; determin-color: none -> color
                                                                 ;; Purpose: Determins the color if the rule 
                                                                 (determin-color (lambda ()
                                                                                   (cond
                                                                                     [(equal? current-rule rule) HIGHTLIGHT-RULE]
                                                                                     [else DEFAULT-RULE])))
                                                                 )
                                                          (match rule
                                                            [(list _ _ _) (text (create-dfa-ndfa-rule rule) FONT-SIZE (determin-color))]
                                                            [(list (list _ _ _) (list _ _)) (text (create-pda-rule rule) FONT-SIZE (determin-color))]
                                                            [(list (list _ _) (list _ _)) (text (create-tm-rule rule) FONT-SIZE (determin-color))]
                                                            [else (error "Invalid pattern match")]))))
                                         
                                         (txt (create-text rule)) ;; The textbox
                                         ;; get-width: none -> int
                                         ;; Purpose: determines how to calulate the width of the rule box based on wether
                                         ;;  the size is known
                                         (get-width (lambda ()
                                                      (cond
                                                        [bool padding]
                                                        [else (+ (image-width txt) padding)])))

                                         (scale-txt (lambda (image s)
                                                      (cond
                                                        [(> (image-width (scale s txt)) (image-width (rectangle (get-width) BOX-HEIGHT "outline" RULE-BOX-COLOR)))
                                                         (scale-txt image (- s .05))]
                                                        [else (scale s image)])))
                                         )
                                  (overlay
                                   (scale-txt txt 1)
                                   (rectangle (get-width) BOX-HEIGHT "outline" RULE-BOX-COLOR))))))
    (list-2-img aList empty-image)))


;; create-dfa-ndfa-rule: rule (tuple) -> string
;; Purpose: Converts a rule into an string
(define (create-dfa-ndfa-rule tup)
  (match tup
    [(list start input final)
     (string-append "(" (symbol->string start) " " (symbol->string input) " " (symbol->string final) ")")]
    [else (error "Invalid dfa/ndfa pattern match")]))


;; create-pda-rule: rule -> string
;; Purpose: Converts the rule to a string
(define (create-pda-rule rule)
  (match rule
    [(list (list from tape pop) (list to push))
     #:when (and (list? pop) (list? push)) ;; case: pop and push are lists
     (string-append
      "((" (symbol->string from) " " (symbol->string tape)
      " " (list->string pop) ") ("
      (symbol->string to) " "  (list->string push) "))")]
    
    [(list (list from tape pop) (list to push))
     #:when (list? pop) ;; case: just pop is a list
     (string-append
      "((" (symbol->string from) " " (symbol->string tape)
      " " (list->string pop) ") ("
      (symbol->string to) " "  (symbol->string push) "))")]
    
    [(list (list from tape pop) (list to push))
     #:when (list? push) ;; case: just push is a list
     (string-append
      "((" (symbol->string from) " " (symbol->string tape)
      " " (symbol->string pop) ") ("
      (symbol->string to) " "  (list->string push) "))")]
    
    [(list (list from tape pop) (list to push)) ;; case: pop and push are not lists
     (string-append
      "((" (symbol->string from) " " (symbol->string tape)
      " " (symbol->string pop) ") ("
      (symbol->string to) " "  (symbol->string push) "))")]
    
    ;; Otherwise throw an error
    [else
     (error "Invalid pda pattern match")]))


;; create-tm-rule rule -> string
;; Purpose: Converts the rule to a string
(define (create-tm-rule rule)
  (match rule
    [(list (list state1 alpha1) (list state2 alpha2))
     (string-append
      "((" (symbol->string state1) " " (symbol->string (format-tm-input alpha1)) ")"
      " (" (symbol->string state2) " " (symbol->string (format-tm-input alpha2)) "))")]
    [else
     (error "Invalid pattern match")]))


;; list->string: list-of-symbols -> String
;; Purpose: Creates a string containing every element in the list
(define (list->string los)
  (let ((string (string-append
                 "("
                 (foldr (lambda (val accum)
                          (string-append (symbol->string val) " " accum)) "" los))))
    (string-append
     (substring
      string
      0
      (- (string-length string) 1))
     ")")))

(define (format-tm-input input)
  (case input
    [(RIGHT) 'R]
    [(LM) '@]
    [(BLANK) '_]
    [else input]))
    
