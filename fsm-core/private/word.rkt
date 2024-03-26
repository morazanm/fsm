; FSM Library Version 1.0
; Copyright (C) 2015 by Marco T. Morazan and Rosario Antunez
; Written by: Marco T. Morazan and Rosario Antunez, 2015

;;; WORD

; A word is a (listof symbol).

; A word index (wi) is a natnum. -- Index of the next character to consume

(module word racket
  (provide get-symb-word get-subword word-consumed? empty-word? word-length generate-words)
  
  ;;; WORD
  
  (define MAX-WORD-LENGTH 10)
  
  ; word --> boolean
  (define empty-word? null?)
  
  ; word wi --> symbol
  (define (get-symb-word w i)
    (if (>= i (length w)) 
        (error (format "The word ~s does not have an ith, ~s, symbol." w i))
        (list-ref w i)))
  
  ; word wi --> word
  (define (get-subword w i)
    (if (>= i (length w)) 
        (error "The word ~s is too short and has no subword starting at position ~s." w i)
        (list-tail w i)))
  
  ; wi --> boolean
  (define (word-consumed? a-wi w) (= a-wi (length w)))
  
  (define word-length length)
  
  ; alphabet [natnum] --> word
  (define (generate-word sigma . l)
    (define alpha-size (length sigma))
    (define (generator len)
      (if (= len 0) 
          '()
          (cons (list-ref sigma (random alpha-size)) (generator (- len 1)))))
    (let ((len (if (null? l) (random MAX-WORD-LENGTH) (car l))))
      (generator len)))
  
  ; natnum alphabet (listof word) --> (listof word)
  (define (generate-words n sigma words)
    (define sigma-len (length sigma))
    (define (calculate-max-len i)
      (cond [(>= i 0) (+ (expt sigma-len i) (calculate-max-len (- i 1)))]
          [else 0]))
    (define max-len (calculate-max-len (- MAX-WORD-LENGTH 1)))
    (generate-words-limited n sigma words max-len))

  (define (generate-words-limited n sigma words max-len)
    (cond [(>= (length words) max-len) words]
          [(= n 0) words]
          [else (let ((w (generate-word sigma)))
                  (cond [(member w words) (generate-words-limited n sigma words max-len)]
                        [else (generate-words-limited (- n 1) sigma (cons w words) max-len)]))]))
  )

  