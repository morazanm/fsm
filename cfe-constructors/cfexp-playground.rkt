#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt")

(define EMPTY (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

;; w = ww^r
(define WWR 
  (local [(define WWR (var-cfexp 'WWR))

          (define AHA (concat-cfexp A WWR A))

          (define BHB (concat-cfexp B WWR B))

          (define EUAHAUBHB (union-cfexp EMPTY AHA BHB))]
    (begin
      (extend-env! WWR 'WWR EUAHAUBHB)
      WWR)))

;;w = a^nb^n
(define ANBN
  (local [(define ANBN (var-cfexp 'ANBN))

          (define ASB (concat-cfexp A ANBN B))

          (define EUASB (union-cfexp EMPTY ASB))]
    (begin
      (extend-env! ANBN 'ANBN EUASB)
      ANBN)))

;;w = a^2ib^i
(define A2iBi
  (local [(define A2iBi (var-cfexp 'A2iBi))

          (define EUAAKB (union-cfexp EMPTY (concat-cfexp A A A2iBi B)))]
    (begin
      (extend-env! A2iBi 'A2iBi EUAAKB)
      A2iBi)))

;;w = A^iB^j
(define AiBj
  (local [(define AiBj (var-cfexp 'AiBj))

          (define AIB (concat-cfexp A AiBj B))

          (define AIBB (concat-cfexp A AiBj B B))

          (define EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))]
    (begin
      (extend-env! AiBj 'AiBj EUAIBUAIBB)
      AiBj)))


(define (loopinator cfe a-num)
  (define (loopinator-helper a-num)
    (if (= a-num 0)
        '()
        (cons (gen-cfexp-word cfe) (loopinator-helper (sub1 a-num)))))
  (remove-duplicates (loopinator-helper a-num)))

(define (valid-wwr-word w)
  (or (eq? w EMP)
      (let* ([w-length (length w)]
             [half-w (take w (/ w-length 2))]
             [w^r (drop w (/ w-length 2))])
        (and (even? w-length)
             (equal? w (append half-w w^r))
             (equal? (reverse half-w) w^r)
             (equal? half-w (reverse w^r))))))

(define (valid-anbn-word w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (even? (length w))
             (equal? w (append as bs))
             (= (length as) (length bs))))))

(define (valid-a2ibi-word w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (equal? w (append as bs))
             (= (length as) (* 2 (length bs)))))))

(define (valid-aibj-word w)
  (or (eq? w EMP)
      (let ([as (filter (λ (s) (eq? s 'a)) w)]
            [bs (filter (λ (s) (eq? s 'b)) w)])
        (and (equal? w (append as bs))
             (<= (length as) (length bs) (* 2 (length as)))))))


;;TESTING
#|
(andmap valid-wwr-word (loopinator WWR 1000))

(andmap valid-anbn-word (loopinator ANBN 1000))

(andmap valid-a2ibi-word (loopinator A2iBi 1000))

(andmap valid-aibj-word (loopinator AiBj 1000))
|#
         