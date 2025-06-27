#lang racket

(require  "../fsm-core/private/constants.rkt"
          "context-free-expressions-constructors.rkt"
          rackunit)

(define EMPTY (empty-cfexp))

(define A (singleton-cfexp 'a))

(define B (singleton-cfexp 'b))

;; w = ww^r
(define WWR 
  (local [(define WWR (var-cfexp 'WWR))

          (define AHA (concat-cfexp A WWR A))

          (define BHB (concat-cfexp B WWR B))]
    (begin
      (update-binding! WWR 'WWR (union-cfexp EMPTY AHA BHB))
      WWR)))

;;w = a^nb^n
(define ANBN
  (local [(define ANBN (var-cfexp 'ANBN))

          (define ASB (concat-cfexp A ANBN B))]
    (begin
      (update-binding! ANBN 'ANBN (union-cfexp EMPTY ASB))
      ANBN)))

(define WWRUANBN (union-cfexp WWR ANBN))

;;w = a^2ib^i
(define A2iBi
  (local [(define A2iBi (var-cfexp 'A2iBi))

          (define EUAAKB (union-cfexp EMPTY (concat-cfexp A A A2iBi B)))]
    (begin
      (update-binding! A2iBi 'A2iBi EUAAKB)
      A2iBi)))

;;w = A^iB^j
(define AiBj
  (local [(define AiBj (var-cfexp 'AiBj))

          (define AIB (concat-cfexp A AiBj B))

          (define AIBB (concat-cfexp A AiBj B B))

          (define EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))]
    (begin
      (update-binding! AiBj 'AiBj EUAIBUAIBB)
      AiBj)))


(define S1
  (local [(define ANBN (var-cfexp 'S))

          (define ASB (concat-cfexp A ANBN B))]
    (begin
      (update-binding! ANBN 'S (union-cfexp EMPTY ASB))
      ANBN)))


(define S2
  (local [(define AiBj (var-cfexp 'S))

          (define AIB (concat-cfexp A AiBj B))

          (define AIBB (concat-cfexp A AiBj B B))

          (define EUAIBUAIBB (union-cfexp EMPTY AIB AIBB))]
    (begin
      (update-binding! AiBj 'S EUAIBUAIBB)
      AiBj)))

(define test (union-cfexp S1 S2))
      


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

(check-pred (λ (low) (andmap valid-wwr-word low)) (loopinator WWR 100000))

(check-pred (λ (low) (andmap valid-anbn-word low)) (loopinator ANBN 100000))

(check-pred (λ (low) (andmap valid-a2ibi-word low)) (loopinator A2iBi 100000))

(check-pred (λ (low) (andmap valid-aibj-word low)) (loopinator AiBj 100000))

         