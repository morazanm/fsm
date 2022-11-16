#lang racket
(require "../../fsm-core/private/rule-pred.rkt" "../test-helpers.rkt")


(module+ test
  (require rackunit rackunit/text-ui)

  (define all-both-test
    (test-suite "Tests for allBoth function"
                (test-case "Marco tests"
                           (check-equal? (allBoth 'AbAbA '(A) '(b)) #t)
                           (check-equal? (allBoth 'Abbbbbb '(b A) '()) #t)
                           (check-equal? (allBoth 'Edeee '(E) '(d i s es)) #f))))

  (define oneAndOne-test
    (test-suite "Tests for oneAndOne function"
                (test-case "Marco tests"
                           (check-equal? (oneAndOne 'aB '(A) '(b)) #f)
                           (check-equal? (oneAndOne 'Ab '(A) '(b)) #f)
                           (check-equal? (oneAndOne 'A '(A) '(b)) #f)
                           (check-equal? (oneAndOne 'b '(A) '(b)) #t)
                           (check-equal? (oneAndOne 'bA '(A) '(b)) #t)
                           (check-equal? (oneAndOne 'bB '(A B) '(a b)) #t)
                           (check-equal? (oneAndOne 'aA '(A B) '(a b)) #t)
                           (check-equal? (oneAndOne 'bA '(A) '(b)) #t))))

  (define allOne-tests
    (test-suite "Tests for allOne function"
                (test-case "Marco tests"
                           (check-equal? (allOne 'ajd '(a j d b s e)) #t)
                           (check-equal? (allOne 'abd '(e a d r s)) #f))))

  (define hasOne-tests
    (test-suite "Tests for hasOne function"
                (test-case "Marco tests"
                           (check-equal? (hasOne 'AbueopjsfaH '(y r l w p j s)) #t)
                           (check-equal? (hasOne 'asdfghjkl '(q w e r t y u i)) #f))))


  (test-all 'verbose
            (all-both-test
             oneAndOne-test
             allOne-tests
             hasOne-tests))

  );; end sub module