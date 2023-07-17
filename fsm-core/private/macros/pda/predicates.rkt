(module predicates racket
  (require
    "../../constants.rkt"
    "../../pda.rkt"
    )
  (provide
   check-input-ndpda
   valid-ndpda-rule?
   )
  (define (check-input-ndpda states
                            sigma
                            gamma
                            start
                            finals
                            rules
                            accepts?)
    (lambda (words)
      (define temp-machine (make-unchecked-ndpda states
                                                sigma
                                                gamma
                                                start
                                                finals
                                                rules))
      (andmap (lambda (x) (equal? (temp-machine x) accepts?)) words)
      )
    )

  ;; Purpose: Checks if the given rule is valid, according to the states,
  ;; alphabet, and gamma. An ndpda-rule is valid if it meets the following
  ;; criteria:
  ;; 1. It is a list with two elements
  ;; 2. The first element is a valid "pop" rule fragment
  ;; 3. The second element is a valid "push" rule fragment.
  ;; See valid-ndpda-pop and valid-ndpda-push for specifics on "pop" and "push"
  ;; fragments.
  (define ((valid-ndpda-rule? states alphabet gamma) rule)
    (and (list? rule)
         (= (length rule) 2)
         (valid-ndpda-pop (first rule) states alphabet gamma)
         (valid-ndpda-push (second rule) states gamma)))

  ;; Purpose: Checks if the given input is a valid ndpda pop fragment
  ;; In order to be valid, it must:
  ;; 1. Be a list with three elements
  ;; 2. The first element must be a state from the list of states
  ;; 3. The second element must be a letter from the alphabet
  ;; 4. The third element must either be the EMP constant, or a list of symbols,
  ;;    all of which must be in the gamma.
  (define (valid-ndpda-pop fragment states alphabet gamma)
    (and (list? fragment)
         (= (length fragment) 3)
         (member (first fragment) states)
         (or (member (second fragment) alphabet) (equal? (second fragment) EMP))
         (or (equal? (third fragment) EMP)
             (and (list? (third fragment))
                  (not (empty? (third fragment)))
                  (andmap (lambda (sym) (member sym gamma)) (third fragment)))))
    )

  ;; Purpose: Checks to see if the given input is a valid ndpda push fragment
  ;; In order to be valid, it must:
  ;; 1. Be a list with two elements
  ;; 2. The first element must be a state frmo the list of states
  ;; 3. The second element must either be the EMP constant, or a list of symbols,
  ;;    all of which must be in the gamma.
  (define (valid-ndpda-push fragment states gamma)
    (and (list? fragment)
         (= (length fragment) 2)
         (member (first fragment) states)
         (or (equal? (second fragment) EMP)
             (and (list? (second fragment))
                  (not (empty? (second fragment)))
                  (andmap (lambda (sym) (member sym gamma)) (second fragment))))))
  )