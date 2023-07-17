(module predicates racket
  (require racket/contract
           "../constants.rkt"
           "../sm-getters.rkt"
           "../fsa.rkt"
           "../tm.rkt"
           "../pda.rkt"
           "../../../main.rkt")
  (provide
   valid-rules?
   valid-finals?           
   valid-start?
   start-in-states?
   valid-sigma?
   valid-list-of-states?
   valid-state?
   valid-alpha?
   invalid-rules
   dfa?
   ndpda?
   ndfa?
   tm?
   listof-words?
   )

  (define (dfa? machine)
    (equal? (sm-type machine) 'dfa))

  (define (ndpda? machine)
    (equal? (sm-type machine) 'pda))
  
  (define (ndfa? machine)
    (equal? (sm-type machine) 'ndfa))

  (define (tm? machine)
    (or (equal? (sm-type machine) 'tm-language-recognizer)
        (equal? (sm-type machine) 'tm)))

    
  ;applies the predicate to the list and returns false if
  ; any of the members of the list are invalid states/sigma
  (define (valid-list-of los pred)
    (andmap pred los)
    )

  (define (valid-state? x)
    (define regex-pattern #px"^[A-Z](?:-[0-9]+)?$")
    (or (equal? x DEAD)
        (if (symbol? x)
            (regexp-match regex-pattern (symbol->string x))
            #f)
        )
    )

  (define (valid-list-of-states? states)
    (valid-list-of states valid-state?)
    )

  (define (valid-alpha? x)
    (define regex-pattern #px"[a-z]")
    (if (symbol? x)
        (regexp-match regex-pattern (symbol->string x))
        #f)
    )

  (define (valid-sigma? sigma)
    (valid-list-of sigma valid-alpha?)
    )

  (define (start-in-states? states)
    (lambda (start)
      (member start states)
      )
    )

  (define (valid-start? states)
    (lambda (start)
      (and (symbol? start)
           (valid-state? start)))
    )

  (define (valid-finals? states)
    (lambda (finals)
      (andmap (lambda (x) (member x states)) finals)
      )
    )

  (define (valid-rules? pred rules)
    (andmap (lambda (rule) (pred rule)) rules))

  (define (invalid-rules pred rules)
    (filter (lambda (rule) (not (pred rule)))
            rules)
    )

  (define (listof-words? words sigma)
    (and (list? words)
         (andmap (lambda (word) (and (list? word)
                                     (andmap (lambda (letter) (symbol? letter))
                                             word))) words))
    )

  )
