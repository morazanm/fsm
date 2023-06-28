#lang racket
(provide define-invariants-for sm-visualize!)

(require
  (for-syntax syntax/parse racket/syntax syntax/stx racket/list racket/match)
  syntax/to-string "electron-backend/server.rkt")

(begin-for-syntax
  (define ID-INV "~a-$inv$-~a")
  (define INV-LIST-ID "~a-inv-gui-list")
  
  ;; A common scope for storing variables 
  (define common-ctx #'common-context-for-hidden-ids)

  ;; check-duplicates* :: [syntax] -> syntax | boolean
  ;; if a duplicate identifier is found then the syntax is returned
  (define (check-duplicates* stx-list)
    (define/match (helper lst acc)
      [(`() _) #f]
      [(`(,x ,xs ...) a)
       (if (member (syntax-e x) a) x (helper xs (cons (syntax-e x) a)))])
    (helper stx-list '()))

  ;; Syntax class for racket define keyword
  (define-syntax-class racket-define
    #:literals (define)
    (pattern (define n:id e:expr))
    (pattern (define (n:id params:id ...) body:expr ...+)))
      
  ;; Syntax for associating a invariant with a state in the machine
  (define-syntax-class (invariant-func machine-name)
    #:datum-literals (define-invariant-for)
    (pattern (~and (define-invariant-for state:id (args:id ...) body:expr ...+)
                   stx)
      #:with id (syntax-local-introduce (format-id common-ctx ID-INV machine-name #'state))
      #:with str-value #`(regexp-replace #px"define-invariant-for *[^\\s]* *"
                                         (syntax->string #`(stx))
                                         "define "))))


(define-syntax (define-invariants-for stx)
  (syntax-parse stx
    [(_ m-name:id (~or func:racket-define (~var func (invariant-func #'m-name))) ...+)
     #:fail-when (empty? (syntax->list #`((~? func.state) ...)))
     "Expected at least one 'define-invariant-for' clause"
     #:fail-when (check-duplicates* (syntax->list #`((~? func.state) ...)))
     "Duplicate invariant for state found"
     #:with list-name (syntax-local-introduce (format-id common-ctx INV-LIST-ID #'m-name))
     #`(begin
         ;; Add the invariant functions
         (~? (define (func.id func.args ...) func.body ...) func) ...

         ;; Add both values to a list so we can call it from sm-visualize
         (define list-name (list (~? (list 'func.state func.str-value func.id)) ...)))]
    [(_ _) (raise-syntax-error #f "Expected at least one 'define-invariant-for' clause" stx)]))


(define-syntax (sm-visualize! stx)
  (syntax-parse stx
    [(_ fsa:id)
     #:with inv-list-id (format-id #'fsa INV-LIST-ID #'fsa)
     #:fail-when (not (identifier-binding #'inv-list-id))
     "Invariants not defined for fsa"
     #`(run-with-prebuilt fsa inv-list-id)]
    ;; If invariants where not defined then defeult to this case
    [(_ fsa)
     #`(run-with-prebuilt fsa '())]))

(module+ test
  (require rackunit syntax/macro-testing)
  (define a^nb^nc^n2 'dummy)


  (check-exn #rx"Duplicate invariant for state found"
             (lambda ()
               (define S-INV (lambda (v) #t))
               (convert-compile-time-error
                (define-invariants-for a^nb^nc^n2
                  (define get-num-of-x (lambda (tape symbol)
                                         (length (filter (lambda (ele)
                                                           (eq? ele symbol)) tape))))

                  (define-invariant-for B (tape posn)
                    (let ((num-z (get-num-of-x tape 'z)))
                      (and (> num-z (get-num-of-x tape 'x))
                           (> num-z (get-num-of-x tape 'y)))))

                  ;; the number of x's before the first b is one more then the number of x's after the first b
                  (define-invariant-for C (tape posn)
                    (let ((num-x (get-num-of-x tape 'x)))
                      (and (= num-x (get-num-of-x tape 'z))
                           (> num-x (get-num-of-x tape 'y)))))
                  

                  ;; the number of z's is one bigger then number of x's and number of y's
                  (define-invariant-for B (tape posn)
                    (let ((num-z (get-num-of-x tape 'z)))
                      (and (> num-z (get-num-of-x tape 'x))
                           (> num-z (get-num-of-x tape 'y)))))))))
  

  (check-exn #rx"Expected at least one 'define-invariant-for' clause"
             (lambda ()
               (convert-compile-time-error (define-invariants-for a^nb^nc^n2 ))))
  

  (check-exn #rx"Expected at least one 'define-invariant-for' clause"
             (lambda ()
               (convert-compile-time-error (define-invariants-for a^nb^nc^n2
                                             (define x 10)))))

  ) ;;end module+ test
