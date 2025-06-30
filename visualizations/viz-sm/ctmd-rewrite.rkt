#lang racket/base

(require syntax-spec-v3
         "../../fsm-core/private/tm.rkt"
         "../../fsm-core/private/constants.rkt"
         (for-syntax syntax/parse racket/base))

(provide new-combine-tms)

(struct ctm (ctm-obj ctm-lst))

(define-syntax (process-ctmd-expr stx)
  (define-syntax-class goto-expr
    (pattern ((~datum GOTO) a-label)
      #:attr label #'a-label))

  (define-syntax-class var-set-expr
    (pattern (((~datum VAR) a-symbol:expr) a-body:expr)
      #:attr symb #'a-symbol
      #:attr body #'a-body))

  (define-syntax-class label-expr
    (pattern ((~datum LABEL) num:expr)
      #:attr label #'num))

  (define-syntax-class branch-path-expr
    (pattern (val:expr a-jmp:goto-expr)
      #:attr match #'val
      #:attr goto #'a-jmp))
  
  (syntax-parse stx
    [(_ a-goto:goto-expr)
     #'(list GOTO a-goto.label)]
    [(_ a-var-set:var-set-expr)
     #'(list (list VAR a-var-set.symb) (process-ctmd-expr a-var-set.body))]
    [(_ a-label-expr:label-expr)
     #'a-label-expr.label]
    [(_ ((~datum BRANCH) (a-path:branch-path-expr ...)))
     #'(list BRANCH
             (list a-path.match (process-ctmd-expr a-path.goto))
             ...)]
    [(_ e:expr)
     #'e]))

(define-syntax (make-ctm-lst stx)
  (syntax-parse stx
    [(_ e:expr ...)
     #'(list (process-ctmd-expr e) ...)]))

(define-syntax compile
  (syntax-parser
    #:datum-literals (GOTO VAR BRANCH LABEL combine-tms)
    [(_ (e:expr ...) alphabet:expr)
     #'(let ([ctm-lst (make-ctm-lst e ...)])
         (ctm (combine-tms ctm-lst alphabet) ctm-lst))]))

(syntax-spec
 (host-interface/expression
  (_new-combine-tms (a-ctmd-expr:ctmd-expr-or-top-lvl-label ...) alphabet:expr)
  #:binding (scope (import a-ctmd-expr) ...)
  #'(compile (a-ctmd-expr ...) alphabet))
 (binding-class top-level-label)
 (binding-class var-level-label)
 (binding-class variable)
 (nonterminal goto-expr
              #:binding-space ctmd
              ((~datum GOTO) a-label:top-level-label)
              ((~datum GOTO) a-label:var-level-label))
 (nonterminal var-set-expr
              #:binding-space ctmd
              (((~datum VAR) a-symbol:variable) c0:ctmd-expr)
              #:binding (scope (bind a-symbol) c0))
 (nonterminal/exporting top-level-label-expr
                        #:binding-space ctmd
                        ((~datum LABEL) num:top-level-label)
                        #:binding (export num))
 (nonterminal branch-expr
              #:binding-space ctmd
              ((~datum BRANCH) ((val:expr a-jmp:goto-expr) ...)))
 (nonterminal ctmd-expr
              #:binding-space ctmd
              (~> ((~datum list) e:expr ...)
                  #'(e ...))
              (~> ((~datum cons) (~datum BRANCH) branches:expr)
                  #'(BRANCH branches))
              (~> ((~datum BRANCH) ((~datum list) ((~datum list) val:expr ((~datum list) (~datum GOTO) a-label:expr)) ...))
                  #'(BRANCH ((val (GOTO a-label)) ...)))
              '()
              a-tm:expr
              a-goto:goto-expr
              a-var-set:var-set-expr
              a-branch:branch-expr
              a-var:variable)
 (nonterminal/exporting ctmd-expr-or-top-lvl-label
                        #:binding-space ctmd
                        (~> n:number #'(LABEL n))
                        a-ctmd:ctmd-expr
                        a-label:top-level-label-expr
                        #:binding (re-export a-label)))

(define-syntax (new-combine-tms stx)
  (syntax-parse stx
    [(_ ((~datum list) e:expr ...) alphabet:expr)
     #'(_new-combine-tms (e ...) alphabet)]
    [(_ (e:expr ...) alphabet:expr)
     #'(_new-combine-tms (e ...) alphabet)]))