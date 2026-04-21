#lang racket/base
(provide (struct-out config)
         (struct-out stack)
         (struct-out computation)
         (struct-out PDA-rule)
         (struct-out PATH)
         #;(all-defined-out))

(struct config (state word stack) #:transparent)

(struct PATH (lor stack word path-length destination-state) #:transparent)

(struct stack (elems len) #:transparent)

(struct PDA-rule (source read pop destination push push-len pop-length) #:transparent)

(struct computation (loc length) #:transparent)