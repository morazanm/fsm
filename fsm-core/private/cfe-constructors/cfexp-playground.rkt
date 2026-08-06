#lang racket/base

(require  "../constants.rkt"
          "context-free-expressions-constructors.rkt"
          "../cfg-struct.rkt"
          "../pda.rkt"
          "../../../sm-graph.rkt"          
          "construct-cfe-macro.rkt"
          )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PDA->CFE & CFE->PDA Transformations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define AnCkBn
  (make-cfe ([EMPTY (empty-cfexp)]
             [A (singleton-cfexp "a")]
             [B (singleton-cfexp "b")]
             [C (singleton-cfexp "c")]
             [ASB (union-cfexp (concat-cfexp A ASB B) Ck)]
             [Ck (union-cfexp (concat-cfexp C Ck) EMPTY)])
    ASB))

(define AiBj (make-cfe ([EMPTY (empty-cfexp)]
                        [A (singleton-cfexp "a")]
                        [B (singleton-cfexp "b")]
                        [AiBj (union-cfexp EMPTY
                                           (concat-cfexp A AiBj B)
                                           (concat-cfexp A AiBj B B))])
                       AiBj))

(define AnBncKUAkBnCn (make-cfe ([EMPTY (empty-cfexp)]
                                 [A (singleton-cfexp "a")]
                                 [B (singleton-cfexp "b")]
                                 [C (singleton-cfexp "c")]
                                 [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
                                 [CF (union-cfexp (concat-cfexp C CF) EMPTY)]
                                 [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
                                 [AZ (union-cfexp (concat-cfexp A AZ) EMPTY)]
                                 [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
                                AiBjCk))

(define AnBncKUAkBnCn2 (make-cfe ([EMPTY (empty-cfexp)]
                                  [A (singleton-cfexp "a")]
                                  [B (singleton-cfexp "b")]
                                  [C (singleton-cfexp "c")]
                                  [AEB (union-cfexp EMPTY (concat-cfexp A AEB B))]
                                  [CF (kleenestar-cfexp C)]
                                  [BWC (union-cfexp (concat-cfexp B BWC C) EMPTY)]
                                  [AZ (kleenestar-cfexp A)]
                                  [AiBjCk (union-cfexp (concat-cfexp AEB CF) (concat-cfexp AZ BWC))])
                                 AiBjCk))


;;L = wcw^r
(define WcWr (make-cfe ([A (singleton-cfexp "a")]
                        [B (singleton-cfexp "b")]
                        [C (singleton-cfexp "c")]
                        [WcWr (union-cfexp (concat-cfexp A WcWr A)
                                           (concat-cfexp B WcWr B)
                                           C)])
                       WcWr))

