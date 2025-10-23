#lang fsm
(require "quickcheck-invariants.rkt")



(define EVEN-NUM-Bs
  (make-dfa '(S F)
                      '(a b)
                      'S
                      '(S)
                      `((S a S) (S b F)
                                (F a F) (F b S))
                      'no-dead))



;;word -> boolean
;;Purpose: Determine if the given word has an even number of Bs
(define (EVEN-NUM-Bs-S-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))

;;word -> boolean
;;Purpose: Determine if the given word has an odd number of Bs
(define (EVEN-NUM-Bs-F-INV a-word)
  (even? (length (filter (λ (w) (equal? w 'b)) a-word))))



(define LOI-EVEN-Bs (list (list 'S EVEN-NUM-Bs-S-INV)
                          (list 'F EVEN-NUM-Bs-F-INV)))







(define mini-monster-kaboom (make-ndfa '(S A B J K L M)
                                                 '(k a
                                                     ;l
                                                     b
                                                     ;u g c h
                                                     o
                                                     ;i n
                                                     m)
                                                 'S
                                                 '(M)
                                                 `((S a S) (S b S)
                                                           (S o S)
                                                           (S m S) (S k A)
                                                                        
                                                           (A a B) (A k S) (A b S)
                                                           (A o S)
                                                           (A w S) (A m S)
                                                                        
                                                           (B b J) (B k S) (B a S)
                                                           (B o S)
                                                           (B m S)


                                                           (J k S) (J a S)
                                                           (J b S)
                                                           (J m S) (J o K)

                                                           (K k S) (K a S) (K b S)
                                                          
                                                           (K m S) (K o L)

                                                           (L m M) (L k S) (L a S) (L b S)
                                                           (L o S)
                                                          

                                                           (M k M) (M a M) (M b M)
                                                           (M o M)
                                                           (M m M)

                                                           )))



