#lang fsm


;;Let Σ = {a b}. Design and implement a pda for L = {w | w has 3
;; times as many as than b}

(define 3xa-b (make-ndpda '(S A B C D E)
                          '(a b)
                          '(a)
                          'S
                          '(S E)
                          `(((S ,EMP ,EMP) (A ,EMP))
                            ((A b ,EMP) (A (a a a)))
                            ((A b (b)) (A ,EMP))
                            ((A b ,EMP) (B (a a a)))
                            ((A a (a)) (C ,EMP))
                            ((B b ,EMP) (B (a a a)))
                            ((B b (b)) (B ,EMP))
                            ((B a (a)) (C ,EMP))
                            ((C b ,EMP) (C (a a a)))
                            ((C b (b)) (C ,EMP))
                            ((C a (a)) (D ,EMP))
                            ((D b ,EMP) (D (a a a)))
                            ((D b (b)) (D b ,EMP))
                            ((D a ,EMP) (E (b)))
                            ((E b ,EMP) (E (a a a)))
                            ((E b (b)) (E ,EMP))
                            ((E a (a)) (A ,EMP)))))


(define a^mb^nc^p (make-ndpda '(S A B C D E F)
                              '(a b c)
                              '(a)
                              'S
                              '(C F)
                              `(((S ,EMP ,EMP) (A ,EMP))
                                ((S ,EMP ,EMP) (D ,EMP))
                                ((A a ,EMP) (A (a)))
                                ((A ,EMP ,EMP) (B ,EMP))
                                ((B b (a)) (B ,EMP))
                                ((B ,EMP ,EMP) (C ,EMP))
                                ((C c ,EMP) (C ,EMP))
                                ((D a ,EMP) (D ,EMP))
                                ((D ,EMP ,EMP) (E ,EMP))
                                ((E b ,EMP) (E (a)))
                                ((E ,EMP ,EMP) (F ,EMP))
                                ((F c (a)) (F ,EMP)))))

