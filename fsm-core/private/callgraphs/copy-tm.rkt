#lang fsm

;; PRE:  tape = (LM BLANK w BLANK) and head on blank after w
;; POST: tape = (LM BLANK w BLANK w BLANK) and head on blank after second w
(define COPYTM
  (make-tm '(S A B C D E F G H I J K L M N O P Q R T U V W Y Z)
           '(a b)
           `(;; FBL
             ((S a) (C ,LEFT))
             ((S b) (C ,LEFT))
             ((S ,BLANK) (C ,LEFT))
             ((C a) (C ,LEFT))
             ((C b) (C ,LEFT))
             ((C ,BLANK) (A ,BLANK))
             ;; R
             ((A a) (B ,RIGHT))
             ((A b) (B ,RIGHT))
             ((A ,BLANK) (B ,RIGHT))
             ;; Branch
             ((B a) (D a))
             ((B b) (E b))
             ((B ,BLANK) (F ,RIGHT))
             ;; a Branch
             ;; WB
             ((D a) (G ,BLANK))
             ;; FBR
             ((G a) (H ,RIGHT))
             ((G b) (H ,RIGHT))
             ((G ,BLANK) (H ,RIGHT))
             ((H a) (H ,RIGHT))
             ((H b) (H ,RIGHT))
             ((H ,BLANK) (I ,RIGHT))
             ;; FBR
             ((I a) (I ,RIGHT))
             ((I b) (I ,RIGHT))
             ((I ,BLANK) (L ,BLANK))
             ;; k
             ((L ,BLANK) (L a))
             ((L a) (J ,LEFT))
             ;; FBL
             ((J a) (J ,LEFT))
             ((J b) (J ,LEFT))
             ((J ,BLANK) (K ,LEFT))
             ;; FBL
             ((K a) (K ,LEFT))
             ((K b) (K ,LEFT))
             ((K ,BLANK) (M ,BLANK))
             ;; k
             ((M ,BLANK) (M a))
             ((M a) (A a))       
             ;; b Branch
             ;; WB
             ((E b) (N ,BLANK))
             ;; FBR
             ((N a) (O ,RIGHT))
             ((N b) (O ,RIGHT))
             ((N ,BLANK) (O ,RIGHT))
             ((O a) (O ,RIGHT))
             ((O b) (O ,RIGHT))
             ((O ,BLANK) (P ,RIGHT))
             ;; FBR
             ((P a) (P ,RIGHT))
             ((P b) (P ,RIGHT))
             ((P ,BLANK) (Q ,BLANK))
             ;; k
             ((Q ,BLANK) (Q b))
             ((Q b) (R ,LEFT))
             ;; FBL
             ((R a) (R ,LEFT))
             ((R b) (R ,LEFT))
             ((R ,BLANK) (Y ,LEFT))
             ;; FBL
             ((Y a) (Y ,LEFT))
             ((Y b) (Y ,LEFT))
             ((Y ,BLANK) (T ,BLANK))
             ;; k
             ((T ,BLANK) (T b))
             ((T b) (A b))     
             ;; BLANK Branch
             ;; FBR
             ((F a) (F ,RIGHT))
             ((F b) (F ,RIGHT))
             ;; L
             ((F ,BLANK) (U ,LEFT))
             ;; Branch
             ((U a) (W ,RIGHT))
             ((U b) (W ,RIGHT))
             ((U ,BLANK) (Z ,RIGHT))
             ;; a, b Branch
             ((W ,BLANK) (V ,BLANK))
             ;; BLANK Branch
             ((Z ,BLANK) (W ,RIGHT)))                     
           'S
           '(V)))

;(sm-showtransitions COPYTM '(@ _ a b _) 4)
;(sm-showtransitions COPYTM '(@ _ _ _) 3)
;(sm-graph COPYTM)