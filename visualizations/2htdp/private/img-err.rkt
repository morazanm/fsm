#lang racket/base

(provide define/chk 
         x-place?
         y-place?
         mode?
         angle?
         side-count?
         image-color?
         pen-style? 
         pen-cap?
         pen-join?
         real-valued-posn?
         step-count?
         check-mode/color-combination)

(require htdp/error
         racket/class
         racket/contract
         lang/posn
         (except-in racket/draw
                    make-pen make-color)
         mrlib/image-core
         (for-syntax racket/base
                     racket/list))

;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                                                                                 
;                                              ;;                      ;;      ;;                 
;                                              ;;                      ;;      ;;                 
;    ;;;;   ;;;;;;;;;  ;;;;   ;;;;      ;;;;   ;;;;;;   ;;;;    ;;;;   ;; ;;;  ;;  ;; ;;;  ;;;;;; 
;   ;;  ;;  ;;;; ;;;; ;;;;;;  ;;;;     ;;;;;;  ;;;;;;  ;;  ;;  ;;;;;;  ;;;;;   ;;  ;;;;;;  ;;;;;; 
;  ;;;;;;;; ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;;;;;;;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;  ;;;      ;;   ;;  ;;;  ;;; ;;      ;;;      ;;  ;; ;;;     ;;;      ;;;;;   ;;  ;;  ;; ;;;  ;; 
;   ;;; ;;  ;;   ;;   ;;;;;;  ;;       ;;;;;;  ;;  ;;  ;;; ;;  ;;;;;;  ;;  ;;  ;;  ;;  ;;  ;;;;;; 
;    ;;;;   ;;   ;;    ;;;;   ;;        ;;;;   ;;  ;;   ;;;;    ;;;;   ;;  ;;; ;;  ;;  ;;   ;;;;; 
;                                                                                          ;; ;;; 
;                                                                                          ;;;;;  
;                                                                                                 
;


(define-syntax define/chk
  (λ (stx)
    (define (adjust-case fn-name case-args bodies)
      (syntax-case case-args ()
        [(args ... . final-arg)
         (identifier? #'final-arg)
         (let ([len (length (syntax->list #'(args ...)))])
           (with-syntax ([(i ...) (build-list len add1)])
             #`((args ... . final-arg)
                (let ([args (check/normalize '#,fn-name 'args args i)] ...
                      [final-arg 
                       (for/list ([x (in-list final-arg)]
                                  [j (in-naturals #,(+ len 1))])
                         (check/normalize '#,fn-name 'final-arg x j))])
                  #,@bodies))))]
        [(args ...)
         (with-syntax ([(i ...) (build-list (length (syntax->list #'(args ...))) add1)]
                       [(arg-ids ...)
                        (map (λ (arg)
                               (syntax-case arg ()
                                 [x 
                                  (identifier? #'x)
                                  #'x]
                                 [(x y)
                                  (identifier? #'x)
                                  #'x]
                                 [_
                                  (raise-syntax-error 'define/chk "unknown argument spec" stx arg)]))
                             (syntax->list #'(args ...)))])
           #`((args ...)
              (let ([arg-ids (check/normalize '#,fn-name 'arg-ids arg-ids i)] ...)
                #,@bodies)))]))
    (syntax-case stx (case-lambda)
      [(define/chk fn-name (case-lambda [in-args in-body ...] ...))
       (with-syntax ([((args body) ...) (map (lambda (a b) (adjust-case #'fn-name a b))
                                             (syntax->list #'(in-args ...))
                                             (syntax->list #'((in-body ...) ...)))])
         #'(define fn-name
             (case-lambda
               [args body] ...)))]
      [(define/chk (fn-name . args) body ...)
       (with-syntax ([(args body) (adjust-case #'fn-name #'args #'(body ...))])
         (quasisyntax/loc stx (define (fn-name . args) body)))])))

;; check/normalize : symbol symbol any number -> any
;; based on the name of the argument, checks to see if the input
;; is valid and, if so, transforms it to a specific kind of value
;;   width, height -> number
;;   mode -> 'outline 'solid
;;   color -> (is-a?/c color<%>)
(define (check/normalize fn-name argname arg i)
  (case argname
    [(x-place)
     (check-arg fn-name
                (x-place? arg)
                'x-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(y-place) 
     (check-arg fn-name
                (y-place? arg)
                'y-place
                i
                arg)
     (let ([sym (if (string? arg)
                    (string->symbol arg)
                    arg)])
       (if (eq? sym 'center)
           'middle
           sym))]
    [(image image1 image2 image3) 
     (check-arg fn-name
                (image? arg)
                'image
                i
                arg)
     (to-img arg)]
    [(images)
     (check-arg fn-name (and (list? arg) (andmap image? arg)) 'image-list i arg)
     (for/list ([i (in-list arg)])
       (to-img i))]
    [(mode)
     (check-arg fn-name
                (mode? arg)
                'mode
                i
                arg)
     (cond
       [(or (equal? arg "solid")
            (equal? arg 'solid))
        255]
       [(equal? arg "outline")
        'outline]
       [(and (integer? arg)
             (not (exact? arg)))
        (inexact->exact arg)]
       [else arg])]
    [(width height radius radius1 radius2 side-length side-length1 side-length2
            side-a side-b side-c)
     (check-arg fn-name
                (and (real? arg)
                     (not (negative? arg)))
                'non\ negative\ real\ number
                i arg)
     arg]
    [(point-count)
     (check-arg fn-name
                (and (integer? arg)
                     (>= arg 2))
                'integer\ greater\ than\ or\ equal\ to\ 2
                i arg)
     (inexact->exact arg)]
    [(dx dy x y x1 y1 x2 y2 pull pull1 pull2)
     (check-arg fn-name
                (real? arg)
                'real\ number
                i arg)
     arg]
    [(factor x-factor y-factor non-zero-radius)
     (check-arg fn-name
                (and (real? arg)
                     (positive? arg))
                'positive\ real\ number
                i arg)
     arg]
    [(side-count)
     (check-arg fn-name
                (side-count? arg)
                'side-count
                i arg)
     (inexact->exact arg)]
    [(step-count)
     (check-arg fn-name
                (step-count? arg)
                'step-count
                i arg)
     (inexact->exact arg)]
    [(angle angle1 angle2 angle-a angle-b angle-c)
     (check-arg fn-name
                (angle? arg)
                'angle\ in\ degrees
                i arg)
     (angle->proper-range arg)]
    [(angle-between-0-and-360)
     (check-arg fn-name
                (angle? arg)
                'angle\ in\ degrees
                i arg)
     (check-arg fn-name
                (<= 0 arg 360)
                '|angle between 0 and 360|
                i arg)
     (check-arg fn-name
                (not (= 0 arg))
                '|angle that is not 0|
                i arg)
     (check-arg fn-name
                (not (= 360 arg))
                '|angle that is not 360|
                i arg)
     arg]
    [(color-only)
     (check-arg fn-name (image-color? arg) 'image-color i arg)
     (cond
       [(color? arg) arg]
       [(string? arg) arg]
       [(symbol? arg) (symbol->string arg)])]
    [(color)
     (check-arg fn-name (or (image-color? arg) (pen? arg)) 'image-color-or-pen i arg)
     ;; return either a string, color, or a pen,
     ;; (technically, the string case is redundant,
     ;;  but since there may be saved files that have 
     ;;  strings in the color positions we leave them
     ;;  here too; note that using a pen struct means
     ;;  'smoothed mode, but a color (or string) means
     ;;  'aligned mode, so that's not redundant).
     (cond
       [(color? arg) arg]
       [(pen? arg) arg]
       [(symbol? arg) (symbol->string arg)]
       [(string? arg) arg])]
    [(color-list)
     (check-arg fn-name (and (list? arg) (andmap image-color? arg)) 'color-list i arg)
     arg]
    [(string)
     (check-arg fn-name (string? arg) 'string i arg)
     arg]
    [(font-size)
     (check-arg fn-name (and (integer? arg) (<= 1 arg 255)) 'font-size i arg)
     (inexact->exact arg)]
    [(face)
     (check-arg fn-name (or (not arg) (string? arg)) 'face i arg)
     arg]
    [(family)
     (let ([syms '(default decorative roman script swiss modern symbol system)])
       (check-arg fn-name
                  (or (memq arg syms)
                      (and (string? arg)
                           (memq (string->symbol arg) syms)))
                  'family i arg))
     (if (string? arg) (string->symbol arg) arg)]
    [(style)
     (let ([syms '(normal italic slant)])
       (check-arg fn-name (or (memq arg syms)
                              (and (string? arg)
                                   (memq (string->symbol arg) syms)))
                  'style i arg))
     (if (string? arg) (string->symbol arg) arg)]
    [(weight)
     (let ([syms '(normal bold light)])
       (check-arg fn-name (or (memq arg syms)
                              (and (string? arg)
                                   (memq (string->symbol arg) syms)))
                  'weight i arg))
     (if (string? arg) (string->symbol arg) arg)]
    [(underline)
     (and arg #t)]
    [(posns)
     (check-arg fn-name
                (and (list? arg)
                     (andmap posn? arg))
                'list-of-posns
                i arg)
     (check-arg fn-name
                (andmap real-valued-posn? arg)
                'list-of-posns-with-real-valued-x-and-y-coordinates
                i arg)
     (check-arg fn-name
                (>= (length arg) 3)
                'list-of-at-least-three-posns
                i arg)
     arg]
    [(posns-or-pulled-points)
     (check-arg fn-name
                (and (list? arg)
                     (andmap (or/c posn? pulled-point?) arg))
                'list-of-posns-or-pulled-points
                i arg)
     (check-arg fn-name
                (andmap (or/c pulled-point? real-valued-posn?) arg)
                'list-of-posns-with-real-valued-x-and-y-coordinates
                i arg)
     (check-arg fn-name
                (>= (length arg) 3)
                'list-of-at-least-three-posns-or-pulled-points
                i arg)
     arg]
    [(zero-or-more-posns)
     (check-arg fn-name
                (and (list? arg)
                     (andmap posn? arg))
                'list-of-posns
                i arg)
     (check-arg fn-name
                (andmap real-valued-posn? arg)
                'list-of-posns-with-real-valued-x-and-y-coordinates
                i arg)
     arg]
    [(int-0-255 int0-255-1 int0-255-2 int0-255-3 int0-255-4)
     (check-arg fn-name (and (integer? arg) (<= 0 arg 255)) 
                'integer\ between\ 0\ and\ 255 i arg)
     (inexact->exact arg)]
    [(real-0-255)
     (check-arg fn-name (and (real? arg) (<= 0 arg 255))
                'real\ between\ 0\ and\ 255 i arg)
     arg]
    
    [(pen-style)
     (check-arg fn-name (pen-style? arg) 'pen-style i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(pen-cap)
     (check-arg fn-name (pen-cap? arg) 'pen-cap i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(pen-join)
     (check-arg fn-name (pen-join? arg) 'pen-join i arg)
     (if (string? arg)
         (string->symbol arg)
         arg)]
    [(filename)
     (check-arg fn-name (path-string? arg) 'path-string i arg)
     arg]
    [else
     (error 'check "the function ~a has an argument with an unknown name: ~s"
            fn-name
            argname)]))

(define (y-place? arg)
  (and (member arg '("top" top "bottom" bottom "middle" middle "center" center 
                           "baseline" baseline "pinhole" pinhole))
       #t))
(define (x-place? arg)
  (and (member arg '("left" left "right" right "middle" middle
                            "center" center "pinhole" pinhole))
       #t))
(define (mode? arg)
  (or (and (member arg '(solid outline "solid" "outline")) #t)
      (and (integer? arg)
           (<= 0 arg 255))))
(define (side-count? i)
  (and (integer? i)
       (3 . <= .  i)))
(define (step-count? i)
  (and (integer? i)
       (1 . <= .  i)))
(define (image-color? c)
  (cond
    [(color? c) #t]
    [(symbol? c)
     (and (string->color-object/f (symbol->string c)) #t)]
    [(string? c)
     (and (string->color-object/f c) #t)]
    [else #f]))

(define (pen-style? arg) 
  (and (member (if (string? arg) (string->symbol arg) arg)
               '(solid dot long-dash short-dash dot-dash))
       #t))
(define (pen-cap? arg)
  (and (member (if (string? arg) (string->symbol arg) arg)
               '(round projecting butt))
       #t))
(define (pen-join? arg)
  (and (member (if (string? arg) (string->symbol arg) arg)
               '(round bevel miter))
       #t))
(define (real-valued-posn? arg)
  (and (posn? arg)
       (real? (posn-x arg))
       (real? (posn-y arg))))


;; checks the dependent part of the 'color' specification
(define (check-mode/color-combination fn-name i mode color)
  (cond
    [(or (eq? mode 'solid)
         (number? mode))
     (check-arg fn-name (image-color? color) 'image-color i color)]
    [(eq? mode 'outline)
     (void)]))

(define (angle? arg)
  (and (real? arg)
       (not (or (= arg +inf.0)
                (= arg -inf.0)
                (equal? arg +nan.0)))))
