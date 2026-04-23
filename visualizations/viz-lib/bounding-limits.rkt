#lang racket/base
(provide (struct-out posn)
         (struct-out bounding-limits)
         bounding-limits-height
         bounding-limits-width
         within-bounding-limits?
         outside-north-side-bounding-limits?
         outside-south-side-bounding-limits?
         outside-east-side-bounding-limits?
         outside-west-side-bounding-limits?
         outside-west-north-sides-bounding-limits?
         outside-west-south-sides-bounding-limits?
         outside-east-north-sides-bounding-limits?
         outside-east-south-sides-bounding-limits?
         outside-x-axis-bounding-limits?
         outside-y-axis-bounding-limits?
         )

;; Coordinate pair
;; num num -> posn
(struct posn (x y))

;; Bounding limits is a struct containing the bounding X and Y
;; values of where we want the image to be placed within
;; num num num num -> bounding-limits
(struct bounding-limits (min-x max-x min-y max-y) #:transparent)

;; bounding-limits -> num
;; Purpose: Returns the height of the bounded area
(define (bounding-limits-height b-limit) (- (bounding-limits-max-y b-limit) (bounding-limits-min-y b-limit)))

;; bounding-limits -> num
;; Purpose: Returns the width of the bounded area
(define (bounding-limits-width b-limit) (- (bounding-limits-max-x b-limit) (bounding-limits-min-x b-limit)))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is within the bounded area or not
(define (within-bounding-limits? b-limit a-posn)
  (and (<= (bounding-limits-min-y b-limit) (posn-y a-posn))
       (>= (bounding-limits-max-y b-limit) (posn-y a-posn))
       (<= (bounding-limits-min-x b-limit) (posn-x a-posn))
       (>= (bounding-limits-max-x b-limit) (posn-x a-posn))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn's Y position is beyond only the minimum Y value
(define (outside-north-side-bounding-limits? b-limit a-posn)
  (> (bounding-limits-min-y b-limit) (posn-y a-posn)))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn's Y position is beyond only the maximum Y value
(define (outside-south-side-bounding-limits? b-limit a-posn)
  (> (posn-y a-posn) (bounding-limits-max-y b-limit)))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn's X position is beyond only the maximum X value
(define (outside-east-side-bounding-limits? b-limit a-posn)
  (> (posn-x a-posn) (bounding-limits-max-x b-limit)))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn's X position is beyond only the minimum X value
(define (outside-west-side-bounding-limits? b-limit a-posn)
  (> (bounding-limits-min-x b-limit) (posn-x a-posn)))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond the minimum X value, and is beyond the minimum Y value
(define (outside-west-north-sides-bounding-limits? b-limit a-posn)
  (and (> (bounding-limits-min-x b-limit) (posn-x a-posn))
       (<= (posn-x a-posn) (bounding-limits-max-x b-limit))
       (> (bounding-limits-min-y b-limit) (posn-y a-posn))
       (<= (posn-y a-posn) (bounding-limits-max-y b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond the minimum X value, and is beyond the maximum Y value
(define (outside-west-south-sides-bounding-limits? b-limit a-posn)
  (and (> (bounding-limits-min-x b-limit) (posn-x a-posn))
       (<= (posn-x a-posn) (bounding-limits-max-x b-limit))
       (<= (bounding-limits-min-y b-limit) (posn-y a-posn))
       (> (posn-y a-posn) (bounding-limits-max-y b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond the maximum X value, and is beyond the minimum Y value
(define (outside-east-north-sides-bounding-limits? b-limit a-posn)
  (and (<= (bounding-limits-min-x b-limit) (posn-x a-posn))
       (> (posn-x a-posn) (bounding-limits-max-x b-limit))
       (> (bounding-limits-min-y b-limit) (posn-y a-posn))
       (<= (posn-y a-posn) (bounding-limits-max-y b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond the maximum X value, and is beyond the maximum Y value
(define (outside-east-south-sides-bounding-limits? b-limit a-posn)
  (and (<= (bounding-limits-min-x b-limit) (posn-x a-posn))
       (> (posn-x a-posn) (bounding-limits-max-x b-limit))
       (<= (bounding-limits-min-y b-limit) (posn-y a-posn))
       (> (posn-y a-posn) (bounding-limits-max-y b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond either the minimum or maximum X value
(define (outside-x-axis-bounding-limits? b-limit a-posn)
  (or (> (bounding-limits-min-x b-limit) (posn-x a-posn))
           (> (posn-x a-posn) (bounding-limits-max-x b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond either the minimum or maximum Y value
(define (outside-y-axis-bounding-limits? b-limit a-posn)
  (or (> (bounding-limits-min-y b-limit) (posn-y a-posn))
           (> (posn-y a-posn) (bounding-limits-max-y b-limit))))

;; bounding-limits posn -> boolean
;; Purpose: Checks whether the given posn is beyond both: either the minimum or maximum X value, and either the minimum or maximum Y value
#;(define (outside-x-and-y-axis-bounding-limits? b-limit a-posn)
  (and (or (> (bounding-limits-min-x b-limit) (posn-x a-posn))
           (> (posn-x a-posn) (bounding-limits-max-x b-limit)))
       (or (> (bounding-limits-min-y b-limit) (posn-y a-posn))
           (> (posn-y a-posn) (bounding-limits-max-y b-limit)))))