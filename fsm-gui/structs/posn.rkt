#lang racket

;; ***** posn.rk *****
;; This file holds the functionality for a posn structure

;; export necessary functions/structures
(provide
 (struct-out posn))

;; posn: A structutre that represents a position
;; - x: The x coordinate of the position
;; y -: The y coordinate of the position
(struct posn (x y))