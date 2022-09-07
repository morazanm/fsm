#lang racket
(require "../../FSM-Visualization/graphViz/main.rkt"
         "../test-helpers.rkt"
         2htdp/image)

(define (make-mock-img w h)
  (rectangle w h "solid" "white"))

(module+ test
  (require rackunit)

  (define img-resize-tests
    (test-suite "Makes sure that images are properly resized"
           (test-case "Rectangle width and height are equal and less then max"
                      (define new-image (resize-image (make-mock-img 100 100) 1000 1000))
                      (check-equal? (image-width new-image) 1000)
                      (check-equal? (image-height new-image) 1000))
           
           (test-case "Rectangle width and height are not equal and are less then max"
                      (define new-image (resize-image (make-mock-img 400 200) 800 800))
                      (check-equal? (image-width new-image) 800)
                      (check-equal? (image-height new-image) 400))
           
           (test-case "Rectangle width and height are equal to max"
                      (define new-image (resize-image (make-mock-img 800 800) 800 800))
                      (check-equal? (image-width new-image) 800)
                      (check-equal? (image-height new-image) 800))
           
           (test-case "Rectangle width and height are equal and greater then max"
                      (define new-image (resize-image (make-mock-img 1000 1000) 500 500))
                      (check-equal? (image-width new-image) 500)
                      (check-equal? (image-height new-image) 500))
           
           (test-case "Rectangle width and height are not equal and greater then max"
                      (define new-image (resize-image (make-mock-img 1000 500) 500 500))
                      (check-equal? (image-width new-image) 500)
                      (check-equal? (image-height new-image) 250))))


  (test-all 'verbose
            (img-resize-tests))
  ) ; end module+ test
