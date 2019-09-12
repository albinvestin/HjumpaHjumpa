#lang racket
(provide groundlevel%)
(provide platform%)

(define groundlevel%
  (class object%
    (init-field
     groundlevel-width ; ett par med startkoordinat och slutkoordinat
     groundlevel-height ; ett värde
     )
    
    ; Get funktioner
    
    (define/public (get-groundlevel-width)
      groundlevel-width)
    (define/public (get-groundlevel-height)
      groundlevel-height)
    
    (super-new)))


(define platform%
  (class object%
    (init-field
     platform-width
     platform-height) ; par med undre resp hövre höjd på plattformen
    
    ; get funktioner
    
    (define/public (get-platform-width)
      platform-width)
    (define/public (get-platform-height)
      platform-height)
    
    (super-new)))