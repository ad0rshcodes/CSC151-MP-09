#lang racket
(require csc151)

; +----------+-------------------------------------------------------
; | Time log |
; +----------+

#|
Ethan   2024-04-29 7:30pm-8:30pm  60 min  Worked on center triangle/squares/circle
NAME    YYYY-MM-DD HH:MM-HH:MM  NN min  ACTIVITY
|#

(define base
  (lambda (n width height)
    (above (solid-equilateral-triangle (* 1/4 width) (rgb 30 0 100 100))
           (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb 255 0 0))
           (beside
            (solid-square (* 1/8 width) (rgb 225 200 0 150))
            (solid-square (* 1/8 width) (rgb 255 255 255)))
            (beside
            (solid-square (* 1/8 width) (rgb 255 255 255))
            (solid-square (* 1/8 width) (rgb 0 0 0)))
            (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb 30 0 120 100)))))

(define circle
  (lambda (n width height)
    (overlay
    (solid-circle (* 1/50 width) (rgb 125 75 0))
    (outlined-circle (* 5/16 width) (rgb 125 75 0) (* 1/200 width))
    (solid-circle (* 5/16 width) (rgb 225 175 0)))))


;(circle 0 500 500)
;(base 0 500 500)
;(rgb 225 175 0)
