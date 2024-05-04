#lang racket
(require csc151)

; +----------+-------------------------------------------------------
; | Time log |
; +----------+

#|
Ethan   2024-04-29 7:30pm-8:30pm  60 min  Worked on center triangle/squares/circle
Ethan   2024-04-29 8:45pm-9:30pm  45 min  Worked on color scheme variability in base/circle
Ethan   2024-04-29 12:30pm-1:30pm  30 min  Worked on shape variability in base/circle
Colin 2024-05-01 4:35-5:40 Created image-series procedure using the helper procedures and worked on trying
                       to condense code and remove redundancy.
Ethan   2024-04-29 2:30pm-3:30pm  60 min  Worked on sketch and condensing circle/square code.
Adarsh  2024-05-03 9pm-9:30pm     30 min  Worked on creating the final output and the image bachground.
Ethan    2024-05-04 10:30 AM -11:15am  45 min  Worked on adding some more variablilty with the canvas
                                              and the circle/squares.
|#


(define base1
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 60)
                                                            (color-mod 1)
                                                            (color-mod 200)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)))
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-square (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 60)
                                                               (color-mod 1)
                                                               (color-mod 220)))))))

(define base2
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)))
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-square (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))))))

(define base3
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 60)
                                                            (color-mod 1)
                                                            (color-mod 200)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 60)
                                                               (color-mod 1)
                                                               (color-mod 220)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)))
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-square (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))))))
(define base4
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 60)
                                                            (color-mod 1)
                                                            (color-mod 200)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)))
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-circle (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 60)
                                                               (color-mod 1)
                                                               (color-mod 220)))))))

(define base5
  (lambda (n width height)
    (vflip (base1 n width height))))

(define base6
  (lambda (n width height)
    (vflip (base2 n width height))))

(define base7
  (lambda (n width height)
    (vflip (base3 n width height))))

(define base8
  (lambda (n width height)
    (vflip (base4 n width height))))


(define circle-unit
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (overlay
       (solid-circle (* 1/50 width) (rgb (color-mod 125)
                                         (color-mod 75)
                                         (color-mod 1)))
       (outlined-circle (* 5/16 width) (rgb (color-mod 125)
                                            (color-mod 75)
                                            (color-mod 1))
                        (* 1/200 width))
       (solid-circle (* 5/16 width) (rgb (color-mod 225)
                                         (color-mod 175)
                                         (color-mod 1)))))))

(define base-unit
  (lambda (n width height)
    (cond
      [(and (<= 0 n) (> 200 n))
       (base1 n width height)]
      [(and (<= 200 n) (> 300 n))
       (base2 n width height)]
      [(and (<= 300 n) (> 400 n))
       (base3 n width height)]
      [(and (<= 400 n) (> 500 n))
       (base4 n width height)]
      [(and (<= 500 n) (> 600 n))
       (base5 n width height)]
      [(and (<= 600 n) (> 700 n))
       (base6 n width height)]
      [(and (<= 700 n) (> 800 n))
       (base7 n width height)]
      [(and (<= 800 n) (> 900 n))
       (base8 n width height)]
      [(and (<= 900 n) (> 999 n))])))
       
(define base
  (lambda (n width height)
    (let ([unit (base-unit n width height)])
      (cond
        [(zero? n)
         unit]
        [(zero? (remainder n 7))
         (beside unit
                 (solid-square (* 1/8 width) (rgb 0 0 0 0))
                 unit)]
        [(zero? (remainder n 6))
         (beside unit
                 (solid-square (* 1/8 width) (rgb 0 0 0 0))
                 (vflip unit))]
        [(zero? (remainder n 5))
         (beside unit
                 (solid-square (* 1/16 width) (rgb 0 0 0 0))
                 (vflip unit)
                 (solid-square( * 1/16 width) (rgb 0 0 0 0))
                 unit)]
        [else
         unit]))))

(define circle
  (lambda (n width height)
    (let ([unit (circle-unit n width height)]
          [spacer (solid-square
                   (* 1/10 width)
                   (rgb 0 0 0 0))])
      (cond
        [(zero? n)
         unit]
        [(zero? (remainder n 9))
         (beside unit spacer unit)]
        [(zero? (remainder n 4))
         (beside (scale unit .5)
                 spacer
                 (scale unit .5)
                 spacer
                 (scale unit .5))]
        [else
         unit]))))


;(color-multiplier 255 0)
(define image-series
  (lambda (n width height)
    (above (circle n width height)
           (base n width height))))
;(rgb 225 175 0)


(define canvas
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
    (above
     (solid-rectangle (* width 1) (* height 0.3) (rgb (color-mod 244)
                                                      (color-mod 244)
                                                      (color-mod 220)))
     (solid-rectangle (* width 1) (* height 0.3) (rgb (color-mod 254)
                                                      (color-mod 191)
                                                      (color-mod 202)))
     (solid-rectangle (* width 1) (* height 0.3) (rgb (color-mod 254)
                                                      (color-mod 0)
                                                      (color-mod 0)))
     (solid-rectangle (* width 1) (* height 0.3) (rgb (color-mod 244)
                                                      (color-mod 244)
                                                      (color-mod 220)))))))
    

(define full-image
  (lambda (n width height)
    (overlay
     (image-series n width height)
     (canvas n width height))))

(full-image 342 300 300)