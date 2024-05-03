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
NAME    YYYY-MM-DD HH:MM-HH:MM  NN min  ACTIVITY
|#



(define base1
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
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
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))))))

(define base2
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))
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
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))
             (beside
              (solid-square (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
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
    (vflip (base1 n width height))))

(define base5
  (lambda (n width height)
    (vflip (base2 n width height))))

(define base6
  (lambda (n width height)
    (vflip (base3 n width height))))

(define base7
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
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
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))))))
(define base8
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-circle (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))))))

(define base9
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb (color-mod 30)
                                                            (color-mod 1)
                                                            (color-mod 100)
                                                            (color-mod 100)))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb (color-mod 254)
                                                                 (color-mod 1)
                                                                 (color-mod 1)))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb (color-mod 30)
                                                               (color-mod 1)
                                                               (color-mod 120)
                                                               (color-mod 100)))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 200)
                                               (color-mod 1)
                                               (color-mod 150)))
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254))))
             (beside
              (solid-circle (* 1/8 width) (rgb (color-mod 254)
                                               (color-mod 254)
                                               (color-mod 254)))
              (solid-circle (* 1/8 width) (rgb (color-mod 1)
                                               (color-mod 1)
                                               (color-mod 1))))))))

(define base10
  (lambda (n width height)
    (vflip (base7 n width height))))



(define circle
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





(define base
  (lambda (n width height)
    (cond
      [(and (<= 0 n) (> 100 n))
       (base1 n width height)]
      [(and (<= 100 n) (> 200 n))
       (base2 n width height)]
      [(and (<= 200 n) (> 300 n))
       (base3 n width height)]
      [(and (<= 300 n) (> 400 n))
       (base4 n width height)]
      [(and (<= 400 n) (> 500 n))
       (base5 n width height)]
      [(and (<= 500 n) (> 600 n))
       (base6 n width height)]
      [(and (<= 600 n) (> 700 n))
       (base7 n width height)]
      [(and (<= 700 n) (> 800 n))
       (base8 n width height)]
      [(and (<= 800 n) (> 900 n))
       (base9 n width height)]
      [(and (<= 900 n) (> 999 n))
       (base10 n width height)])))


;(color-multiplier 255 0)
(define image-series
  (lambda (n width height)
    (above (circle n width height)
           (base n width height))))
    ;(rgb 225 175 0)