#lang racket
(require csc151)
(require rackunit)

; +----------+-------------------------------------------------------
; | Time log |
; +----------+

#|
Ethan   2024-04-29 7:30pm-8:30pm  60 min  Worked on center triangle/squares/circle
Ethan   2024-04-29 8:45pm-9:30pm  45 min  Worked on color scheme variability in base/circle
Ethan   2024-04-29 12:30pm-1:15pm  45 min  Worked on shape variability in base/circle
Colin 2024-05-01 4:40pm-5:40pm 60 min Created center-piece procedure using the helper procedures and worked on trying
                       to condense code and remove redundancy.
Colin 2024-05-03 2:45pm-3:30pm 45 min Worked on writing textual description
Colin 2024-05-03 4:30pm-5:15pm 60 min Finished writing textual description
Ethan   2024-05-03 2:30pm-3:30pm  60 min  Worked on sketch and condensing circle/square code.
Adarsh  2024-05-03 9pm-9:30pm     30 min  Worked on creating the final output and the image bachground.
Ethan    2024-05-04 10:30AM-11:30am  60 min  Worked on adding some more variablilty with the canvas
                                              and the circle/squares.
Colin 2024-05-04 8:45pm-9:30pm 45 min Inputted some sample examples for our image-series procedure to ensure it was working correctly
Adarsh/Colin  2024-05-05 7am-8pm     60 min  Cleaned the code base, added styling, created documentation.
Ethan         2024-05-04 4:00pm-5:30pm  90 min  Included rotate-hue to be applied based on values of n.
|#

; +--------------+---------------------------------------------------
; | Design goals |
; +--------------+

#|
We are trying to recreate Serie V, nr 6 by Hilma af Klint. However, we want to
write a procedure, (image-series n width height), that takes three non-negative
integers as inputs and generates a width-by-height image that is similar to but
different from the original image. The same n/width/height triplet should always
give the same image, and the procedure should support values of n between 0 and 999, inclusive.
However, different values of n should give different variations whether that be different colors,
creating multiple center-pieces for the image, vertically flipping the center-piece,
creating a variation of the background, or a combination of these. Our code uses several helper procedures:
'base1' through 'base8' create house-like structures from basic geometric shapes. Then 'base-unit' selects
one of these structures based on the input 'n'. The 'base' function then further modifies the selected image,
potentially flipping it or adding additional elements based on the input 'n'. Also, the 'circle-unit'
function creates a layered circle image with dynamic colors that change based on 'n'. The 'circle' procedure
then arranges these circle images in varying configurations depending on 'n'. The 'canvas' function creates a
vertically stacked background of colored rectangles whose colors vary based on 'n'. Lastly, the 'center-piece' procedure
creates the main image by stacking a circle image above a base image. The 'image-series' function then overlays
this center piece onto the background canvas completing the image. Furthermore, the series of procedures uses
recursion, conditional statements, and color manipulation (through the 'color-mod' procedure) to create a distinct image each time
the parameters ('n', 'width', and 'height') change.
|#

;;; (base1 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image with various shapes and colors based on the input `n`, `width`, and `height`.

;;; --- BASE IMAGE ---
;;; The base image includes an equilateral triangle, a rectangle, and two squares. 
;;; The triangle, rectangle, squares are filled with a color based on `n`.
;;; Dimensions are proportional to `width`.

(define base1
  (lambda (n width height)
    (above (solid-equilateral-triangle (* 1/4 width) (rgb 60 0 200))
           (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb 255 0 0))
           (beside
            (solid-square (* 1/8 width) (rgb 255 200 0))
            (solid-square (* 1/8 width) (rgb 255 255 255)))
           (beside
            (solid-square (* 1/8 width) (rgb 254 254 254))
            (solid-square (* 1/8 width) (rgb 0 0 0)))
    (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb 60 1 220)))))


;;; (base2 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image with various shapes and colors based on the input `n`, `width`, and `height`.


(define base2
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb 30 0 100))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb 255 0 0))
             (beside
              (solid-square (* 1/8 width) (rgb 254 200 0))
              (solid-square (* 1/8 width) (rgb 254 254 254)))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb 30 0 120))
             (beside
              (solid-square (* 1/8 width) (rgb 255 255 255))
              (solid-square (* 1/8 width) (rgb 0 0 0)))))))


;;; (base3 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image with various shapes and colors based on the input `n`, `width`, and `height`.


(define base3
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb 60 0 200))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb 255 0 0))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb 60 0 220))
             (beside
              (solid-square (* 1/8 width) (rgb 255 200 0))
              (solid-square (* 1/8 width) (rgb 255 255 255)))
             (beside
              (solid-square (* 1/8 width) (rgb 255 255 255))
              (solid-square (* 1/8 width) (rgb 0 0 0)))))))

;;; (base4 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image with various shapes and colors based on the input `n`, `width`, and `height`.


(define base4
  (lambda (n width height)
    (let ([color-mod
           (lambda (digit)
             (remainder (* digit (+ 1 n)) 255))])
      (above (solid-equilateral-triangle (* 1/4 width) (rgb 60 0 200))
             (solid-rectangle (* 1/4 width) (* 1/200 width) (rgb 255 0 0))
             (beside
              (solid-circle (* 1/8 width) (rgb 255 200 0))
              (solid-circle (* 1/8 width) (rgb 255 255 255)))
             (beside
              (solid-circle (* 1/8 width) (rgb 255 255 255))
              (solid-circle (* 1/8 width) (rgb 0 0 0)))
             (solid-rectangle (* 1/4 width) (* 1/8 width) (rgb 60 0 220))))))

;;; (base5 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image by vertically flipping the image generated by the `base1` function with parameters `n`, `width`, and `height`.

(define base5
  (lambda (n width height)
    (vflip (base1 n width height))))

;;; (base6 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image by vertically flipping the image generated by the `base2` function with parameters `n`, `width`, and `height`.

(define base6
  (lambda (n width height)
    (vflip (base2 n width height))))


;;; (base7 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image by vertically flipping the image generated by the `base3` function with parameters `n`, `width`, and `height`.

(define base7
  (lambda (n width height)
    (vflip (base3 n width height))))

;;; (base8 n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image by vertically flipping the image generated by the `base4` function with parameters `n`, `width`, and `height`.

(define base8
  (lambda (n width height)
    (vflip (base4 n width height))))

;;; (circle-unit n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates an image with three concentric circles, each with colors based on the input `n`, `width`, and `height`.

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


;;; (sum-of-digits n) -> real?
;;;   n : real?
;;; Computes the sum of the digits of the input `n`.

(define sum-of-digits
  (lambda (n)
    (cond
      [(< n 10) n]
      [else (+ (remainder n 10)
               (sum-of-digits (quotient n 10)))])))

;;; (base-unit n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Selects a base image based on the sum of digits of the input `n`
;;; and creates the corresponding image with dimensions `width` and `height`.

(define base-unit
  (lambda (n width height)
    (cond
      [(<= (sum-of-digits n) 3) (base1 n width height)]
      [(<= (sum-of-digits n) 6) (base2 n width height)]
      [(<= (sum-of-digits n) 9) (base3 n width height)]
      [(<= (sum-of-digits n) 12) (base4 n width height)]
      [(<= (sum-of-digits n) 15) (base5 n width height)]
      [(<= (sum-of-digits n) 18) (base6 n width height)]
      [(<= (sum-of-digits n) 21) (base7 n width height)]
      [(<= (sum-of-digits n) 24) (base8 n width height)]
      [else (base1 n width height)])))
    

;;; (base n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a base image based on the input `n` with dimensions `width` and `height`.

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
        [else
         unit]))))

;;; (circle n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates an image of concentric circles based on the input `n` with dimensions `width` and `height`.

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


;;; (center-piece n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a series of images stacked vertically, a circle image followed by a base image based on the input `n`, dimensions `width`, `height`.

(define center-piece
  (lambda (n width height)
    (above (circle n width height)
           (base n width height))))

;;; (canvas n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a canvas image with four rectangles of different colors based on the input `n`, with dimensions `width` and `height`.

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

;;; (rotate-hue c angle) -> image?
;;;    img : image?
;;; angle: real?
;;; Takes in an image a adjust the hue value by a number of degree determined by "angle".

(define rotate-hue-p1
  (lambda (img angle)
    (hsv->rgb (hsv (if (> (+(hsv-hue (rgb->hsv img)) angle) 360)
                       (remainder (+(hsv-hue (rgb->hsv img)) angle) 360)
                       (+(hsv-hue (rgb->hsv img)) angle))  
                   (hsv-saturation (rgb->hsv img))
                   (hsv-value  (rgb->hsv img))))))

(define rotate-hue
  (lambda (img angle)
    (pixel-map (cut (rotate-hue-p1 <> angle)) img)))

; +-------------------+----------------------------------------------
; | Primary procedure |
; +-------------------+

;;; (image-series n width height) -> image?
;;;   n : non-negative-integer?
;;;   width : non-negative-integer?
;;;   height : non-negative-integer?
;;; Create an image of the given width and height that meets the design
;;; goals specified above.  Given the same `n`, always creates the same 
;;; image.  Given different values of `n`, creates different images.

(define image-series
  (lambda (n width height)
    (rotate-hue (overlay
                 (center-piece n width height)
                 (canvas n width height)) n)))

