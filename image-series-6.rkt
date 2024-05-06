#lang racket
(require csc151)
(require rackunit)


;;; image-series.rkt
;;;   An exploration of TITLE by Hilma Af Klint
;;;   A final project for CSC-151-NN 2024Sp
;;;
;;; Authors:
;;;   Ethan Versh
;;;   Adarsh Sharma
;;;   Colin Byrnes
;;;
;;; Date: 2024-05-05
;;;
;;; Acknowledgements
;;; 
;;; * The project template comes from SamR and Leah.

; +----------+-------------------------------------------------------
; | Time log |
; +----------+

#|
Ethan   2024-04-29 7:30pm-8:30pm  60 min  Worked on center triangle/squares/circle
Ethan   2024-04-29 8:45pm-9:30pm  45 min  Worked on color scheme variability in base/circle
Ethan   2024-04-29 12:30pm-1:15pm 45 min  Worked on shape variability in base/circle
Colin   2024-05-01 4:40pm-5:40pm  60 min  Created center-piece procedure, condensed code and removed redundancy.
Colin   2024-05-03 2:45pm-3:30pm  45 min  Worked on writing textual description
Colin   2024-05-03 4:30pm-5:15pm  60 min  Finished writing textual description
Ethan   2024-05-03 2:30pm-3:30pm  60 min  Worked on sketch and condensing circle/square code.
Adarsh  2024-05-03 9pm-9:30pm     30 min  Worked on creating the final output and the image bachground.
Ethan   2024-05-04 10:30-11:30am  60 min  Worked on adding some more variablilty with the canvas and the circle/squares.
Colin   2024-05-04 8:45pm-9:30pm 45 min Inputted some sample examples for our image-series procedure to ensure it was working correctly
Adarsh/Colin  2024-05-05 7am-8pm        60 min  Cleaned the code base, added styling, created documentation.
Ethan         2024-05-04 4:00pm-5:30pm  90 min  Included rotate-hue to be applied based on values of n.
Adarsh        2024-05-04 4:00pm-6pm    120 min  Added recursive functions to generate images.
Colin         2024-05-04 6:45pm-7:45pm  90 min  Edited design plans and make powerpoint presentation.
Adarsh        2024-05-04 8:00pm-11:00pm    180 min  Refactored the code base to smaller helper function and add more recursive calls.
Colin         2024-05-04 8:00pm-11:00pm    180 min  Worked on the design goals and presentation. 
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
one of these structures based on the input 'n'. The 'base' procedure then further modifies the selected image,
potentially flipping it or adding additional elements based on the input 'n'. Also, the 'circle-unit'
procedure creates a layered circle image with dynamic colors that change based on 'n' using the 'color-mod' procedure. The 'circle' procedure
then arranges these circle images in varying configurations depending on 'n'. The 'canvas' procedure creates a
vertically stacked background of colored rectangles using 'color-mod2', 'create-rectangles', and 'stack-rectangles' as helper procedures, with colors varying based on 'n' using 'color-mod2'.
Lastly, the 'center-piece' procedure creates the main image by stacking a circle image above a base image. The 'image-series' procedure then overlays
this center piece onto the background canvas, and the 'rotate-hue' procedure adjusts the overall hue of the final image based on 'n'.
Furthermore, the series of procedures use recursion, conditional statements, and color manipulation to create
a distinct image each time the parameters ('n', 'width', and 'height') change.
|#


#|

; +------------------------+-----------------------------------------
; | Requirements checklist |
; +------------------------+

What are two image-making techniques you've used?  Where do you
use them?

> Vertical Flipping
Used in functions Base5, Base6, Base7, Base8

> HSV Color Manipulation
Used in functions rotate-hue-p1 and rotate-hue

What are two instances of recursion in your program? 

> Recursion has being used in `sum-of-digits` (to calculate the sume of digits of 'n')
and `canvas` functions (to generate the final image by stacking figures in a list). 

Describe a piece of code that you are particularly proud of.


|#



; +-------------------+----------------------------------------------
; | Helper procedures |
; +-------------------+

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
        [else unit]))))

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
        [else unit]))))


;;; (center-piece n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a series of images stacked vertically, a circle image followed by a base image based on the input `n`, dimensions `width`, `height`.

(define center-piece
  (lambda (n width height)
    (above (circle n width height)
           (base n width height))))

;;; (create-rectangles colors width height) -> image?
;;;   colors : (listof color?)
;;;   width : real?
;;;   height : real?
;;; Creates a list of rectangles with colors based on the input `colors`, with dimensions `width` and `height`.

(define create-rectangles
  (lambda (colors width height)
    (if (null? colors)
        '()
        (cons (solid-rectangle (* width 1) (* height 0.3) (car colors))
              (create-rectangles (cdr colors) width height)))))

;;; (stack-rectangles rects) -> image?
;;;   rects : (listof image?)
;;; Stacks a list of rectangles vertically to create the final canvas image.

(define stack-rectangles
  (lambda (rects)
    (if (null? rects)
        (solid-rectangle 0 0 (rgb 0 0 0 0)) ; Empty rectangle
        (above (car rects) (stack-rectangles (cdr rects))))))


;;; (color-mod digit) -> rgb?
;;;   digit : real?
;;; Generates an RGB color based on the input `digit` by applying modular arithmetic.

(define color-mod2
  (lambda (digit)
    (rgb (remainder (* digit 3) 255)
         (remainder (* digit 5) 255)
         (remainder (* digit 7) 255))))

;;; (canvas n width height) -> image?
;;;   n : real?
;;;   width : real?
;;;   height : real?
;;; Creates a canvas image with four rectangles of different colors based on the input `n`, with dimensions `width` and `height`.

(define canvas
  (lambda (n width height)
    (stack-rectangles (create-rectangles (map color-mod2 '(244 254 254 244)) width height))))

;;; (rotate-hue-p1 img angle) -> image?
;;;   img : image?
;;;   angle : real?
;;; Rotates the hue of the input `img` by `angle` degrees.

(define rotate-hue-p1
  (lambda (img angle)
    (hsv->rgb (hsv (if (> (+(hsv-hue (rgb->hsv img)) angle) 360)
                       (remainder (+(hsv-hue (rgb->hsv img)) angle) 360)
                       (+(hsv-hue (rgb->hsv img)) angle))  
                   (hsv-saturation (rgb->hsv img))
                   (hsv-value  (rgb->hsv img))))))

;;; (rotate-hue c angle) -> image?
;;;    img : image?
;;; angle: real?
;;; Takes in an image a adjust the hue value by a number of degree determined by "angle".

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

