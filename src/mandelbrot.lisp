(losh:eval-dammit
  (ql:quickload '(:trivial-ppm)))

(defpackage :sand.mandelbrot
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))


(in-package :sand.mandelbrot)


(defun-inline magnitude-squared (complex-number)
  (+ (square (realpart complex-number))
     (square (imagpart complex-number))))

(defun escapesp (x y)
  (iterate
    (for c = (complex x y))
    (for z :first c :then (+ (square z) c))
    (repeat 10)
    (thereis (>= (magnitude-squared z) 4))))

(defun cycles-to-escape (x y)
  (declare (optimize speed)
           (type single-float x y))
  (iterate
    (declare (iterate:declare-variables))
    (for (the (complex single-float) c) = (complex x y))
    (for (the (complex single-float) z) :first c :then (+ (square z) c))
    (for (the fixnum cycle) :from 0 :below 255)
    (finding cycle :such-that (>= (magnitude-squared z) 4))))

(defun coords (ix iy width height)
  (values (map-range 0 width -2.3 0.7 ix)
          (map-range 0 height -1.5 1.5 iy)))

(defun draw (width height &optional (filename "mandelbrot"))
  (let ((image (make-array (list width height))))
    (time (iterate (for (pixel ix iy) :in-array image)
                   (for (values x y) = (coords ix iy width height))
                   (for cycles = (cycles-to-escape x y))
                   (for color = (if (null cycles)
                                  0
                                  (- 255 cycles)))
                   (setf (aref image ix iy) color)))
    (trivial-ppm:write-to-file (format nil "~A.pgm" filename) image
                               :format :pgm
                               :encoding :binary
                               :if-exists :supersede)))
