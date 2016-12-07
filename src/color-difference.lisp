(in-package :sand.color-difference)

; https://en.wikipedia.org/wiki/Color_difference

(defparameter *c1* (make-cie-rgb-color 0.0 1.0 1.0))
(defparameter *c2* (make-cie-rgb-color 0.1 1.0 1.0))
(defparameter *c3* (make-cie-rgb-color 0.7 1.0 1.0))


(defun cie-76-distance (c1 c2)
  (multiple-value-bind (l1 a1 b1) (cie-lab-color-coordinates c1)
    (multiple-value-bind (l2 a2 b2) (cie-lab-color-coordinates c2)
      (sqrt (+ (square (- l2 l1))
               (square (- a2 a1))
               (square (- b2 b1)))))))


; (cie-76-distance *c1* *c2*)

; (defparameter *c* )

; (cie-lab-color-coordinates (make-cie-xyz-color 0 0 0))
