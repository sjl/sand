(losh:eval-dammit
  (ql:quickload '(:trivial-ppm)))

(defpackage :sand.art.simple
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils))

(in-package :sand.art.simple)

;; http://www.tylerlhobbs.com/writings/triangle-subdivision

(defun v (x y)
  (make-array 2 :element-type 'single-float :initial-contents (list x y)))

(defun-inline x (v)
  (aref v 0))

(defun-inline y (v)
  (aref v 1))


(defun v+ (v1 v2)
  (v (+ (x v1) (x v2))
     (+ (y v1) (y v2))))

(defun v/ (v scalar)
  (v (/ (x v) scalar)
     (/ (y v) scalar)))


(defstruct (triangle (:conc-name ""))
  (a (v 0.0 0.0) :type (simple-array single-float (2)))
  (b (v 0.0 0.0) :type (simple-array single-float (2)))
  (c (v 0.0 0.0) :type (simple-array single-float (2))))

(defun triangle (a b c)
  (make-triangle :a a :b b :c c))


(defun image-coords (image point)
  (destructuring-bind (width height) (array-dimensions image)
    (values (truncate (* (x point) (1- width)))
            (truncate (* (y point) (1- height))))))

(defun draw-vertical-line (image p1 p2)
  (if (< (y p2) (y p1))
    (draw-vertical-line image p2 p1)
    (nest
      (multiple-value-bind (x1 y1) (image-coords image p1))
      (multiple-value-bind (x2 y2) (image-coords image p2))
      (iterate (for y :from y1 :below y2)
               (for x = (floor (map-range y1 y2 x1 x2 y)))
               (setf (aref image x y) 0)))))

(defun draw-horizontal-line (image p1 p2)
  (if (< (x p2) (x p1))
    (draw-horizontal-line image p2 p1)
    (nest
      (multiple-value-bind (x1 y1) (image-coords image p1))
      (multiple-value-bind (x2 y2) (image-coords image p2))
      (iterate (for x :from x1 :below x2)
               (for y = (floor (map-range x1 x2 y1 y2 x)))
               (setf (aref image x y) 0)))))

(defun slope (v1 v2)
  (let ((run (- (x v2) (x v1)))
        (rise (- (y v2) (y v1))))
    (/ rise run)))

(defun draw-line (image p1 p2)
  (if (or (= (x p1) (x p2))
          (> (abs (slope p1 p2)) 1))
    (if (= (y p1) (y p2))
      nil
      (draw-vertical-line image p1 p2))
    (draw-horizontal-line image p1 p2)))

(defun draw-triangle (image triangle)
    (draw-line image (a triangle) (b triangle))
    (draw-line image (b triangle) (c triangle))
    (draw-line image (c triangle) (a triangle)))

(defun split-triangle (triangle)
  (let* ((a (a triangle))
         (b (b triangle))
         (c (c triangle))
         (n (clamp 0.2 0.8 (random-gaussian 0.5 0.05)))
         (p (v (lerp (x b) (x c) n)
               (lerp (y b) (y c) n))))
    (list (triangle p b a)
          (triangle p a c))))


(defun draw (width height)
  (let ((image (make-array (list width height) :initial-element 255)))
    (recursively ((triangle (triangle (v 0.05 0.05)
                                      (v 0.05 0.95)
                                      (v 0.95 0.05)))
                  (depth 6))
      (if (zerop depth)
        (draw-triangle image triangle)
        (dolist (tri (split-triangle triangle))
          (recur tri (1- depth)))))
    (trivial-ppm:write-to-file "triangles.pgm" image
                               :format :pgm
                               :if-exists :supersede)))
