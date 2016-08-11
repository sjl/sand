(in-package #:sand.terrain.diamond-square)



(defvar *size* nil)

(defun heightmap-size (heightmap)
  (first (array-dimensions heightmap)))

(defun hm-ref (heightmap x y)
  (flet ((ref (n)
           (cond ((< -1 n *size*) n)
                 (t (mod n *size*)))))
    (aref heightmap (ref x) (ref y))))


(defun heightmap-extrema (heightmap)
  (iterate
    (for v :across-flat-array heightmap :with-index i)
    (maximize v :into max)
    (minimize v :into min)
    (finally (return (values min max)))))

(defun normalize-heightmap (heightmap)
  (multiple-value-bind (min max) (heightmap-extrema heightmap)
    (do-array (v heightmap)
      (setf v (norm min max v)))))


(defun ds-init (heightmap)
  (let ((last (1- *size*)))
    (setf
      (aref heightmap 0 0) 0.5
      (aref heightmap 0 last) 0.5
      (aref heightmap last 0) 0.5
      (aref heightmap last last) 0.5)))


(defun ds-square (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) (- y radius))
                                 (hm-ref heightmap (- x radius) (+ y radius))
                                 (hm-ref heightmap (+ x radius) (- y radius))
                                 (hm-ref heightmap (+ x radius) (+ y radius)))
                       spread)))

(defun ds-diamond (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average4 (hm-ref heightmap (- x radius) y)
                                 (hm-ref heightmap (+ x radius) y)
                                 (hm-ref heightmap x (- y radius))
                                 (hm-ref heightmap x (+ y radius)))
                       spread)))


(defun ds-squares (heightmap radius spread)
  (iterate
    (for x :from radius :below *size* :by (* 2 radius))
    (iterate
      (for y :from radius :below *size* :by (* 2 radius))
      (ds-square heightmap x y radius spread))))

(defun ds-diamonds (heightmap radius spread)
  (iterate
    (for i :from 0)
    (for y :from 0 :below *size* :by radius)
    (iterate
      (with shift = (if (evenp i) radius 0))
      (for x :from shift :below *size* :by (* 2 radius))
      (ds-diamond heightmap x y radius spread))))


(defun diamond-square (heightmap)
  (ds-init heightmap)
  (let ((*size* (heightmap-size heightmap))
        (spread 0.8)
        (spread-reduction 0.7))
    (recursively ((radius (floor size 2))
                  (spread spread))
      (when (>= radius 1)
        (ds-squares heightmap radius spread)
        (ds-diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction)))))
  (normalize-heightmap heightmap)
  heightmap)

