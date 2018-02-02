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


(defun-inline draw-pixel (image x y color)
  (setf (aref image x y) color))

(defun draw-vertical-line (image x y1 y2)
  (iterate (for y :from (min y1 y2) :to (max y1 y2))
           (draw-pixel image x y 0)))

(defun draw-horizontal-line (image x1 x2 y)
  (iterate (for x :from (min x1 x2) :to (max x1 x2))
           (draw-pixel image x y 0)))


(defconstant +wu-bits+ 16)
(defconstant +color-bits+ 8)

(deftype wu-unsigned ()
  `(unsigned-byte ,+wu-bits+))

(deftype wu-signed ()
  `(signed-byte ,(1+ +wu-bits+)))

(defun-inline wu+ (value increment)
  (multiple-value-bind (overflow result)
      (floor (+ value increment)
             (expt 2 +wu-bits+))
    (values result
            (not (zerop overflow)))))

(defmacro define-wu (name vertical?)
  (destructuring-bind (main1 main2 aux1 aux2)
      (if vertical?
        '(y1 y2 x1 x2)
        '(x1 x2 y1 y2))
    `(defun ,name (image x1 y1 x2 y2)
       (check-type image (simple-array t (* *)))
       (check-type x1 (integer 0 100000))
       (check-type y1 (integer 0 100000))
       (check-type x2 (integer 0 100000))
       (check-type y2 (integer 0 100000))
       (iterate
         (declare (optimize speed)
                  (iterate:declare-variables)
                  (type wu-unsigned d)
                  (type wu-signed d-step))
         (with slope = (/ (- ,aux2 ,aux1)
                          (- ,main2 ,main1)))
         (with d = 0)
         (with d-step = (floor (+ (* slope (expt 2 +wu-bits+)) 1/2)))
         (with aux-step = (if (minusp slope) -1 1))
         (initially (draw-pixel image x1 y1 0)
                    (draw-pixel image x2 y2 0))
         (incf ,main1)
         (decf ,main2)
         (until (> ,main1 ,main2))
         (multiple-value-bind (new-d overflow) (wu+ d d-step)
           (setf d new-d)
           (when overflow
             (incf ,aux1 aux-step)
             (decf ,aux2 aux-step)))
         (for ca = (truncate d (expt 2 (- +wu-bits+ +color-bits+))))
         (for cb = (- (expt 2 +color-bits+) 1 ca))
         ,@(if vertical?
             '((draw-pixel image x1 y1 ca)
               (draw-pixel image (1+ x1) y1 cb)
               (draw-pixel image x2 y2 ca)
               (draw-pixel image (1- x2) y2 cb))
             '((draw-pixel image x1 y1 ca)
               (draw-pixel image x1 (1+ y1) cb)
               (draw-pixel image x2 y2 ca)
               (draw-pixel image x2 (1- y2) cb)))))))

(define-wu wu-horizontal nil)
(define-wu wu-vertical t)

(defun draw-line (image x1 y1 x2 y2)
  (let ((dx (abs (- x2 x1)))
        (dy (abs (- y2 y1))))
    (cond ((zerop dx) (draw-vertical-line image x1 y1 y2))
          ((zerop dy) (draw-horizontal-line image x1 x2 y1))
          ((> dy dx) (if (> y1 y2)
                       (wu-vertical image x2 y2 x1 y1)
                       (wu-vertical image x1 y1 x2 y2)))
          (t (if (> x1 x2)
               (wu-horizontal image x2 y2 x1 y1)
               (wu-horizontal image x1 y1 x2 y2))))))

(defun image-coords (image point)
  (destructuring-bind (width height) (array-dimensions image)
    (values (truncate (* (x point) (1- width)))
            (truncate (* (y point) (1- height))))))

(defun slope (v1 v2)
  (let ((run (- (x v2) (x v1)))
        (rise (- (y v2) (y v1))))
    (/ rise run)))


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
    (draw-line image 0 10 50 6)
    (draw-line image 0 20 50 25)
    (draw-line image 10 30 15 45)
    (time (iterate (repeat 100000)
                   (draw-line image 0 0 (1- width) (- height 5))))
    (trivial-ppm:write-to-file "image.pgm" image
                               :format :pgm
                               :if-exists :supersede)))
