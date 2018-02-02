(losh:eval-dammit
  (ql:quickload '(:trivial-ppm)))

(defpackage :sand.art.simple
  (:use :cl :losh :iterate :sand.quickutils))

(in-package :sand.art.simple)

;; http://www.tylerlhobbs.com/writings/triangle-subdivision

(defun v (x y)
  (make-array 2 :element-type 'real :initial-contents (list x y)))

(defun-inline x (v)
  (aref v 0))

(defun-inline y (v)
  (aref v 1))


(defun v+ (v1 v2)
  (v (+ (x v1) (x v2))
     (+ (y v1) (y v2))))

(defun v- (v1 v2)
  (v (- (x v1) (x v2))
     (- (y v1) (y v2))))

(defun v/ (v scalar)
  (v (/ (x v) scalar)
     (/ (y v) scalar)))

(defun vm2 (v)
  (+ (square (x v))
     (square (y v))))


(defstruct (triangle (:conc-name ""))
  (a (v 0.0 0.0) :type (simple-array real (2)))
  (b (v 0.0 0.0) :type (simple-array real (2)))
  (c (v 0.0 0.0) :type (simple-array real (2))))

(define-with-macro (triangle :conc-name "") a b c)

(defun triangle (a b c)
  (make-triangle :a a :b b :c c))


(defun-inline blend (old-color new-color transparency)
  (round (map-range 255 0
                    old-color new-color
                    transparency)))

(defun-inline draw-pixel (image x y color)
  (zapf (aref image x y) (blend % 0 color)))

(defun draw-vertical-line (image x y1 y2)
  (iterate
    (with x2 = (if (zerop x) 1 (1- x)))
    (for y :from (min y1 y2) :to (max y1 y2))
    (draw-pixel image x y 10)
    (draw-pixel image x2 y (- 255 20))))

(defun draw-horizontal-line (image x1 x2 y)
  (iterate
    (with y2 = (if (zerop y) 1 (1- y)))
    (for x :from (min x1 x2) :to (max x1 x2))
    (draw-pixel image x y 10)
    (draw-pixel image x y2 (- 255 20))))

(defun draw-diagonal-line (image x1 y1 x2 y2)
  (when (> x1 x2)
    (rotatef x1 x2)
    (rotatef y1 y2))
  (iterate
    (with last = (1- (array-dimension image 1)))
    (with dy = (if (> y2 y1) 1 -1))
    (for x :from x1 :to x2)
    (for y :from y1 :by dy)
    (draw-pixel image x y 10)
    (unless (zerop y)
      (draw-pixel image x (- y 1) (- 255 50)))
    (unless (= last y)
      (draw-pixel image x (+ y 1) (- 255 50)))
    ))


(defconstant +wu-bits+ 16)
(defconstant +color-bits+ 8)

(deftype wu-unsigned ()
  `(unsigned-byte ,+wu-bits+))

(deftype wu-signed ()
  `(signed-byte 32))

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


(defun wu-draw-line (image x1 y1 x2 y2)
  (let ((dx (abs (- x2 x1)))
        (dy (abs (- y2 y1))))
    (cond ((zerop dx) (draw-vertical-line image x1 y1 y2))
          ((zerop dy) (draw-horizontal-line image x1 x2 y1))
          ((= dy dx) (draw-diagonal-line image x1 y1 x2 y2))
          ((> dy dx) (if (> y1 y2)
                       (wu-vertical image x2 y2 x1 y1)
                       (wu-vertical image x1 y1 x2 y2)))
          (t (if (> x1 x2)
               (wu-horizontal image x2 y2 x1 y1)
               (wu-horizontal image x1 y1 x2 y2))))))



(defun image-coords (image point)
  (destructuring-bind (width height) (array-dimensions image)
    (values (round (* (x point) (1- width)))
            (round (* (y point) (1- height))))))

(defun draw-line (image p1 p2)
  (multiple-value-bind (x1 y1) (image-coords image p1)
    (multiple-value-bind (x2 y2) (image-coords image p2)
      (wu-draw-line image x1 y1 x2 y2))))


(defun line< (p1 p2)
  (let ((x1 (aref p1 0))
        (y1 (aref p1 1))
        (x2 (aref p2 0))
        (y2 (aref p2 1)))
    (cond
      ((< x1 x2) t)
      ((= x1 x2) (< y1 y2)))))

(defun canonical-line (a b)
  (sort (list a b) #'line<))

(defun triangle-lines (triangle)
  (with-triangle (triangle)
    (list (canonical-line a b)
          (canonical-line b c)
          (canonical-line c a))))

(defun draw-triangles (image triangles)
  (map nil (lambda (line)
             (destructuring-bind (a b) line
               (draw-line image a b)))
       (remove-duplicates (mappend #'triangle-lines triangles)
                          :test #'equalp)))

(defun draw-triangle (image triangle)
  (with-triangle (triangle)
    (draw-line image a b)
    (draw-line image b c)
    (draw-line image c a)))

(defun round-to (number divisor)
  (* divisor (round number divisor)))

(defun split-triangle-evenly (triangle)
  (with-triangle (triangle)
    (let* ((n 1/2)
           (p (v (lerp (x b) (x c) n)
                 (lerp (y b) (y c) n))))
      (list (triangle p b a)
            (triangle p a c)))))

(defun split-triangle-randomly (triangle)
  (with-triangle (triangle)
    (let* ((n (-<> (random-gaussian 0.5 0.2)
                (clamp 0.1 0.9 <>)
                (round-to <> 1/100)))
           (p (v (lerp (x b) (x c) n)
                 (lerp (y b) (y c) n))))
      (list (triangle p b a)
            (triangle p a c)))))


(defun find-longest-side (triangle)
  (with-triangle (triangle)
    (let* ((ab (vm2 (v- a b)))
           (bc (vm2 (v- b c)))
           (ca (vm2 (v- c a)))
           (longest (max ab bc ca)))
      (cond
        ((= longest ab) (list c a b))
        ((= longest bc) (list a c b))
        ((= longest ca) (list b c a))
        (t (error "what?"))))))

(defun split-triangle-self-balancing (triangle)
  (destructuring-bind (a b c) (find-longest-side triangle)
    (let* ((n (-<> (random-gaussian 0.5 0.15)
                (clamp 0.3 0.7 <>)
                (round-to <> 1/100)))
           (p (v (lerp (x b) (x c) n)
                 (lerp (y b) (y c) n))))
      (list (triangle p b a)
            (triangle p a c)))))


(defun draw (width height &optional (depth 2))
  (let* ((image (make-array (list width height) :initial-element 255))
         (pad 1/100)
         (z pad)
         (o (- 1 pad))
         (initial (list (triangle (v z o)
                                  (v o o)
                                  (v z z))
                        (triangle (v o z)
                                  (v o o)
                                  (v z z)))))
    ;; (recursively ((depth depth)
    ;;               (triangles initial))
    ;;   (if (or (zerop depth) (randomp 0.05))
    ;;     (draw-triangles image triangles)
    ;;     (recur (1- depth) (mappend #'split-triangle-self-balancing triangles))))
    (labels ((recur (d triangle)
               (if (or (zerop d) (randomp (map-range depth 0
                                                     0.0 0.07
                                                     d)))
                 (draw-triangle image triangle)
                 (map nil (curry #'recur (1- d)) (split-triangle-self-balancing triangle)))))
      (map nil (curry #'recur depth) initial))
    (trivial-ppm:write-to-file "image.pgm" image
                               :format :pgm
                               :if-exists :supersede)))
