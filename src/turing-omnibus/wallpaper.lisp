(in-package :sand.turing-omnibus.wallpaper)

;;;; From The New Turing Omnibus, Chapter 1

;;;; Config
(defparameter *width* 600)
(defparameter *height* 600)


;;;; Utils
(defmacro with-setup (&body body)
  `(progn
    (background (gray 1.0))
    ,@body))

(defmacro in-context (&body body)
  `(prog1
    (push-matrix)
    (progn ,@body)
    (pop-matrix)))

(defmacro scancode-case (scancode-form &rest pairs)
  (with-gensyms (scancode)
    `(let ((,scancode ,scancode-form))
      (cond
        ,@(mapcar (lambda (pair)
                    (destructuring-bind (key-scancode &rest body) pair
                      `((sdl2:scancode= ,scancode ,key-scancode)
                        ,@body)))
           pairs)))))

(defmacro just-once (dirty-place &body body)
  `(when ,dirty-place
     (setf ,dirty-place nil)
     ,@body))


;;;; Sketch
(defun plot (x y color)
  (with-pen (make-pen :fill color)
    (in-context
      (translate x y)
      (rect 0 0 1 1))))

(defsketch wallpaper
    ((width *width*) (height *height*) (y-axis :up) (title "Wallpaper")
     (copy-pixels t)
     (mouse (list 0 0))
     (mouse-down-left nil)
     (mouse-down-right nil)
     (dirty t)
     ;; Data
     (palette (iterate (repeat (random-range 2 10))
                       (collect (rgb (random 1.0) (random 1.0) (random 1.0))
                                :result-type 'vector)))
     (corner-a (random 100))
     (corner-b (random 100))
     (side (random-range 10.0 20.0))
     (tiles (random-range 40 110))
     (number-of-colors (length palette)))
  ;;
  (just-once dirty
    (with-setup
      (in-context
        (scale (/ *width* tiles))
        (iterate
          (for-nested ((i :from 0 :below tiles)
                       (j :from 0 :below tiles)))
          (for x = (+ corner-a (* i (/ side tiles))))
          (for y = (+ corner-b (* j (/ side tiles))))
          (for c = (truncate (+ (* x x) (* y y))))
          (plot i j (aref palette (mod c number-of-colors)))))
      (with-pen (make-pen :fill (rgb 1.0 1.0 1.0))
        (rect 0 0 110 20))
      (text (format nil "Side: ~8F" side) 0 0)))
  ;;

  )


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-up (mulf (slot-value instance 'side) 1.01))
    (:scancode-down (mulf (slot-value instance 'side) 0.99))
    (:scancode-l (decf (slot-value instance 'corner-a)))
    (:scancode-h (incf (slot-value instance 'corner-a)))
    (:scancode-k (decf (slot-value instance 'corner-b)))
    (:scancode-j (incf (slot-value instance 'corner-b)))
    )
  (setf (slot-value instance 'dirty) t))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event ((instance wallpaper) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *wallpaper* (make-instance 'wallpaper))
