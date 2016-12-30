(in-package :sand.mandelbrot)

;;;; Config
(defparameter *width* 100)
(defparameter *height* 100)
(defparameter *black-pen* (make-pen :fill (rgb 0 0 0)))


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
(defun magnitude² (complex-number)
  (+ (square (realpart complex-number))
     (square (imagpart complex-number))))

(defun plot (x y)
  (with-pen *black-pen*
    (in-context
      (translate x y)
      (rect 0 0 1 1))))

(defun escapesp (x y)
  (iterate
    (for c = (complex x y))
    (for z :first c :then (+ (square z) c))
    (repeat 10)
    (thereis (>= (magnitude² z) 4))))

(defun screen-to-coord (ox oy sx sy)
  (values (- sx ox)
          (- sy oy)))


(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Mandelbrot")
     (copy-pixels t)
     (mouse (list 0 0))
     (mouse-down-left nil)
     (mouse-down-right nil)
     (dirty t)
     ;; Data
     (size 2.0d0)
     (ox 0)
     (oy 0)
     )
  ;;
  (just-once dirty
    (with-setup
      (in-context
        (translate (/ *width* 2)
                   (/ *height* 2))
        (iterate
          (with scale = (/ size (/ *width* 2.0d0)))
          (for-nested ((sx :from (- (/ *width* 2)) :below (/ *width* 2))
                       (sy :from (- (/ *height* 2)) :below (/ *height* 2))))
          (for x = (* scale sx))
          (for y = (* scale sy))
          (when (not (escapesp x y))
            (plot sx sy))))))
  ;;

  )


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-up (mulf (slot-value instance 'size) 1.1))
    (:scancode-down (mulf (slot-value instance 'size) 0.9))
    )
  (setf (slot-value instance 'dirty) t))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'demo))
