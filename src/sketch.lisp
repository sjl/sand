(in-package #:sand.sketch)

;;;; Config
(setf *bypass-cache* t)
(defparameter *width* 600)
(defparameter *height* 400)


(defparameter *center-x* (/ *width* 2))
(defparameter *center-y* (/ *height* 2))


(defvar *shift* nil)
(defvar *control* nil)
(defvar *command* nil)
(defvar *option* nil)


;;;; Utils
(defmacro with-setup (&body body)
  `(progn
    (background (gray 1))
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


;;;; Box

(defparameter *world-exponent* 4)
(defparameter *world-size* (expt 2 *world-exponent*))

(defun allocate-heightmap (size)
  (make-array (list size size)
    :element-type 'single-float
    :initial-element 0.0
    :adjustable nil))

(defun normalize-heightmap (heightmap)
  (iterate
    (for i :from 0 :below (array-total-size heightmap))
    (for v = (row-major-aref heightmap i))
    (maximize v :into max)
    (minimize v :into min)
    (finally
      (iterate
        (with span = (- max min))
        (for i :from 0 :below (array-total-size heightmap))
        (for v = (row-major-aref heightmap i))
        (setf (row-major-aref heightmap i)
              (/ (- v min) span)))
      (return heightmap))))


(defun draw-hm (hm ox oy ts)
  (let ((size (- (hm-size hm) 0)))
    (in-context
      (translate (* ox (* ts size))
                 (* oy (* ts size)))
      (iterate
        (for x :from 0 :below size)
        (iterate
          (for y :from 0 :below size)
          (for h = (aref hm x y))
          (with-pen (make-pen :fill (if (<= 0.0 h 1.0)
                                      (gray h)
                                      (rgb 1.0 0 0)))
            (rect (* x ts) (* y ts)
                  ts ts))))
      (with-pen (make-pen :fill nil :stroke (rgb 1.0 0 0))
        ; (rect 0 0 (* ts size) (* ts size))
        ))))


(defmacro just-once (done &body body)
  `(when (not ,done)
     (setf ,done t)
     ,@body))


;;;; Sketch
(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Sketch")
     (copy-pixels t)
     (mouse (list 0 0))
     (frame 0)
     (done nil)
     ;; Data
     (size (1+ (expt 2 4)))
     (hm (sand.terrain.diamond-square::diamond-square
           (allocate-heightmap size)))
     (tile-size 5)
     ;; Pens
     (black-pen (make-pen :stroke (rgb 0 0 0) :fill (rgb 0.4 0.4 0.4) :weight 1 :curve-steps 50))
     (red-pen (make-pen :stroke (rgb 0.6 0 0) :fill (rgb 0.9 0 0) :weight 1 :curve-steps 50))
     (green-pen (make-pen :stroke (rgb 0 0.6 0) :fill (rgb 0 0.9 0) :weight 1 :curve-steps 50))
     (blue-pen (make-pen :stroke (rgb 0 0 0.6) :fill (rgb 0 0 0.9) :weight 1 :curve-steps 50))
     )
  (incf frame)
  ;;
  (just-once done
    (with-setup
      (in-context
        (iterate
          (for x :from 0 :to (floor *width* (* size tile-size)))
          (iterate
            (for y :from 0 :to (floor *height* (* size tile-size)))
            (draw-hm hm x y tile-size))))))
  ;;

  )


;;;; Mouse
(defun mousemove (instance x y)
  (with-slots (mouse) instance
    (setf mouse (list x (- *height* y)))
    ;;
    ;;
    )
  )


(defun mousedown-left (instance x y)
  (declare (ignorable instance x y))
  )

(defun mousedown-right (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-left (instance x y)
  (declare (ignorable instance x y))
  )

(defun mouseup-right (instance x y)
  (declare (ignorable instance x y))
  )


(defmethod kit.sdl2:mousemotion-event ((window demo) ts b x y xrel yrel)
  (declare (ignore ts b xrel yrel))
  (mousemove window x y))

(defmethod kit.sdl2:mousebutton-event ((window demo) state ts button x y)
  (declare (ignore ts))
  (funcall (case state
             (:mousebuttondown
              (case button
                (1 #'mousedown-left)
                (3 #'mousedown-right)))
             (:mousebuttonup
              (case button
                (1 #'mouseup-left)
                (3 #'mouseup-right))))
           window x y))


;;;; Keyboard
(defun keydown (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-space (sketch::prepare instance))
    (:scancode-lshift (setf *shift* t))
    (:scancode-lctrl (setf *control* t))
    (:scancode-lgui (setf *command* t))
    (:scancode-lalt (setf *option* t))
    ;;
    ;;
    ))

(defun keyup (instance scancode)
  (declare (ignorable instance))
  (scancode-case scancode
    (:scancode-lshift (setf *shift* nil))
    (:scancode-lctrl (setf *control* nil))
    (:scancode-lgui (setf *command* nil))
    (:scancode-lalt (setf *option* nil))
    (:scancode-space nil)))


(defmethod kit.sdl2:keyboard-event ((instance demo) state timestamp repeatp keysym)
  (declare (ignore timestamp repeatp))
  (cond
    ((eql state :keyup) (keyup instance (sdl2:scancode-value keysym)))
    ((eql state :keydown) (keydown instance (sdl2:scancode-value keysym)))
    (t nil)))


;;;; Run
; (defparameter *demo* (make-instance 'demo))
