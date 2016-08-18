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


(defparameter *black-pen*
  (make-pen :stroke (rgb 0 0 0) :fill (rgb 0.4 0.4 0.4) :weight 1 :curve-steps 50))

(defparameter *red-pen*
  (make-pen :stroke (rgb 0.6 0 0) :fill (rgb 0.9 0 0) :weight 1 :curve-steps 50))

(defparameter *green-pen*
  (make-pen :stroke (rgb 0 0.6 0) :fill (rgb 0 0.9 0) :weight 1 :curve-steps 50))

(defparameter *blue-pen*
  (make-pen :stroke (rgb 0 0 0.6) :fill (rgb 0 0 0.9) :weight 1 :curve-steps 50))


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

(defmacro just-once (dirty-place &body body)
  `(when ,dirty-place
     (setf ,dirty-place nil)
     ,@body))


;;;; Diamond Square
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
  (let ((size (first (array-dimensions hm))))
    (in-context
      (translate (* ox (* ts size))
                 (* oy (* ts size)))
      (iterate
        (for (h x y) :in-array hm)
        (with-pen (make-pen :fill (gray h))
          (rect (* x ts) (* y ts)
                ts ts)))
      (with-pen (make-pen :fill nil :stroke (rgb 1.0 0 0 0.5))
        (rect 0 0 (* ts size) (* ts size))))))


(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Diamond Square")
     (copy-pixels t)
     (mouse (list 0 0))
     (dirty t)
     ;; Data
     (size (1+ (expt 2 4)))
     (hm (sand.terrain.diamond-square::diamond-square
           5 :tileable t :spread 0.7 :spread-reduction 0.5))
     (tile-size 3)
     )
  ;;
  (just-once dirty
    (with-setup
      (iterate
        (for-nested ((x :from 0 :to (floor *width* (* size tile-size)))
                     (y :from 0 :to (floor *height* (* size tile-size)))))
        (draw-hm hm x y tile-size))))
  ;;

  )

;;;; Template
(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Sketch")
     (copy-pixels t)
     (mouse (list 0 0))
     (dirty t)
     ;; Data
     )
  ;;
  (just-once dirty
    (with-setup
      (text "Demo" (- *center-x* 23) (- *center-y* 10))

      ))
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
