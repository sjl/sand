(in-package #:sand.sketch)

;;;; Config
(setf *bypass-cache* t)
(defparameter *wat* nil)
(defparameter *width* 600)
(defparameter *height* 600)


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
(defparameter *tile-count* 20)
(defparameter *tile-width* (/ *width* *tile-count*))
(defparameter *tile-height* (/ *height* *tile-count*))

(defparameter *wall-pen* (make-pen :fill (gray 0.0)))
(defparameter *floor-pen* (make-pen :fill (gray 1.0)))
(defparameter *goal-pen* (make-pen :fill (rgb 0.0 1.0 0.0)))

(defun draw-map (map)
  (iterate (for (v x y) :in-array map)
           (with-pen (ecase v
                       (:blank *floor-pen*)
                       (:wall *wall-pen*)
                       (:goal *goal-pen*))
             (rect (* x *tile-width*)
                   (* y *tile-height*)
                   *tile-width*
                   *tile-height*))))

(defun draw-dijkstra (dm)
  (iterate
    (with max = (sand.dijkstra-maps:dm-maximum-value dm))
    (with data = (sand.dijkstra-maps::dm-map dm))
    (for (v x y) :in-array data)
    (unless (= most-positive-single-float v)
      (with-pen (make-pen :fill (rgb 1.0 0.0 0.0
                                     (/ v max)))
        (rect (* x *tile-width*)
              (* y *tile-height*)
              *tile-width*
              *tile-height*)))))

(defsketch demo
    ((width *width*) (height *height*) (y-axis :up) (title "Sketch")
     (copy-pixels t)
     (mouse (list 0 0))
     (mouse-down-left nil)
     (mouse-down-right nil)
     (dirty t)
     ;; Data
     (map (make-array (list *tile-count* *tile-count*)
            :element-type t
            :initial-element :blank))
     (dm nil)
     (lol (progn
            (setf (aref map
                        (random-range 0 *tile-count*)
                        (random-range 0 *tile-count*))
                  :goal)
            )))
  ;;
  (just-once dirty
    (with-setup
      (setf dm (sand.dijkstra-maps::make-dijkstra-map map
                                                      (curry #'eql :goal)
                                                      (curry #'eql :wall)))
      (draw-map map)
      (draw-dijkstra dm)))
  ;;

  )


;;;; Mouse
(defun mouse-in-bounds-p (x y)
  (and (>= x 0)
       (>= y 0)
       (< x *width*)
       (< y *height*)))

(defun mousemove (instance x y)
  (when (mouse-in-bounds-p x y)
    (with-slots (mouse) instance
      (setf mouse (list x (- *height* y 1)))
      ;;
      (when (or (slot-value instance 'mouse-down-left)
                (slot-value instance 'mouse-down-right))
        (setf (slot-value instance 'dirty) t)
        (let ((tx (floor x *tile-width*))
              (ty (floor (- *height* y 1) *tile-height*)))
          (zapf (aref (slot-value instance 'map) tx ty)
                (if (slot-value instance 'mouse-down-left)
                  (case %
                    (:blank :wall)
                    (:goal :goal)
                    (:wall :wall))
                  (case %
                    (:blank :blank)
                    (:goal :goal)
                    (:wall :blank))))))
      ;;
      ))
  )


(defun mousedown-left (instance x y)
  (when (mouse-in-bounds-p x y)
    (setf (slot-value instance 'mouse-down-left) t)
    ;;
    (mousemove instance x y)
    ;;
    ))

(defun mousedown-right (instance x y)
  (when (mouse-in-bounds-p x y)
    (setf (slot-value instance 'mouse-down-right) t)
    ;;
    (mousemove instance x y)
    ;;
    ))

(defun mouseup-left (instance x y)
  (declare (ignorable x y))
  (setf (slot-value instance 'mouse-down-left) nil)
  ;;
  )

(defun mouseup-right (instance x y)
  (declare (ignorable x y))
  (setf (slot-value instance 'mouse-down-right) nil)
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
