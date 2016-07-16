(in-package #:sand.ascii)


(defparameter *running* nil)
(defparameter *running* t)

(defparameter *ball* (list :x 0 :y 20 :vx 1))
(defparameter *width* 1)
(defparameter *height* 1)

(defun render ()
  (charms:move-cursor charms:*standard-window*
                      (getf *ball* :x)
                      (getf *ball* :y))
  (charms:write-char-at-cursor charms:*standard-window* #\o)
  (charms:move-cursor charms:*standard-window* 0 0))


(defun tick ()
  (incf (getf *ball* :x)
        (getf *ball* :vx))
  (when (not (< 0 (getf *ball* :x) (1- *width*)))
    (zap% (getf *ball* :vx) #'- %))
  (setf (getf *ball* :y) (truncate (/ *height* 2))))

(defun handle-input ()
  (let ((input (charms:get-char charms:*standard-window* :ignore-error t)))
    (case input
      ((nil) nil)
      (#\q (setf *running* nil)))))

(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *width* w *height* h)))

(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (iterate
      (while *running*)
      (charms:clear-window charms:*standard-window*)
      (manage-screen)
      (handle-input)
      (tick)
      (render)
      (charms:refresh-window charms:*standard-window*)
      (sleep 0.03))))


; (run)
