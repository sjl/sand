(in-package :sand.ascii)


(defparameter *running* nil)
(defparameter *running* t)

(defparameter *ball* (list :x 0 :y 6 :vx 2))
(defparameter *width* 1)
(defparameter *height* 1)
(defparameter *window-x* 0)
(defparameter *window-y* 0)


(defun render (window)
  (charms:clear-window window)
  (charms:move-cursor window
                      (getf *ball* :x)
                      (getf *ball* :y))
  (charms:write-char-at-cursor window #\@)
  ; (charms:move-cursor window 0 0)
  )


(defun tick ()
  (incf (getf *ball* :x)
        (getf *ball* :vx))
  (when (not (in-range-p 0 (getf *ball* :x) 10))
    (negatef (getf *ball* :vx))
    (zapf (getf *ball* :x)
          (max 0 (min (1- 10) %))))
  ; (setf (getf *ball* :y) (truncate (/ *height* 2)))
  )

(defparameter *input* nil)
(defun handle-input ()
  (let ((input (charms:get-char charms:*standard-window* :ignore-error t)))
    (when input
      (push input *input*))
    (case input
      ((nil) nil)
      (#\h (zapf *window-x* (1- %)))
      (#\j (zapf *window-y* (1+ %)))
      (#\k (zapf *window-y* (1- %)))
      (#\l (zapf *window-x* (1+ %)))
      (#\q (setf *running* nil)))))

(defun manage-screen ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (setf *width* w *height* h)))

(defun fill-window (window width height ch)
  (iterate (for-nested ((x :from 0 :below width)
                        (y :from 0 :below height)))
           (charms:write-char-at-point window ch x y)))

(defmacro with-window ((symbol width height start-x start-y) &body body)
  `(let ((,symbol (charms:make-window ,width ,height ,start-x ,start-y)))
     (unwind-protect (progn ,@body)
       (charms:destroy-window ,symbol))))

(defmacro with-panel ((symbol window) &body body)
  `(let ((,symbol (charms:make-panel ,window)))
     (unwind-protect (progn ,@body)
       (charms:destroy-panel ,symbol))))

(defmacro with-windows (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-window ,(first bindings)
       (with-windows ,(rest bindings)
         ,@body))))

(defmacro with-panels (bindings &body body)
  (if (null bindings)
    `(progn ,@body)
    `(with-panel ,(first bindings)
       (with-panels ,(rest bindings)
         ,@body))))


; (defun run ()
;   (setf *running* t)
;   (charms:with-curses ()
;     (charms:disable-echoing)
;     (charms:enable-raw-input :interpret-control-characters t)
;     (charms:enable-extra-keys charms:*standard-window*)
;     (charms:enable-non-blocking-mode charms:*standard-window*)

;     (with-windows ((x-win 20 20 1 1)
;                    (ball-win 10 10 0 0)
;                    (o-win 5 5 4 0))
;       (fill-window x-win 20 20 #\x)
;       (fill-window o-win 5 5 #\O)
;       ; (with-panels ((x-pan x-win)
;       ;               (ball-pan ball-win)
;       ;               (o-pan o-win))
;       (iterate
;         (while *running*)
;         (manage-screen)
;         (handle-input)
;         (tick)
;         ; (charms:move-panel ball-pan *window-x* *window-y*)
;         (render ball-win)
;         ; (charms:update-panels)
;         (charms:update)
;         (sleep 0.1))
;       ; )
;       )))

(defun run ()
  (setf *running* t)
  (charms:with-curses ()
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-extra-keys charms:*standard-window*)
    (charms:enable-non-blocking-mode charms:*standard-window*)

    (fill-window t 20 20 #\#)
    (iterate
      (while *running*)
      (manage-screen)
      (handle-input)
      (tick)
      (charms:refresh-window t)
      (sleep 0.1))))

; (run)

