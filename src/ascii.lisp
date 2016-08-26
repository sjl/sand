(in-package #:sand.ascii)


(defparameter *running* nil)
(defparameter *running* t)

(defparameter *ball* (list :x 0 :y 20 :vx 1))
(defparameter *width* 1)
(defparameter *height* 1)


;;;; Color --------------------------------------------------------------------
(defmethod print-object ((object hash-table) stream)
  (let* ((keys (hash-table-keys object))
         (vals (hash-table-values object))
         (count (hash-table-count object))
         (key-width (-<> keys
                      (mapcar (compose #'length #'prin1-to-string) <>)
                      (reduce #'max <> :initial-value 0)
                      (clamp 0 20 <>))))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream ":test ~A :count ~D {~%~{~{  ~vs ~s~}~%~}}"
              (hash-table-test object)
              count
              (loop
                :with limit = 40
                :for key :in keys
                :for val :in vals
                :for i :from 0 :to limit
                :collect (if (= i limit)
                           (list key-width 'too-many-items (list (- count i) 'more))
                           (list key-width key val)))))))


(defvar *colors* (make-hash-table))
(defvar *color-pairs* (make-hash-table))
(defvar *color-pair-counter* 100)

(defun initialize-color ()
  (charms/ll:start-color)
  (clrhash *color-pairs*)
  (setf *color-pair-counter* 100)
  (iterate
    (for (nil (number r g b)) :in-hashtable *colors*)
    (charms/ll:init-color number r g b)))



(defun color-content (color-index)
  (cffi:with-foreign-objects ((r :short) (g :short) (b :short))
    (charms/ll:color-content 1 r g b)
    (list (cffi:mem-ref r :short)
          (cffi:mem-ref g :short)
          (cffi:mem-ref b :short))))

(defmacro with-attr (attr &body body)
  `(prog2
    (charms/ll:attron ,attr)
    (progn ,@body)
    (charms/ll:attroff ,attr)))


(defun define-color (name number r g b)
  (flet ((conv (fl)
           (clamp 0 999 (truncate (* 1000 fl)))))
    (setf (gethash name *colors*)
          (list number (conv r) (conv g) (conv b)))
    (clrhash *color-pairs*) ; fuck it
    t))

(define-color :black    100 0 0 0)
(define-color :lavender 101 0.733 0.549 0.757)
(define-color :peach    102 0.831 0.537 0.416)
(define-color :red      103 1 0 0)

(defun setup-color-pair (fg bg)
  (let ((pair-id (incf *color-pair-counter*)))
    (charms/ll:init-pair pair-id
                         (first (gethash fg *colors*))
                         (first (gethash bg *colors*)))
    pair-id))

(defun retrieve-color-pair (fg bg)
  (-<> *color-pairs*
    (ensure-gethash bg <> (make-hash-table))
    (ensure-gethash fg <> (setup-color-pair fg bg))
    (charms/ll:color-pair <>)))

(defmacro with-color ((fg bg) &body body)
  `(with-attr (retrieve-color-pair ,fg ,bg)
    ,@body))

(defun render ()
  (with-color (:red :red)
    (charms:write-string-at-point charms:*standard-window*
                                  "KINGBREAKER"
                                  10 8))
  (charms:move-cursor charms:*standard-window*
                      (getf *ball* :x)
                      (getf *ball* :y))
  (charms:write-char-at-cursor charms:*standard-window* #\o)
  (charms:move-cursor charms:*standard-window* 0 0)
  )


(defun tick ()
  (incf (getf *ball* :x)
        (getf *ball* :vx))
  (when (not (< 0 (getf *ball* :x) (1- *width*)))
    (negatef (getf *ball* :vx)))
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
    (initialize-color)

    (iterate
      (while *running*)
      (charms:clear-window charms:*standard-window*)
      (manage-screen)
      (handle-input)
      (tick)
      (render)
      (charms:refresh-window charms:*standard-window*)
      (sleep 0.1))))


; (run)

