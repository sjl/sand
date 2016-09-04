(in-package #:sand.streams)


;;;; Streams from SICP

;;;; Delay/Force --------------------------------------------------------------
(defun memoize (thunk)
  (let ((done nil) (result nil))
    (lambda ()
      (if done
        result
        (setf done t
              result (funcall thunk))))))

(defmacro delay (&body body)
  `(memoize (lambda () ,@body)))

(defun force (delay)
  (funcall delay))


;;;; Basic Streams ------------------------------------------------------------
(defun scar (stream)
  (car stream))

(defun scdr (stream)
  (if (cdr stream)
    (force (cdr stream))
    nil))

(defmacro scons (car cdr)
  `(cons ,car (delay ,cdr)))

(defun snullp (stream)
  (null stream))

(defun empty-stream ()
  nil)


;;;; Stream Operations --------------------------------------------------------
(defun stream-nth (n stream)
  (if (zerop n)
    (scar stream)
    (stream-nth (1- n) (scdr stream))))

(defun stream-map (function stream)
  (if (snullp stream)
    (empty-stream)
    (scons (funcall function (scar stream))
           (stream-map function (scdr stream)))))

(defun stream-do (function stream)
  (if (snullp stream)
    (values)
    (progn (funcall function (scar stream))
           (stream-do function (scdr stream)))))

(defun print-stream (stream)
  (stream-do #'pr stream))

(defun stream-range (low high)
  (if (> low high)
    (empty-stream)
    (scons low (stream-range (1+ low) high))))

(defun stream-filter (function stream)
  (if (snullp stream)
    (empty-stream)
    (if (funcall function (scar stream))
      (scons (scar stream) (stream-filter function (scdr stream)))
      (stream-filter function (scdr stream)))))

(defun stream-take (n stream)
  (if (or (snullp stream) (zerop n))
    (empty-stream)
    (scons (scar stream) (stream-take (1- n) (scdr stream)))))

(defun stream-drop (n stream)
  (if (or (snullp stream) (zerop n))
    (scdr stream)
    (stream-drop (1- n) (scdr stream))))


;;;; Scratch ------------------------------------------------------------------
(trace primep)

(->> (stream-range 10000 1000000)
  (stream-filter #'primep)
  (stream-take 2)
  print-stream)

(stream-nth 2 (stream-filter #'primep (stream-range 1 10)))

(untrace primep)



