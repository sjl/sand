(in-package :sand.streams)


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

(defmacro str* (&rest args)
  (if (null (cdr args))
    (car args)
    `(scons ,(car args) (stream* ,@(cdr args)))))

(defun str (&rest args)
  (if (null args)
    (empty-stream)
    (scons (car args) (apply #'str (cdr args)))))


;;;; Stream Operations --------------------------------------------------------
(defun stream-nth (n stream)
  (if (zerop n)
    (scar stream)
    (stream-nth (1- n) (scdr stream))))

(defun stream-map (function &rest streams)
  (if (some #'snullp streams)
    (empty-stream)
    (scons (apply function (mapcar #'scar streams))
           (apply #'stream-map function (mapcar #'scdr streams)))))

(defun stream-do (function stream)
  (if (snullp stream)
    (values)
    (progn (funcall function (scar stream))
           (stream-do function (scdr stream)))))

(defun print-stream (stream)
  (stream-do #'pr stream))

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

(defun stream-add (&rest streams)
  (apply #'stream-map #'+ streams))

(defun stream-mul (&rest streams)
  (apply #'stream-map #'* streams))

(defun stream-scale (factor stream)
  (stream-map (curry #'* factor) stream))

(defun stream-partial-sums (stream)
  (scons (scar stream)
         (stream-map (curry #'+ (scar stream))
                     (stream-partial-sums (scdr stream)))))


;;;; Stream Creation ----------------------------------------------------------
(defun stream-range (low high)
  (if (> low high)
    (empty-stream)
    (scons low (stream-range (1+ low) high))))


(defun integers-from (n)
  (scons n (integers-from (1+ n))))

(defun integers ()
  (integers-from 0))


(defun fibgen (a b)
  (scons a (fibgen b (+ a b))))

(defun fibonacci-numbers ()
  (fibgen 0 1))


(defun sieve (stream)
  (let ((x (scar stream)))
    (scons x (sieve
               (stream-filter (lambda (n) (not (dividesp n x)))
                              (scdr stream))))))

(defun primes ()
  (sieve (integers-from 2)))


(defparameter *primes%*
  (scons 2 (stream-filter #'primep% (integers-from 3))))

(defun primep% (n)
  (recursively ((prime-stream *primes%*))
    (cond
      ((> (square (scar prime-stream)) n) t)
      ((dividesp n (scar prime-stream)) nil)
      (t (recur (scdr prime-stream))))))


;;;; Scratch ------------------------------------------------------------------
; (trace primep)

; (->> (stream-range 10000 1000000)
;   (stream-filter #'primep)
;   (stream-take 2)
;   print-stream)

; (stream-nth 2 (stream-filter #'primep (stream-range 1 10)))

; (untrace primep)

(defparameter *ones*
  (scons 1 *ones*))

(defparameter *ints*
  (scons 1 (stream-add *ones* *ints*)))

(defparameter *fibs*
  (str* 0 1 (stream-add *fibs* (scdr *fibs*))))

(defparameter *powers-of-two*
  (scons 1 (stream-scale 2 *powers-of-two*)))

(defparameter *factorials*
  (scons 1 (stream-mul *factorials* (scdr (integers)))))



; (->> *integers* 
;   (stream-filter (lambda (i) (not (dividesp i 7))))
;   (stream-take 15)
;   print-stream)

; (->> *factorials*
;   (stream-take 10)
;   print-stream)


; (->> (stream-partial-sums (integers))
;   (stream-take 10)
;   print-stream)

