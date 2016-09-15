(in-package #:sand.number-letters)

; https://www.youtube.com/watch?v=LYKn0yUTIU4

(declaim (optimize (debug 0) (safety 1) (speed 3)))


;;;; Slow/Reference Implementation --------------------------------------------
(defun number-string (n)
  (format nil "~R" n))

(defun slow-letter-count (n)
  (count-if #'alpha-char-p (number-string n)))


;;;; Fast Version -------------------------------------------------------------
(define-constant +small-counts+
  (make-array 1000
    :element-type 'fixnum
    :initial-contents (iterate (for i :from 0 :below 1000)
                               (collect (slow-letter-count i)))))

(defparameter *suffixes*
  (cons ""
        (iterate (for i :from 1 :to 21)
                 (collect (subseq (format nil "~R" (expt 1000 i)) 4)))))

(define-constant +suffix-lengths+ (mapcar #'length *suffixes*)
  :test #'equal)


(declaim (ftype (function ((integer 0)) fixnum)
                fast-letter-count))
(defun fast-letter-count (n)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (if (zerop n)
    4
    (iterate
      (for i :first n :then (floor i 1000))
      (for sl :in +suffix-lengths+)
      (while (not (zerop i)))
      (for part = (mod i 1000))
      (when (not (zerop part))
        (sum sl)
        (sum (aref +small-counts+ part))))))

(defun sanity-check ()
  (iterate (for i :from 1 :to 10000000)
           (finding i :such-that (not (= (fast-letter-count i)
                                         (slow-letter-count i))))))


;;;; Chains -------------------------------------------------------------------
(defun chain-length (n)
  (if (= n 4)
    1
    (1+ (chain-length (fast-letter-count n)))))

(defun print-chain (n)
  (let ((lc (letter-count n)))
    (format t "~D - ~R -> ~D~%" n n lc)
    (when (not (= n 4))
      (print-chain lc))))


(define-constant +cache-size+ 1000)
(define-constant +cache+
  (make-array +cache-size+
    :element-type 'fixnum
    :initial-contents (iterate (for i :from 0 :below +cache-size+)
                               (collect (chain-length i)))))

(defun chain-length% (n)
  (iterate
    (for i :first n :then (fast-letter-count i))
    (summing 1 :into result)
    (declare (type fixnum result))
    (when (< i +cache-size+)
      (return (the fixnum (+ result (aref +cache+ i)))))))


(defun longest-chain (max)
  (iterate
    (for i :from 1 :to max)
    (finding i :maximizing (chain-length% i))))



; (time
;   (print-chain (longest-chain (expt 10 9))))
