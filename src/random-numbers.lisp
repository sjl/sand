(in-package #:sand.random-numbers)


;;;; Types, etc
; (declaim (optimize (speed 1) (safety 1) (debug 3)))
; (declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype positive-fixnum () `(integer 1 ,most-positive-fixnum))
(deftype negative-fixnum () `(integer ,most-negative-fixnum -1))
(deftype nonnegative-fixnum () `(integer 1 ,most-positive-fixnum))
(deftype nonpositive-fixnum () `(integer ,most-negative-fixnum -1))


;;;; Utils
(declaim (ftype (function (nonnegative-fixnum
                            nonnegative-fixnum
                            nonnegative-fixnum)
                          nonnegative-fixnum)
                mod+)
         (inline mod+))

(defun mod+ (x y m)
  (if (<= x (- m 1 y))
    (+ x y)
    (- x (- m y))))


;;;; Random Number Generators
(defun make-linear-congruential-rng (modulus multiplier increment seed)
  (declare (type nonnegative-fixnum seed)
           (type positive-fixnum modulus multiplier increment))
  (let ((val (mod (logxor seed multiplier)
                  modulus)))
    (dlambda
      (:next ()
       (ldb (byte 32 16)
            (setf val (mod (+ (* val multiplier) increment)
                           modulus))))
      (:modulus () modulus))))


(declaim (inline rng-next rng-modulus))

(defun rng-next (generator)
  (funcall generator :next))

(defun rng-modulus (generator)
  (funcall generator :modulus))


(define-compiler-macro make-linear-congruential-rng
    (&whole form
     modulus multiplier increment seed)
  (if (and (constantp modulus)
           (constantp multiplier)
           (<= (* multiplier (1- modulus))
               most-positive-fixnum))
    `(make-linear-congruential-rng-fast% ,modulus ,multiplier ,increment ,seed)
    form))


(defparameter *generator*
  (make-linear-congruential-rng (expt 2 48)
                                25214903917
                                11
                                0))


(defun rand ()
  (rng-next *generator*))

(defun rand-float ()
  (float (/ (rng-next *generator*)
            (rng-modulus *generator*))))


;;;; Mapping
;;; The Monte Carlo method is bad because it's biased, but it's fast.
;;;
;;; Basically we take our generator that generates say 1-8, and map the range
;;; ABC onto it:
;;;
;;;    1 2 3 4 5 6 7 8
;;;    A B C A B C A B
;;;
;;; Notice that it's not uniform.
(defun monte-carlo (width)
  (mod (rng-next *generator*) width))


;;; The Las Vegas method is a bit slower, but unbiased.  We group the random
;;; numbers into contiguous buckets, with the last "partial bucket" being
;;; excess.  If we hit that one we just loop and try again:
;;;
;;;    1 2 3 4 5 6 7 8
;;;    A A B B C C retry
(defun las-vegas (width)
  (let* ((modulus (rng-modulus *generator*))
         (bucket-width (truncate (/ modulus width))))
    (iterate
      (for bucket = (truncate (/ (rng-next *generator*)
                                 bucket-width)))
      (finding bucket :such-that (< bucket width)))))


(defun rand-range-bad (min max)
  (+ min (monte-carlo (- max min))))

(defun rand-range (min max)
  (+ min (las-vegas (- max min))))



;;;; Spectral Test
(defun spectral ()
  (spit "data"
    (iterate
      (repeat 1000)
      (for i = (rand))
      (for j :previous i)
      (for k :previous j)
      (when k
        (format t "~d ~d ~d~%" i j k)))))


; (spectral)
