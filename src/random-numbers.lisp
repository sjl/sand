(in-package #:sand.random-numbers)


(declaim (optimize (speed 1) (safety 1) (debug 3)))

(deftype positive-fixnum () `(integer 1 ,most-positive-fixnum))
(deftype negative-fixnum () `(integer ,most-negative-fixnum -1))
(deftype nonnegative-fixnum () `(integer 1 ,most-positive-fixnum))
(deftype nonpositive-fixnum () `(integer ,most-negative-fixnum -1))

(defun* +mod ((x nonnegative-fixnum)
              (y nonnegative-fixnum)
              (m positive-fixnum))
  (if (<= x (- m 1 y))
    (+ x y)
    (- x (- m y))))


(defun* make-linear-congruential-rng
    ((modulus positive-fixnum)
     (multiplier nonnegative-fixnum)
     (increment nonnegative-fixnum)
     (seed nonnegative-fixnum))
  (let ((val seed))
    (lambda (incr)
      (loop :repeat incr :do
            (setf val (mod (+ (* multiplier val)
                              increment)
                           modulus))))))

(defun* make-linear-congruential-rng-fast%
    ((modulus positive-fixnum)
     (multiplier nonnegative-fixnum)
     (increment nonnegative-fixnum)
     (seed nonnegative-fixnum))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((val seed))
    (lambda (incr)
      (declare (positive-fixnum incr))
      (loop :repeat incr :do
            (setf val (mod (+ (the nonnegative-fixnum (* multiplier val))
                              increment)
                           modulus))))))

(define-compiler-macro make-linear-congruential-rng
    (&whole form
     modulus multiplier increment seed)
  (if (and (constantp modulus)
           (constantp multiplier)
           (<= (* multiplier (1- modulus))
               most-positive-fixnum))
    `(make-linear-congruential-rng-fast% ,modulus ,multiplier ,increment ,seed)
    form))


(defun dammit () (make-linear-congruential-rng 50 2 3 2))
(defparameter *r* (dammit))
(disassemble *r*)

(defparameter m 40)

(defun run ()
  (let ((r (make-linear-congruential-rng 50 2 3 2)))
    (disassemble r)
    (funcall r 100000000)))

(time (run))
