(in-package :sand.random-numbers)


;;;; Types, etc
; (declaim (optimize (speed 1) (safety 1) (debug 3)))
; (declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype positive-fixnum () `(integer 1 ,most-positive-fixnum))
(deftype negative-fixnum () `(integer ,most-negative-fixnum -1))
(deftype nonnegative-fixnum () `(integer 0 ,most-positive-fixnum))
(deftype nonpositive-fixnum () `(integer ,most-negative-fixnum 0))


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
(defun make-linear-congruential-rng-java (modulus multiplier increment seed)
  (declare (type nonnegative-fixnum seed)
           (type positive-fixnum modulus multiplier increment))
  (let ((val (mod (logxor seed multiplier)
                  modulus)))
    (lambda (arg)
      (case arg
        (:next
         (ldb (byte 32 16) ; java's j.u.Random only gives out 32 high-order bits
              (setf val (mod (+ (* val multiplier) increment)
                             modulus))))
        (:modulus
         modulus)))))

(defun make-linear-congruential-rng (modulus multiplier increment seed)
  (declare (type nonnegative-fixnum seed)
           (type positive-fixnum modulus multiplier increment))
  (let ((val (mod (logxor seed multiplier)
                  modulus)))
    (lambda (arg)
      (case arg
        (:next
         (setf val (mod (+ (* val multiplier) increment)
                        modulus)))
        (:modulus modulus)))))


(declaim (inline rng-next rng-modulus))

(defun rng-next (generator)
  (funcall generator :next))

(defun rng-modulus (generator)
  (funcall generator :modulus))


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


;;;; Distributions
(defun prefix-sums (list)
  (iterate
    (for i :in list)
    (sum i :into s)
    (collect s :result-type vector)))

(defun frequencies (seq &key (test 'eql))
  (iterate
    (with result = (make-hash-table :test test))
    (for i :in-whatever seq)
    (incf (gethash i result 0))
    (finally (return result))))


(defun random-weighted-list (weights n)
  (iterate
    (with sums = (prefix-sums weights))
    (with max = (elt sums (1- (length sums))))
    (repeat n)
    (collect (iterate
               (with r = (rand-range 0 max))
               (for s :in-vector sums :with-index i)
               (finding i :such-that (< r s))))))

(defun random-weighted (weights)
  (first (random-weighted-list weights 1)))


;; from cl-utilities

;; If we're using the SB-ROTATE-BYTE extension, we should inline our
;; call and let SBCL handle optimization from there.
#+sbcl (declaim (inline rotate-byte))

(defun rotate-byte (count bytespec integer)
  "Rotates a field of bits within INTEGER; specifically, returns an
integer that contains the bits of INTEGER rotated COUNT times
leftwards within the byte specified by BYTESPEC, and elsewhere
contains the bits of INTEGER. See http://www.cliki.net/ROTATE-BYTE"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 1)))
  #-sbcl
  (let ((size (byte-size bytespec)))
    (when (= size 0)
      (return-from rotate-byte integer))
    (let ((count (mod count size)))
      (flet ((rotate-byte-from-0 (count size integer)
                 (let ((bytespec (byte size 0)))
                   (if (> count 0)
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (- count size))))
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (+ count size))))))))
        (dpb (rotate-byte-from-0 count size (ldb bytespec integer))
             bytespec
             integer))))
  ;; On SBCL, we use the SB-ROTATE-BYTE extension.
  #+sbcl (sb-rotate-byte:rotate-byte count bytespec integer))


;;;; PCG
(defstruct (pcg (:constructor make-pcg%))
  (state 0 :type (unsigned-byte 64))
  (increment 0 :type (unsigned-byte 64)))

(declaim (inline permute-xor-shift permute-rotate advance-state))

(defun permute-xor-shift (data)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type (unsigned-byte 37) data))
  (-<> data
    (ash <> -18)
    (logxor data <>)
    (ldb (byte 32 0) <>)))

(defun permute-rotate (data selector)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type (unsigned-byte 32) data)
           (type (unsigned-byte 5) selector))
  (sb-rotate-byte:rotate-byte selector
                              (byte 32 0)
                              data))

(defun advance-state (pcg)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type pcg pcg))
  (setf (pcg-state pcg)
        (mod (+ (* (pcg-state pcg) 6364136223846793005)
                (pcg-increment pcg))
             (expt 2 64)))
  nil)


; uint64_t oldstate = rng->state;
; rng->state = oldstate * 6364136223846793005ULL + rng->inc;
; bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
; >>27
; bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb...........................
; assign to uint32 fuckin lol
; .....bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb...........................

; uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
; uint32_t rot = oldstate >> 59u;
; return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
(declaim (ftype (function (pcg) (unsigned-byte 32)) pcg))

(defun-inlineable pcg-random (pcg)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type pcg pcg))
  (let* ((state (pcg-state pcg))
         (data (ldb (byte 37 (- 64 37)) state))
         (selector (ldb (byte 5 (- 64 5)) state)))
    (advance-state pcg)
    (-<> data
      (permute-xor-shift <>)
      (permute-rotate <> selector))))

(defun make-pcg (seed &optional (stream-id 0))
  (let* ((increment (logior 1 (ash stream-id 1)))
         (pcg (make-pcg% :state 0 :increment increment)))
    (pcg-random pcg)
    (incf (pcg-state pcg) seed)
    (modf (pcg-state pcg) 64)
    (pcg-random pcg)
    pcg))

(defun pcg-random-bounded (pcg bound)
  (declare
    (optimize (speed 3) (debug 0) (safety 1))
    (type pcg pcg)
    (type (and (unsigned-byte 32)
               (integer 1))
          bound)
    (inline pcg-random))
  (loop
    :with threshold = (mod (expt 2 32) bound)
    :for n = (pcg-random pcg)
    :when (>= n threshold)
    :do (return (values (mod n bound)))))

(declaim (ftype (function (pcg (integer 1 32))
                          (unsigned-byte 32)) pcg-random-bits))

(defun-inline pcg-random-bits (pcg count)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (inline pcg-random))
  (ldb (byte count 0) (pcg-random pcg)))

(defun pcg-random-float (pcg)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type pcg pcg))
  ; https://en.wikipedia.org/wiki/Single-precision_floating-point_format
  ; Singles have 24 bits of precision
  (/ (pcg-random-bits pcg 24)
     (coerce (expt 2 24) 'single-float)))

; https://en.wikipedia.org/wiki/Double-precision_floating-point_format
; Doubles have 53 bits of precision
(defun pcg-random-double (pcg)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type pcg pcg))
  (/ (logior (ash (pcg-random-bits pcg 26) 27)
             (pcg-random-bits pcg 27))
     (coerce (expt 2 53) 'double-float)))

(defun pcg-random-range (pcg min max)
  (+ min (pcg-random-bounded pcg (- max min))))

(defun pcg-random-range-inclusive (pcg min max)
  (+ min (pcg-random-bounded pcg (1+ (- max min)))))

;;;; Scratch
; (spectral)
