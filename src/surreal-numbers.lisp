(defpackage :sand.surreal-numbers
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils))

(in-package :sand.surreal-numbers)


(defstruct (surreal (:constructor surreal (left right)))
  (left nil :type list)
  (right nil :type list))


(defun surreal<= (x y)
  ;; "One number is less than or equal to another if and only if
  ;; no member of the first number's left set is greater than or equal to the second number, and
  ;; no member of the second number's right set is less than or equal to the first number"
  (and (notany (lambda (n) (surreal>= n y)) (surreal-left x))
       (notany (lambda (n) (surreal<= n x)) (surreal-right y))))

(defun surreal>= (x y)
  (surreal<= y x))

(defun surreal< (x y)
  (and (surreal<= x y)
       (not (surreal<= y x))))

(defun surreal> (x y)
  (surreal< y x))

(defun surreal= (x y)
  (and (surreal<= x y)
       (surreal<= y x)))

(defun surreal!= (x y)
  (not (surreal= x y)))


(defun check-surreal (surreal)
  ;; "No member of the left set is greater than or equal to a member of the
  ;; right set"
  (dolist (x (surreal-left surreal) t)
    (dolist (y (surreal-right surreal))
      (assert (not (surreal>= x y)))))
  t)


(defparameter *0* (surreal nil nil))
(defparameter *1* (surreal (list *0*) nil))
(defparameter *-1* (surreal nil (list *0*)))
