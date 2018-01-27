(losh:eval-dammit
  (ql:quickload '(:easing)))

(defpackage :sand.easing
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils))

(in-package :sand.easing)


;;;; Data ---------------------------------------------------------------------
(defparameter *new-eases* nil)
(defparameter *current-eases* nil)



;;;; Internal ------------------------------------------------------------------
(eval-dammit
  (defun elapsed-time (start-time)
    (-<> (get-internal-real-time)
      (- <> start-time)
      (/ <> internal-time-units-per-second)))

  (defun elapsed-time-normalized (start-time duration)
    (min 1.0 (/ (elapsed-time start-time) duration)))

  (defun ease% (place from to duration easing-function after environment)
    (multiple-value-bind (temps exprs stores store-expr access-expr)
        (get-setf-expansion place environment)
      (declare (ignore access-expr))
      (with-gensyms (start)
        (once-only (duration from to)
          `(push (let ((,start (get-internal-real-time)))
                   (lambda ()
                     (let* ((n (elapsed-time-normalized ,start ,duration))
                            ,@(mapcar #'list temps exprs)
                            (,(car stores)
                              (map-range 0 1 ,from ,to
                                         (funcall ,easing-function n))))
                       ,store-expr
                       (if (= n 1.0)
                         (progn ,after t)
                         nil))))
             *new-eases*))))))


;;;; API ----------------------------------------------------------------------
(defmacro ease (place from to &key
                (easing-function #'easing:linear)
                (duration 1.0)
                (after nil)
                &environment environment)
  (ease% place from to duration easing-function after environment))

(defmacro ease-to (place to &key
                   (easing-function #'easing:linear)
                   (duration 1.0)
                   (after nil)
                   &environment environment)
  (with-gensyms (from)
    (multiple-value-bind (temps exprs stores store-expr access-expr)
        (get-setf-expansion place environment)
      (declare (ignore stores store-expr))
      `(let* (,@(mapcar #'list temps exprs)
              (,from ,access-expr))
         ,(ease% place from to duration easing-function after environment)))))

(defmacro chain-eases (&rest eases)
  (destructuring-bind (ease . remaining) eases
    (if (null remaining)
      ease
      (append ease `(:after (chain-eases ,@remaining))))))

(defun run-eases ()
  (setf *current-eases* (append *new-eases* *current-eases*)
        *new-eases* nil
        *current-eases* (delete-if #'funcall *current-eases*)))


;;;; Scratch ------------------------------------------------------------------
; (defparameter *foo* (cons 0 10))
; (pprint *foo*)

; (chain-eases
;   (ease-to (car *foo*) 1000 :duration 10)
;   (ease (cdr *foo*) 99 50 :duration 5.0)
;   (ease-to (car *foo*) 0.0 :duration 10.0))

; (progn (run-eases) (pprint *foo*))
