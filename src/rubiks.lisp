(in-package #:sand.rubiks)

; (declaim (optimize (speed 1) (safety 3) (debug 3)))

(deftype cube () '(simple-array t (6 9)))

(defun make-solved-cube ()
  (make-array '(6 9)
    :adjustable nil
    ; :element-type 'symbol
    :initial-contents
    '((w w w w w w w w w) ; top
      (g g g g g g g g g) ; left
      (r r r r r r r r r) ; front
      (b b b b b b b b b) ; right
      (o o o o o o o o o) ; back
      (y y y y y y y y y)))) ; down


(defun face-index (face)
  (case face
    (:top 0)
    (:left 1)
    (:front 2)
    (:right 3)
    (:back 4)
    (:down 5)))


(defmacro defaccessor (name face)
  `(defmacro ,name (cube n)
    `(aref ,cube ,(face-index ,face) ,n)))


(defaccessor top :top)
(defaccessor down :down)
(defaccessor left :left)
(defaccessor right :right)
(defaccessor front :front)
(defaccessor back :back)

(defmacro define-move (name &rest groups)
  (flet ((access (spec)
           `(,(first spec) cube ,(second spec))))
    `(progn
      (declaim (ftype (function (cube) null) ,name))
      (defun ,name (cube)
        ,@(mapcar (lambda (group)
                    `(rotatef ,@(mapcar #'access group)))
                  groups)
        nil))))

;       0 1 2
;       3 4 5
;       6 7 8
; 0 1 2 0 1 2 0 1 2 0 1 2
; 3 4 5 3 4 5 3 4 5 3 4 5
; 6 7 8 6 7 8 6 7 8 6 7 8
;       0 1 2
;       3 4 5
;       6 7 8

(define-move move-front
  ((front 0) (front 6) (front 8) (front 2))
  ((front 1) (front 3) (front 7) (front 5))
  ((top 6) (left 8) (down 2) (right 0))
  ((top 7) (left 5) (down 1) (right 3))
  ((top 8) (left 2) (down 0) (right 6)))

(define-move move-top
  ((top 0) (top 6) (top 8) (top 2))
  ((top 1) (top 3) (top 7) (top 5))
  ((front 0) (right 0) (back 0) (left 0))
  ((front 1) (right 1) (back 1) (left 1))
  ((front 2) (right 2) (back 2) (left 2)))

(define-move move-left
  ((left 0) (left 6) (left 8) (left 2))
  ((left 1) (left 3) (left 7) (left 5))
  ((front 0) (top 0) (back 2) (down 0))
  ((front 3) (top 3) (back 5) (down 3))
  ((front 6) (top 6) (back 8) (down 6)))

(define-move move-right
  ((right 0) (right 6) (right 8) (right 2))
  ((right 1) (right 3) (right 7) (right 5))
  ((front 2) (down 2) (back 8) (top 2))
  ((front 5) (down 5) (back 5) (top 5))
  ((front 8) (down 8) (back 2) (top 8)))



(defun print-cube (cube)
  (flet ((pad ()
           (format t "      "))
         (row (face row)
           (format t "~A ~A ~A "
                   (aref cube (face-index face) (+ 0 (* row 3)))
                   (aref cube (face-index face) (+ 1 (* row 3)))
                   (aref cube (face-index face) (+ 2 (* row 3))))))
    (pad) (row :top 0) (terpri)
    (pad) (row :top 1) (terpri)
    (pad) (row :top 2) (terpri)
    (row :left 0) (row :front 0) (row :right 0) (row :back 0) (terpri)
    (row :left 1) (row :front 1) (row :right 1) (row :back 1) (terpri)
    (row :left 2) (row :front 2) (row :right 2) (row :back 2) (terpri)
    (pad) (row :down 0) (terpri)
    (pad) (row :down 1) (terpri)
    (pad) (row :down 2) (terpri))
  (values))


(defparameter *c* (make-solved-cube))
; (move-front *c*)
; (move-top *c*)
; (move-left *c*)
; (move-right *c*)
; (print-cube *c*)
