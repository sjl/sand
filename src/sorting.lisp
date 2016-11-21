(in-package :sand.sorting)


;; http://cdn.cs50.net/2015/fall/lectures/3/m/notes3m/notes3m.html


(defun bubble-sort (vector)
  (iterate
    (with size = (length vector))
    (for done = t)
    (iterate (for i :from 0 :below (1- size))
             (for j = (1+ i))
             (when (< (aref vector j) (aref vector i))
               (rotatef (aref vector i)
                        (aref vector j))
               (setf done nil)))
    (until done))
  vector)


(defun selection-sort (vector)
  (iterate
    (for target :index-of-vector vector)
    (for smallest = (iterate
                      (for value :in-vector vector :from target :with-index i)
                      (finding i minimizing value)))
    (prl target smallest)
    (rotatef (aref vector target)
             (aref vector smallest)))
  vector)



(defparameter *v* #(1 3 7 0 2 1 4))
; (selection-sort *v*)
; (bubble-sort *v*)
