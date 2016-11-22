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

(defun merge-with-temporary (vector temp lstart lsize rstart rsize
                             &key (predicate #'<))
  "Merge two consecutive subvectors destructively, using `temp` for storage."
  (let ((lend (+ lstart lsize))
        (rend (+ rstart rsize)))
    (recursively ((i 0) (l lstart) (r rstart))
      (cond
        ((= l lend) (replace temp vector ; Done with l, bulk-copy the rest of r
                             :start1 i
                             :start2 r :end2 rend))
        ((= r rend) (replace temp vector ; Done with r, bulk-copy the rest of l
                             :start1 i
                             :start2 l :end2 lend))
        ;; Take r only if it's strictly less than l, so we have a stable mege
        ((funcall predicate (aref vector r) (aref vector l))
         (setf (aref temp i) (aref vector r))
         (recur (1+ i) l (1+ r)))
        (t ; Otherwise l <= r, so take l
         (setf (aref temp i) (aref vector l))
         (recur (1+ i) (1+ l) r)))))
  (replace vector temp :start1 lstart :end2 (+ lsize rsize)))

(defun merge-sort (vector)
  "Merge-sort `vector` destructively.

  A single vector the same size as `vector` will be consed internally for
  temporary storage.

  "
  (let ((temp (make-array (length vector)))) ; just cons one temp array
    (recursively ((start 0)
                  (size (length vector)))
      (when (>= size 2)
        (let* ((half (floor size 2))
               (left-start start)
               (right-start (+ start half))
               (left-size half)
               (right-size (- size half)))
          (recur left-start left-size)
          (recur right-start right-size)
          (merge-with-temporary vector temp
                                left-start left-size
                                right-start right-size)))))
  vector)


(defparameter *v* #(1 3 7 0 2))
; (selection-sort *v*)
; (bubble-sort *v*)
; (merge-sort *v*)
