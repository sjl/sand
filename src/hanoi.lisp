(in-package :sand.hanoi)

(defun move (disc from to)
  (format t "Move disc ~D from ~D to ~D~%" disc from to))

(defun hanoi (n)
  (recursively ((disc n)
                (from 1)
                (to 3)
                (using 2))
    (when (plusp disc)
      (recur (1- disc) from using to)
      (move disc from to)
      (recur (1- disc) using to from)))
  (values))

