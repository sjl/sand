(losh:eval-dammit
  (ql:quickload '(:trivial-ppm)))

(defpackage :sand.turing-omnibus.wallpaper
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))


(in-package :sand.turing-omnibus.wallpaper)

;;;; From The New Turing Omnibus, Chapter 1

(defun draw (width height &optional (side 200.0d0) (colors 2))
  (declare (optimize speed)
           (type (integer 2 4) colors)
           (type (integer 0 50000) width height)
           (type double-float side))
  (let ((image (make-array (list width height)))
        (palette #(#(86 50 16)
                   #(255 209 105)
                   #(204 132 76)
                   #(165 144 125))))
    (time
      (dotimes (i width)
        (dotimes (j height)
          (let* ((x (* i (/ side width)))
                 (y (* j (/ side height)))
                 (c (truncate (+ (* x x) (* y y)))))
            (setf (aref image i j)
                  (elt palette (mod c colors)))))))
    (time (trivial-ppm:write-to-file "wallpaper.ppm" image
                                     :format :ppm
                                     :if-exists :supersede))))



