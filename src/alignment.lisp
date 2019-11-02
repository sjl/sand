(defpackage :sand.alignment
  (:use :cl :losh :iterate))

(in-package :sand.alignment)

;;;; An implementation of the Needleman-Wunsch global alignment algorithm.

(declaim (ftype (function (base-char base-char) fixnum) basic-score))

(defun basic-score (a b)
  (if (char= a b)
    1
    -1))

(defun make-needleman-wunsch-grid (rows cols)
  (let ((grid (make-array (list rows cols)
                :element-type 'fixnum
                :initial-element 0)))
    (iterate (for row :below rows)
             (for score :downfrom 0)
             (setf (aref grid row 0) score))
    (iterate (for col :below cols)
             (for score :downfrom 0)
             (setf (aref grid 0 col) score))
    grid))

(defun extract-path (prev)
  (iterate
    (with (rows cols) = (array-dimensions prev))
    (for coord
         :first (cons (1- rows) (1- cols)) ; start at the bottom right
         :then (aref prev (car coord) (cdr coord))) ; move according to the path
    (until (null coord))
    (collect coord :at :start)))

(defun compute-alignment (seq1 seq2 path)
  (macrolet
      ((mark (&body dest-value-pairs)
         `(progn
            ,@(loop :for (dest value) :on dest-value-pairs :by #'cddr
                    :collect `(collect ,value
                                :into ,dest
                                :result-type 'simple-base-string)))))
    (iterate
      (for (r . c) :in path)
      (for pr :previous r)
      (for pc :previous c)
      (if-first-time
        ;; Chew off the gap at the beginning of either sequence, if any.
        (progn (dotimes (i r)
                 (mark align1 #\-
                       middle #\Space
                       align2 (aref seq2 i)))
               (dotimes (i c)
                 (mark align1 (aref seq1 i)
                       middle #\Space
                       align2 #\-)))
        ;; Mark another character.
        (progn
          (for ch1 = (aref seq1 (1- c)))
          (for ch2 = (aref seq2 (1- r)))
          (mark align1 (if (= c pc) #\- ch1)
                align2 (if (= r pr) #\- ch2)
                middle (cond
                         ((or (= r pr) (= c pc)) #\space)
                         ((char= ch1 ch2) #\|)
                         (t #\x)))))
      (finally (return (values align1 middle align2))))))

(defun print-grid (seq1 seq2 grid &key (pad 4))
  (flet ((print-padded (seq)
           (map nil (lambda (el) (format t "~V@A" pad el)) seq))
         (row-of (array row) ; ugly hack, but just for debugging…
           (iterate
             (for col :below (array-dimension array 1))
             (collect (aref array row col)))))
    (print-padded "  ")
    (print-padded seq1)
    (terpri)
    (iterate
      (for row :below (array-dimension grid 0))
      (print-padded (list (if (plusp row)
                            (aref seq2 (1- row))
                            #\Space)))
      (print-padded (row-of grid row))
      (terpri))))

(defun needleman-wunsch (seq1 seq2 &key
                         (score-function #'basic-score)
                         (gap-score -1))
  (setf seq1 (coerce seq1 'simple-base-string)
        seq2 (coerce seq2 'simple-base-string))
  (iterate
    (with rows = (1+ (length seq2)))
    (with cols = (1+ (length seq1)))
    (with grid = (make-needleman-wunsch-grid rows cols))
    (with prev = (make-array (list rows cols) :initial-element nil))
    (for row :from 1 :below rows)
    (for ch2 :in-string seq2)
    (iterate
      (for col :from 1 :below cols)
      (for ch1 :in-string seq1)
      (for row-1 = (1- row))
      (for col-1 = (1- col))
      (for match = (+ (aref grid row-1 col-1)
                      (funcall score-function ch1 ch2)))
      (for gap-1 = (+ (aref grid row col-1) gap-score))
      (for gap-2 = (+ (aref grid row-1 col) gap-score))
      (for best = (max match gap-1 gap-2))
      (setf (aref grid row col) best
            (aref prev row col) (cond
                                  ((= best match) (cons row-1 col-1))
                                  ((= best gap-1) (cons row col-1))
                                  ((= best gap-2) (cons col-1 row)))))
    (finally
      (print-grid seq1 seq2 grid)
      (return
        (compute-alignment seq1 seq2 (extract-path prev))))))



;; (needleman-wunsch
;;   "GGGGTTATAAAAC"
;;   "GGTAT")
