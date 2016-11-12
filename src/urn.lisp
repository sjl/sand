(in-package :sand.urn)

(defun make-vec-from (contents)
  (make-array (length contents)
    :initial-contents contents
    :fill-pointer (length contents)
    :adjustable t))

(defun make-empty-vec (size)
  (make-array size
    :fill-pointer 0
    :adjustable t))


(defun sample (population size)
  (let ((result (make-empty-vec size)))
    (dotimes (_ size)
      (vector-push-extend (random-elt population) result))
    result))

(defun unsample (sample target-size)
  (recursively ((sample sample))
    (if (= (length sample) target-size)
      sample
      (progn (vector-push-extend (random-elt sample) sample)
             (recur sample)))))



(defparameter *population*
  (make-vec-from
    (append (make-list 1000 :initial-element 'a)
            (make-list 3000 :initial-element 'b)
            (make-list 500 :initial-element 'c))))


; (print-hash-table (frequencies *population*))
; (print-hash-table (proportions *population* :float t))

; (-<> *population*
;   (sample <> 100)
;   (unsample <> 1000)
;   (proportions <> :float t)
;   (print-hash-table <>)
;   )

; (-<> *population*
;   (sample <> 100)
;   (proportions <> :float nil)
;   (print-hash-table <>)
;   )

