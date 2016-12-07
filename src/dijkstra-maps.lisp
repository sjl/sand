(in-package :sand.dijkstra-maps)


(defclass dijkstra-map ()
  ((map
     :initarg :map
     :accessor dm-map
     :type (simple-array single-float (* *)))
   (source
     :initarg :source
     :accessor dm-source
     :type (array t (* *)))
   (maximum-value
     :initarg :maximum-value
     :accessor dm-maximum-value
     :type single-float)
   (impassable-p
     :initarg :impassable-p
     :accessor dm-impassable-p
     :type function)
   (goal-p
     :initarg :goal-p
     :accessor dm-goal-p
     :type function)))


(defmethod print-object ((object dijkstra-map) stream)
  (destructuring-bind (rows cols)
      (array-dimensions (dm-map object))
    (print-unreadable-object (object stream :type t)
      (format stream "(~D ~D) (max ~F)~%" rows cols (dm-maximum-value object))
      (if (and (< cols 10)
               (< rows 30))
        (iterate
          (for row :from 0 :below rows)
          (iterate
            (for col :from 0 :below cols)
            (for val = (aref (dm-map object) row col))
            (if (= val most-positive-single-float)
              (format stream "   INF")
              (format stream " ~5,1F" val)))
          (terpri stream))
        (format stream "   ...very large array...")))))


(defun dm-ref (dm x y)
  (aref (dm-map dm) x y))


;;;; Reference
(defun make-dijkstra-map (array goal-p impassable-p)
  (let ((dm (make-instance 'dijkstra-map
                           :source array
                           :map (make-array (array-dimensions array))
                           :maximum-value 0.0
                           :impassable-p impassable-p
                           :goal-p goal-p)))
    (dm-recalculate dm)
    dm))


(defun array-index-in-bounds-p (array &rest subscripts)
  (iterate
    (for dimension :in (array-dimensions array))
    (for subscript :in subscripts)
    (always (< -1 subscript dimension))))

(defun array-neighboring-indices (array row col radius)
  (iterate
    (for (dr dc) :within-radius radius :skip-origin t)
    (for r = (+ row dr))
    (for c = (+ col dc))
    (when (array-index-in-bounds-p array r c)
      (collect (list r c)))))



;;;; Chili Dogs
(defun make-dijkstra-map (array goal-p impassable-p)
  (let ((dm (make-instance 'dijkstra-map
                           :source array
                           :map (make-array (array-dimensions array)
                                  :element-type 'single-float
                                  :initial-element 0.0
                                  :adjustable nil)
                           :maximum-value 0.0
                           :impassable-p impassable-p
                           :goal-p goal-p)))
    (dm-recalculate dm)
    dm))


(defun dm-recalculate (dm)
  (let* ((source (dm-source dm))
         (rows (first (array-dimensions source)))
         (cols (second (array-dimensions source)))
         (map (dm-map dm))
         (impassable-p (dm-impassable-p dm))
         (goal-p (dm-goal-p dm))
         (unset most-positive-single-float))
    (flet ((init-goals ()
             (iterate (for (val r c) :in-array source)
                      (when (funcall goal-p val)
                        (setf (aref map r c) 0.0)
                        (collect (list 0.0 r c)))))
           (find-neighbors (row col)
             (iterate
               (for (dr dc) :within-radius 1 :skip-origin t)
               (for nr = (+ row dr))
               (for nc = (+ col dc))
               (when (and (< -1 nr rows)
                          (< -1 nc cols))
                 (for nv = (aref source nr nc))
                 (unless (funcall impassable-p nv)
                   (collect (list (aref map nr nc) nr nc)))))))
      (fill-multidimensional-array-single-float map unset)
      (iterate
        main
        (for frontier
             :first (init-goals)
             :then (iterate
                     new-frontier
                     (for (value row col) :in frontier)
                     (iterate
                       (for (nvalue nrow ncol) :in (find-neighbors row col))
                       (when (= nvalue unset)
                         (let ((nvalue (+ 1.0 value)))
                           (setf (aref map nrow ncol) nvalue)
                           (in main (maximize nvalue :into max))
                           (in new-frontier (collect (list nvalue nrow ncol))))))))
        (while frontier)
        (finally (setf (dm-maximum-value dm) (float max)))))))



; (defparameter *m*
;   (make-array '(5 6)
;     :initial-contents (list (list 0 8 0 0 1 0)
;                             (list 0 0 1 0 1 0)
;                             (list 0 0 1 0 1 0)
;                             (list 0 0 1 0 0 0)
;                             (list 8 0 1 0 0 0))))


; (defparameter *d*
;   (make-dijkstra-map *m*
;                      (curry #'= 8)
;                      (curry #'= 1)))

