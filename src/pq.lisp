(in-package :sand.pq)

;;;; Priority Queue -----------------------------------------------------------
;;; Jankass priority queue implementation.
(defstruct pq
  (contents nil)
  (predicate #'<)
  (test #'eql))


(defun pqn-priority (node)
  (first node))

(defun pqn-element (node)
  (second node))


(defun pq-resort (pq)
  (zapf (pq-contents pq)
        (sort % (pq-predicate pq) :key #'pqn-priority))
  pq)

(defun pq-lookup (pq element)
  (find element (pq-contents pq)
        :key #'pqn-element
        :test (pq-test pq)))

(defun pq-insert (pq element priority)
  (zapf (pq-contents pq)
        (merge 'list `((,priority ,element)) % (pq-predicate pq)
               :key #'pqn-priority))
  pq)

(defun pq-ensure (pq element priority)
  (let ((existing (pq-lookup pq element)))
    (if existing
      (progn (setf (car existing) priority)
             (pq-resort pq)
             t)
      (progn (pq-insert pq element priority)
             nil)))
  pq)

(defun pq-dequeue (pq)
  (if (pq-contents pq)
    (values (pqn-element (pop (pq-contents pq))) t)
    (values nil nil)))

