(in-package #:sand.binary-decision-diagrams)


(defun required ()
  (error "Argument required."))

(defstruct (bdd-node (:constructor make-bdd-node (number low high)))
  (number (required) :type fixnum)
  (low (required) :type (or bit bdd-node))
  (high (required) :type (or bit bdd-node)))

(defun make-bdd (contents)
  (etypecase contents
    (bit contents)
    (cons
      (destructuring-bind (number low high) contents
        (make-bdd-node number (make-bdd low) (make-bdd high))))))

(defun evaluate-bdd (bdd &rest arguments)
  (recursively ((n 1)
                (bdd bdd)
                (argument (first arguments))
                (remaining (rest arguments)))
    (etypecase bdd
      (bit bdd)
      (bdd-node
        (if (> (bdd-node-number bdd) n)
          (recur (1+ n)
                 bdd
                 argument
                 remaining)
          (recur (1+ n)
                 (if (zerop argument)
                   (bdd-node-low bdd)
                   (bdd-node-high bdd))
                 (first remaining)
                 (rest remaining)))))))

(defun bdd-map-nodes (function bdd)
  (etypecase bdd
    (bit (list (funcall function bdd)))
    (bdd-node
      (append (list (funcall function bdd))
              (bdd-map-nodes function (bdd-node-low bdd))
              (bdd-map-nodes function (bdd-node-high bdd))))))

(defun bdd-map-edges (function bdd)
  (etypecase bdd
    (bit nil)
    (bdd-node
      (let ((low (bdd-node-low bdd))
            (high (bdd-node-high bdd)))
        (list* (funcall function bdd low t)
               (funcall function bdd high nil)
               (append (bdd-map-edges function low)
                       (bdd-map-edges function high)))))))


(defun node-label (node)
  (etypecase node
    (bit (if (zerop node) 'false 'true))
    (bdd-node (bdd-node-number node))))

(defun node-shape (node)
  (etypecase node
    (bit :box)
    (bdd-node :circle)))


(defun draw-bdd (bdd &optional (path "bdd.dot"))
  (let ((nodes (make-hash-table)))
    (graphviz-digraph
      (bdd-map-nodes (lambda (node)
                       (list (gethash-or-init node nodes (gensym))
                             :label (node-label node)
                             :shape (node-shape node)))
                     bdd)
      (bdd-map-edges (lambda (a b lowp)
                       (list (gethash a nodes)
                             (gethash b nodes)
                             :style (if lowp :dashed :solid)))
                     bdd)
      :path path)))


(defparameter *maj*
  (make-bdd '(1
              (2 0 (3 0 1))
              (2 (3 0 1) 1))))

(draw-bdd *maj*)
