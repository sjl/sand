(in-package #:sand.graphs)


(defun make-edge (from to)
  (cons from to))

(defun edge-from (edge)
  (car edge))

(defun edge-to (edge)
  (cdr edge))

(defun edge= (test e1 e2)
  (and (funcall test (edge-from e1) (edge-from e2))
       (funcall test (edge-to e1) (edge-to e2))))


(defclass directed-graph ()
  ((edges :initarg :edges :accessor digraph-edges)
   (nodes :initarg :nodes :accessor digraph-nodes)
   (node-test :initarg :node-test :accessor digraph-node-test)
   (edge-test :initarg :edge-test :accessor digraph-edge-test)))

(defun make-directed-graph (&key (test #'eql))
  (make-instance 'directed-graph
                 :node-test test
                 :edge-test (curry #'edge= test)
                 :nodes nil
                 :edges nil))


(defun digraph-node= (digraph o1 o2)
  (funcall (digraph-node-test digraph) o1 o2))

(defun digraph-edge= (digraph e1 e2)
  (funcall (digraph-edge-test digraph) e1 e2))


(defun digraph-map-nodes (function digraph)
  (mapcar function (digraph-nodes digraph)))

(defun digraph-map-edges (function digraph)
  (iterate (for edge :in (digraph-edges digraph))
           (collect (funcall function (edge-from edge) (edge-to edge)))))

(defun digraph-filter-edges (predicate digraph &key (key 'identity))
  (remove-if-not predicate (digraph-edges digraph) :key key))


(defun digraph-edges-from (digraph object)
  (digraph-filter-edges (curry #'digraph-node= digraph object)
                        digraph
                        :key #'edge-from))

(defun digraph-edges-to (digraph object)
  (digraph-filter-edges (curry #'digraph-node= digraph object)
                        digraph
                        :key #'edge-to))

(defun digraph-edges-involving (digraph object)
  (digraph-filter-edges (lambda (edge)
                          (or (digraph-node= digraph object (edge-from edge))
                              (digraph-node= digraph object (edge-to edge))))
                        digraph))


(defun digraph-successors (digraph object)
  (mapcar #'edge-to (digraph-edges-from digraph object)))

(defun digraph-predecessors (digraph object)
  (mapcar #'edge-from (digraph-edges-to digraph object)))

(defun digraph-map-successors (function digraph object)
  (mapcar function (digraph-successors digraph object)))

(defun digraph-map-predecessors (function digraph object)
  (mapcar function (digraph-predecessors digraph object)))


(defun digraph-add-node (digraph object)
  (zapf (digraph-nodes digraph)
        (adjoin object % :test (digraph-node-test digraph))))

(defun digraph-add-edge (digraph from to)
  (zapf (digraph-edges digraph)
        (adjoin (make-edge from to) %
                :test (digraph-edge-test digraph))))

(defun digraph-remove-node (digraph object)
  (zapf (digraph-nodes digraph)
        (remove object % :test (digraph-node-test digraph))
        (digraph-edges digraph)
        (set-difference % (digraph-edges-involving digraph object)
                        :test (digraph-edge-test digraph)))
  nil)

(defun digraph-remove-edge (digraph from to)
  (zapf (digraph-edges digraph)
        (remove (make-edge from to) %
                :test (digraph-edge-test digraph)))
  nil)


(defmethod print-object ((digraph directed-graph) stream)
  (print-unreadable-object (digraph stream :type t :identity t)
    (when (not (null (digraph-nodes digraph)))
      (terpri stream)
      (digraph-map-nodes
        (lambda (node)
          (format stream "    ~S -> ~S~%"
                  node
                  (mapcar #'edge-to (digraph-edges-from digraph node))))
        digraph))))


