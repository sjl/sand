(in-package #:sand.graphviz)

(defun graphviz-node (id &key (label id) (shape :box))
  (format t "    ~A [shape=~(~A~),label=\"~A\"];~%"
          id shape label))

(defun graphviz-edge (from-id to-id &key (label "") (style :solid))
  (format t "    ~A -> ~A [style=~(~A~),label=\"~A\"];~%"
          from-id to-id style label))

(defun %graphviz-digraph (nodes edges)
  (format t "digraph G {~%")
  (mapc (curry #'apply #'graphviz-node) nodes)
  (mapc (curry #'apply #'graphviz-edge) edges)
  (format t "}~%"))

(defun graphviz-digraph (nodes edges &key (path t))
  "Output some Graphviz code to draw a digraph.

  If `path` is `t`, output to a string.  If `nil`, return a string.  Otherwise
  it should be a path designator, and the code will be spit to that file.

  Each element in `nodes` will have `graphviz-node` applied to it, so they
  should look like this:

    (node-id)
    (node-id :label \"foo\" :shape :circle)

  Each element in `edges` will have `graphviz-edge` applied to it, so they
  should look like this:

    (from-node-id to-node-id)
    (from-node-id to-node-id :style :dashed :label \"bar\")

  "
  (case path
    ((t) (%graphviz-digraph nodes edges))
    ((nil) (with-output-to-string (*standard-output*)
             (%graphviz-digraph nodes edges)))
    (t (with-open-file (*standard-output* path
                                          :direction :output
                                          :if-exists :supersede)
         (%graphviz-digraph nodes edges)))))
