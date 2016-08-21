(in-package #:sand.binary-decision-diagrams)


(defun required ()
  (error "Argument required."))


(defstruct (bdd (:constructor %make-bdd (number low high)))
  (number (required) :type fixnum)
  (low (required) :type (or bit bdd))
  (high (required) :type (or bit bdd)))

(defun make-bdd (contents)
  (etypecase contents
    (bit contents)
    (cons
      (destructuring-bind (number low high) contents
        (%make-bdd number (make-bdd low) (make-bdd high))))))

(defmacro bdd-case (bdd
                    ((sink) &body sink-body)
                    ((number low high) &body node-body))
  (once-only (bdd)
    `(etypecase ,bdd
      (bit (let ((,sink ,bdd))
             (declare (ignorable ,sink))
             ,@sink-body))
      (bdd (with-accessors ((,number bdd-number)
                            (,low bdd-low)
                            (,high bdd-high))
               ,bdd
             ,@node-body)))))


(defun evaluate-bdd (bdd &rest arguments)
  (recursively ((n 1)
                (bdd bdd)
                (argument (first arguments))
                (remaining (rest arguments)))
    (bdd-case bdd
      ((sink) sink)
      ((number low high)
       (if (> number n)
          (recur (1+ n)
                 bdd
                 argument
                 remaining)
          (recur (1+ n)
                 (if (zerop argument)
                   low
                   high)
                 (first remaining)
                 (rest remaining)))))))


(defun bdd-map-nodes (function bdd)
  (bdd-case bdd
    ((sink)
     (list (funcall function sink)))
    ((n low high)
     (append (list (funcall function bdd))
             (bdd-map-nodes function low)
             (bdd-map-nodes function high)))))

(defun bdd-map-edges (function bdd)
  (bdd-case bdd
    ((sink) nil)
    ((n low high)
     (list* (funcall function bdd low t)
            (funcall function bdd high nil)
            (append (bdd-map-edges function low)
                    (bdd-map-edges function high))))))


(defun node-label (node)
  (bdd-case node
    ((sink) (if (zerop sink) 'false 'true))
    ((number low high) number)))

(defun node-shape (node)
  (bdd-case node
    ((sink) :box)
    ((n l h) :circle)))


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
