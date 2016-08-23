(in-package #:sand.binary-decision-diagrams)

(deftype non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))


;;;; Reference ----------------------------------------------------------------
(adt:defdata bdd
  (sink bit)
  (node non-negative-fixnum bdd bdd))

(defun make-bdd (contents)
  (etypecase contents
    (bit (sink contents))
    (cons
      (destructuring-bind (number low high) contents
        (node number (make-bdd low) (make-bdd high))))))


(defun evaluate-bdd (bdd &rest arguments)
  (recursively ((n 1)
                (bdd bdd)
                (argument (first arguments))
                (remaining (rest arguments)))
    (adt:match bdd bdd
      ((sink bit) bit)
      ((node number low high)
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
  (adt:match bdd bdd
    ((sink _)
     (list (funcall function bdd)))
    ((node _ low high)
     (append (list (funcall function bdd))
             (bdd-map-nodes function low)
             (bdd-map-nodes function high)))))

(defun bdd-map-edges (function bdd)
  (adt:match bdd bdd
    ((sink _) nil)
    ((node _ low high)
     (list* (funcall function bdd low t)
            (funcall function bdd high nil)
            (append (bdd-map-edges function low)
                    (bdd-map-edges function high))))))


(defun node-label (node)
  (adt:match bdd node
    ((sink bit) (if (zerop bit) 'false 'true))
    ((node number _ _) number)))

(defun node-shape (node)
  (adt:match bdd node
    ((sink _) :box)
    ((node _ _ _) :circle)))


(defun draw-bdd (bdd &optional (path "bdd.dot"))
  (let ((nodes (make-hash-table)))
    (graphviz-digraph
      (bdd-map-nodes (lambda (node)
                       (list (ensure-gethash node nodes (gensym))
                             :label (node-label node)
                             :shape (node-shape node)))
                     bdd)
      (bdd-map-edges (lambda (a b lowp)
                       (list (gethash a nodes)
                             (gethash b nodes)
                             :style (if lowp :dashed :solid)))
                     bdd)
      :path path)))


;;;; Scratch ------------------------------------------------------------------
(defparameter *maj*
  (make-bdd '(1
              (2 0 (3 0 1))
              (2 (3 0 1) 1))))


; (evaluate-bdd *maj* 1 0 1)

; (draw-bdd *maj* t)
