(in-package #:sand.huffman-trees)

;;;; Data ---------------------------------------------------------------------
;;; Interface:
;;;     Constructors: make-leaf make-node
;;;     Generic: tree-symbols tree-weight
;;;     Leaves: leaf-symbol leaf-weight
;;;     Nodes: node-left node-right
;;;
;;; SICP's abstraction layer is a little wonky in this example:
;;;
;;;    (define (make-leaf symbol weight) (list 'leaf symbol weight))
;;;    (define (leaf? object) (eq? (car object) 'leaf))
;;;
;;;    (define (symbol-leaf x) (cadr x))
;;;    (define (weight-leaf x) (caddr x))
;;;
;;;    (define (make-code-tree left right) ...)
;;;
;;;    (define (left-branch tree) (car tree))
;;;    (define (right-branch tree) (cadr tree))
;;;
;;;    (define (symbols tree)
;;;      (if (leaf? tree) ...))
;;;
;;;    (define (weight tree)
;;;      (if (leaf? tree) ...))
;;;
;;; Okay, so `symbols` and `weight` are generic functions that operate on either
;;; kind of tree component (leaves and code trees), cool.  Their argument is
;;; just called `tree` so that must mean "either kind of component".
;;;
;;; But wait, `left-branch` takes a "tree" argument, but it only works on code
;;; trees, not leaves.  Same for `right-branch`.
;;;
;;; Sometimes I just want to drop everything and go write OCaml.

(defstruct huffman-tree)

(defstruct (leaf (:include huffman-tree)
                 (:constructor make-leaf (symbol weight)))
  (symbol (required-argument))
  (weight (required-argument) :type real))

(defstruct (node (:include huffman-tree)
                 (:constructor %make-node))
  (left (required-argument) :type huffman-tree)
  (right (required-argument) :type huffman-tree)
  (symbols (required-argument) :type list)
  (weight (required-argument) :type real))


(defun tree-symbols (tree)
  (etypecase tree
    (leaf (list (leaf-symbol tree)))
    (node (node-symbols tree))))

(defun tree-weight (tree)
  (etypecase tree
    (leaf (leaf-weight tree))
    (node (node-weight tree))))


(defun make-node (left right)
  (%make-node :left left
              :right right
              :weight (+ (tree-weight left)
                         (tree-weight right))
              :symbols (append (tree-symbols left)
                               (tree-symbols right))))


;;;; External Interface -------------------------------------------------------
(defun decode (bits tree)
  (flet ((choose-branch (bit tree)
           (ecase bit
             (0 (node-left tree))
             (1 (node-right tree)))))
    (recursively ((bits bits)
                  (current tree))
      (when bits
        (let ((next-branch (choose-branch (first bits) current)))
          (etypecase next-branch
            (leaf (cons (leaf-symbol next-branch)
                        (recur (rest bits) tree)))
            (node (recur (rest bits) next-branch))))))))


(defun adjoin-set (tree set)
  (cond
    ((null set)
     (list tree))
    ((< (tree-weight tree) (tree-weight (first set)))
     (cons tree set))
    (t
     (cons (first set)
           (adjoin-set tree (rest set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
    '()
    (destructuring-bind (symbol weight)
        (first pairs)
      (adjoin-set (make-leaf symbol weight)
                  (make-leaf-set (rest pairs))))))


(defparameter *sample-tree*
  (make-node (make-leaf 'a 4)
             (make-node (make-leaf 'b 2)
                        (make-node (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


; (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) *sample-tree*)
