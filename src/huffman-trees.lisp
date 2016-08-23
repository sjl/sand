(in-package #:sand.huffman-trees)

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


(defun huffman-tree-symbols (tree)
  (etypecase tree
    (leaf (list (leaf-symbol tree)))
    (node (node-symbols tree))))

(defun huffman-tree-weight (tree)
  (etypecase tree
    (leaf (leaf-weight tree))
    (node (node-weight tree))))


(defun make-node (left right)
  (%make-node :left left
              :right right
              :weight (+ (huffman-tree-weight left)
                         (huffman-tree-weight right))
              :symbols (append (huffman-tree-symbols left)
                               (huffman-tree-symbols right))))


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
    ((< (huffman-tree-weight tree) (huffman-tree-weight (first set)))
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
