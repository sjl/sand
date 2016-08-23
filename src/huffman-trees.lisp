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

(define-with-macro node left right)


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

(defun length1p (list)
  "Return whether `list` has length 1, without traversing it all the way."
  (and (consp list) (null (cdr list))))


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

(defun encode (message tree)
  (labels
      ((fail (symbol)
         (error "Unknown symbol ~S" symbol))
       (encode-symbol (symbol tree)
         (recursively ((tree tree))
           (etypecase tree
             (leaf
               (if (eql symbol (leaf-symbol tree))
                 '()
                 (fail symbol)))
             (node
               (with-node (tree)
                 (cond
                   ((member symbol (tree-symbols left)) (cons 0 (recur left)))
                   ((member symbol (tree-symbols right)) (cons 1 (recur right)))
                   (t (fail symbol)))))))))
    (if (null message)
      '()
      (append (encode-symbol (first message) tree)
              (encode (rest message) tree)))))

(defun encode (message tree)
  ;; Alternate version
  (flet ((encode-symbol (symbol tree)
           (recursively ((tree tree))
             (etypecase tree
               (leaf (if (eql symbol (leaf-symbol tree))
                       '()
                       (error "Unknown symbol ~S" symbol)))
               (node (with-node (tree)
                       ;; If it's not in the left, assume it's in the right.  If
                       ;; it's not present at all we'll just recur all the way
                       ;; down to the rightmost leaf and let that handle the
                       ;; error.
                       ;;
                       ;; This saves a member check at each level, but doesn't
                       ;; bail early on garbage data.  One would hope garbage
                       ;; data is rare.
                       (if (member symbol (tree-symbols left))
                         (cons 0 (recur left))
                         (cons 1 (recur right)))))))))
    (if (null message)
      '()
      (append (encode-symbol (first message) tree)
              (encode (rest message) tree)))))


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
    (destructuring-bind (symbol . weight)
        (first pairs)
      (adjoin-set (make-leaf symbol weight)
                  (make-leaf-set (rest pairs))))))


(defun generate-huffman-tree (data)
  (check-type data cons)
  (labels ((successive-merge (trees)
             (if (length1p trees)
               (first trees)
               (destructuring-bind (a b . rest) trees
                 (successive-merge
                   (adjoin-set (make-node a b) rest))))))
    (successive-merge (make-leaf-set (hash-table-alist (frequencies data))))))


;;;; Scratch ------------------------------------------------------------------
(defparameter *sample-tree*
  (make-node (make-leaf 'a 4)
             (make-node (make-leaf 'b 2)
                        (make-node (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(defparameter *song*
  '(Well she was just seventeen
    You know what I mean
    And the way she looked was way beyond compare
    So how could I dance with another
    When I saw her standing there

    Well she looked at me and I I could see
    That before too long Id fall in love with her
    She wouldnt dance with another
    When I saw her standing there))

(defparameter *song-tree* (generate-huffman-tree *song*))



; (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) *sample-tree*)
; (encode '(A D A B B C A) *sample-tree*)
; (decode (encode '(d a b c a b) *sample-tree*) *sample-tree*)

; (decode (encode *song* *song-tree*) *song-tree*)
