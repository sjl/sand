(losh:eval-dammit
  (ql:quickload '(:plump :clss :parse-float)))

(defpackage :sand.qud
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export))

(in-package :sand.qud)

(setf *print-length* 10)

(defparameter *object-blueprints-path*
  #p"/Users/sjl/Library/Application Support/Steam/steamapps/common/Caves of Qud/CoQ.app/Contents/Resources/Data/StreamingAssets/Base/ObjectBlueprints.xml")

(defparameter *blueprints*
  (plump:parse *object-blueprints-path*))

(defparameter *objects*
  (clss:select "object" *blueprints*))

(defparameter *index* (make-hash-table))

(defun symbolize (string)
  (if (null string)
    nil
    (-<> string
      (string-upcase <>)
      (substitute #\- #\Space <>)
      (intern <>)
      (nth-value 0 <>))))

(defun build-part-attributes (part-node)
  (iterate (with attrs = (plump:attributes part-node))
           (with name = (symbolize (gethash "name" attrs)))
           (for (key val) :in-hashtable (plump:attributes part-node))
           (unless (string-equal "name" key)
             (collect (symb name "." (symbolize key)))
             (collect val))))

(defun build-object-attributes (object-node)
  (iterate (for part :in-whatever (clss:select "part" object-node))
           (appending (build-part-attributes part))))

(defun insert-object-into-index (object-node)
  (let ((name (symbolize (plump:attribute object-node "name")))
        (parent (symbolize (plump:attribute object-node "inherits"))))
    (setf (gethash name *index*)
          (list name parent (build-object-attributes object-node)))))

(defun build-object-index (object-nodes)
  (map nil #'insert-object-into-index object-nodes)
  (values))


(defun lookup-object (name)
  (gethash name *index*))

(defun lookup-attribute (name attribute)
  (destructuring-bind (_ parent attributes)
      (lookup-object name)
    (declare (ignore _))
    (or (getf attributes attribute)
        (when parent
          (lookup-attribute parent attribute)))))


(defun floatize (string)
  (when string (parse-float:parse-float string)))

(defun list-prices ()
  (iterate
    (for (object nil) :in-hashtable *index*)
    (for price = (floatize (lookup-attribute object 'commerce.value)))
    (for weight = (floatize (lookup-attribute object 'physics.weight)))
    (for name = (lookup-attribute object 'render.displayname))
    (when (and price weight (plusp weight))
      (collect (list (/ price weight) object name price weight)))))

(defun list-best-prices ()
  (let ((prices (list-prices)))
    (sort prices #'> :key #'first)))

(defun dump ()
  (build-object-index *objects*)
  (write-string-into-file
    (with-output-to-string (*standard-output*)
      (print-table (cons '(price/weight item-key display-name price weight)
                         (list-best-prices))))
    "qud-items.txt"
    :if-exists :supersede)
  (values))
