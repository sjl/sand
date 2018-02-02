(losh:eval-dammit
  (ql:quickload '(:plump :clss :parse-float)))

(defpackage :sand.qud
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils)
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
  (append (iterate (for part :in-whatever (clss:select "part" object-node))
                   (appending (build-part-attributes part)))
          (iterate (for tag :in-whatever (clss:select "tag" object-node))
                   (appending (build-part-attributes tag)))
          (iterate (for tag :in-whatever (clss:select "stat" object-node))
                   (appending (build-part-attributes tag)))))

(defun insert-object-into-index (object-node)
  (let ((name (symbolize (plump:attribute object-node "name")))
        (parent (symbolize (plump:attribute object-node "inherits"))))
    (setf (gethash name *index*)
          (list name parent (build-object-attributes object-node)))))

(defun build-object-index (object-nodes)
  (map nil #'insert-object-into-index object-nodes)
  (values))

(defun parent-name (name)
  (second (lookup-object name)))

(defun subtype-p (name parent-name)
  (if (eq name parent-name)
    t
    (if-let ((parent (parent-name name)))
      (subtype-p parent parent-name)
      nil)))

(defun level-to-tier (level)
  (when level
    (1+ (truncate (parse-integer level) 5))))

(defun lookup-tier (name)
  (let ((tier (lookup-attribute name 'tier.value))
        (level (lookup-attribute name 'level.value)))
    (if tier
      (parse-integer tier)
      (level-to-tier level))))

(defun lookup-tier-explicit (name)
  (let ((tier (lookup-attribute name 'tier.value :include-inherited nil))
        (level (lookup-attribute name 'level.value :include-inherited nil)))
    (if tier
      (parse-integer tier)
      (level-to-tier level))))

(defun lookup-object (name)
  (gethash name *index*))

(defun lookup-attribute (name attribute &key (include-inherited t))
  (destructuring-bind (_ parent attributes)
      (lookup-object name)
    (declare (ignore _))
    (or (getf attributes attribute)
        (when (and parent include-inherited)
          (lookup-attribute parent attribute
                            :include-inherited include-inherited)))))


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

(defun list-tiers ()
  (sort (iterate
          (for (object nil) :in-hashtable *index*)
          (for creature = (or (subtype-p object 'creature)
                              (subtype-p object 'plant)))
          (when creature
            (for tier = (lookup-tier object))
            (for etier = (lookup-tier-explicit object))
            (for name = (lookup-attribute object 'render.displayname))
            (when (null etier)
              (collect (list (or tier 0) object name)))))
        #'< :key #'first))

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

(defun dump-tiers ()
  (build-object-index *objects*)
  (write-string-into-file
    (with-output-to-string (*standard-output*)
      (print-table (cons '(tier item-key display-name) (list-tiers))))
    "qud-tiers.txt"
    :if-exists :supersede)
  (values))
