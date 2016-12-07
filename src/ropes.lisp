(in-package :sand.ropes)

;;;; De-crazifying Trivia's struct pattern matching
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun struct% (name vars-and-accessors)
    (with-gensyms (instance)
      `(guard1
        (,instance :type ,name) (typep ,instance ',name)
        ,@(iterate (for (var accessor) :in vars-and-accessors)
                   (unless (string= (symbol-name var) "_")
                     (collect `(,accessor ,instance))
                     (collect `(guard1 ,var t)))))))

  (defun find-accessors (name-and-options slots-and-vars)
    (destructuring-bind
        (name &key (conc-name (symb name '-)))
        name-and-options
      (iterate (for (slot var) :in slots-and-vars)
               (for accessor = (symb conc-name slot))
               (collect (list var accessor))))))

(defpattern struct (name-and-options &rest slots)
  (let ((name-and-options (ensure-list name-and-options)))
    (struct%
      (first name-and-options)
      (when slots
        (etypecase (first slots)
          (keyword (find-accessors name-and-options (subdivide slots 2)))
          (symbol (find-accessors name-and-options (mapcar #'list slots slots)))
          (cons slots))))))


;;;; Ropes --------------------------------------------------------------------
(deftype rope ()
  '(or simple-string concat))

(deftype non-negative-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(deftype array-index ()
  `(integer 0 ,array-dimension-limit))


(defstruct (concat (:constructor make-concat%))
  (size 0 :type non-negative-fixnum :read-only t)
  (left-size 0 :type non-negative-fixnum :read-only t)
  (left nil :type rope :read-only t)
  (right nil :type (or null rope) :read-only t))

(defpattern concat (size left-size left right)
  `(struct concat :size ,size :left-size ,left-size :left ,left :right ,right))


(declaim (ftype (function (*) array-index) size))

(defun-ematch size (rope)
  ((type simple-string) (length rope))
  ((concat size _ _ _) size))

(defun minimalp (left right)
  (and (typep left 'simple-string)
       (typep right 'simple-string)
       (< (+ (length left)
             (length right))
          25)
       t))

(defun make-concat (left right)
  (cond ((equal right "") left)
        ((equal left "") right)
        ((minimalp left right) (concatenate 'string left right))
        (t (make-concat% :size (+ (size left) (size right))
                         :left-size (size left)
                         :left left
                         :right right))))


(defmacro sanity-check ((rope &optional indexes char)
                        &body body)
  `(progn
    (check-type ,rope rope)
    ,@(when char
        `((check-type ,char character)))
    ,@(when indexes
        (mapcar (lambda (i) `(check-type ,i array-index))
                indexes))
    (locally
      (declare (type rope ,rope)
               ,@(when indexes `((type array-index ,@indexes)))
               ,@(when char `((type character ,char))))
      ,@(mapcar (lambda (i) `(assert (< ,i (size ,rope))
                              (,rope ,i)
                              "Index ~D is out of bounds for rope ~S"
                              ,i ,rope))
                indexes)
      ,@body)))


;;;; Lookup
(declaim (ftype (function (rope array-index) (values character &optional))
                lookup%))
(defun lookup% (rope index)
  ; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ematch rope
    ((type simple-string)
     (aref rope index))

    ((concat _ left-size left right)
     (if (< index left-size)
       (lookup% left index)
       (lookup% right (- index left-size))))))

(defun lookup (rope index)
  (sanity-check (rope (index))
    (lookup% rope index)))


;;;; Set
(defun set-char% (rope index new-char)
  (ematch rope
    ((type string) (let ((result (copy-seq rope)))
                     (setf (aref result index) new-char)
                     result))
    ((concat _ left-size left right)
     (if (< index left-size)
       (make-concat (set-char left index new-char)
                    right)
       (make-concat left
                    (set-char right (- index left-size) new-char))))))

(defun set-char (rope index new-char)
  (sanity-check (rope (index) new-char)
    (set-char% rope index new-char)))


;;;; Concat
(defun rope-concat (left right)
  (sanity-check (left)
    (sanity-check (right)
      (make-concat left right))))


;;;; Substring
(declaim (ftype (function (rope array-index array-index)
                          (values rope &optional))
                rope-substring%))

(defun rope-substring% (rope start end)
  ; (declare (optimize (speed 3) (safety 0) (debug 0)))
  (etypecase rope
    (simple-string (subseq rope start end))
    (concat (let ((ls (concat-left-size rope)))
              (cond ((< end ls) (rope-substring% (concat-left rope)
                                                 start
                                                 end))
                    ((>= start ls) (rope-substring% (concat-right rope)
                                                    (- start ls)
                                                    (- end ls)))
                    (t (make-concat
                         (rope-substring% (concat-left rope) start ls)
                         (rope-substring% (concat-right rope) 0 (- end ls)))))))))

(defun rope-substring (rope start end)
  (sanity-check (rope (start end))
    (rope-substring% rope start end)))


;;;; Stringifying
(declaim (ftype (function (rope) (values simple-string &optional))
                rope-to-string%))

(defun rope-to-string% (rope)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ematch rope
    ((type simple-string) rope)
    ((concat _ _ left right)
     (concatenate 'string
                  (rope-to-string% left)
                  (rope-to-string% right)))))

(defun rope-to-string (rope)
  (sanity-check (rope)
    (rope-to-string% rope)))


;;;; Scratch
(defparameter *r* (rope-concat "foo" "bar"))
