(in-package #:sand.markov)

(defparameter *text* (slurp "data/lightships-and-lighthouses.txt"))

(defclass markov ()
  ((database :initarg :database :accessor markov-database)
   (beginnings :initarg :beginnings :accessor markov-beginnings)))


(defun make-vector (&optional (initial-length 1))
  (make-array initial-length :fill-pointer 0 :adjustable t))


(defun delimiterp (c)
  (member c '(#\space #\newline) :test #'char=))

(defun sentence-end-p (word)
  (member (aref word (1- (length word)))
          '(#\. #\? #\!)))


(defun split-words (string)
  (split-sequence-if #'delimiterp string :remove-empty-subseqs t))

(defun partition-if (pred seq)
  (iterate
    (for element :in seq)
    (collect element :into current)
    (when (funcall pred element)
      (collect current :into result)
      (setf current nil))
    (finally (return result))))


(defun build-markov-generator (corpus order)
  (let* ((database (make-hash-table :test 'equal))
         (beginnings nil)
         (words (split-words corpus))
         (sentences (partition-if #'sentence-end-p words)))
    (iterate
      (for sentence :in sentences)
      (when (> (length sentence) order)
        (iterate
          (for chunk :in (n-grams (1+ order) sentence))
          (for prefix = (take order chunk))
          (for suffix = (car (last chunk)))
          (if-first-time (pushnew prefix beginnings :test 'equal))
          (vector-push-extend
            suffix
            (gethash-or-init prefix database (make-vector))))))
    (make-instance 'markov
                   :database database
                   :beginnings (coerce beginnings 'vector))))


(defun generate-sentence (markov)
  (iterate
    (repeat 50)
    (with start = (random-elt (markov-beginnings markov)))
    (for prefix :first start :then (append (cdr prefix) (list word)))
    (for word = (random-elt (gethash prefix (markov-database markov))))
    (collect word :into sentence)
    (until (sentence-end-p word))
    (finally (return (format nil "~{~A~^ ~}" (append start sentence))))))



(defparameter *m* (build-markov-generator *text* 2))


(iterate (repeat 10)
         (terpri)
         (terpri)
         (princ (generate-sentence *m*)))
