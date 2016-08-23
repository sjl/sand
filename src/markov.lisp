(in-package #:sand.markov)

(defparameter *text*
  (read-file-into-string "data/lightships-and-lighthouses.txt"))

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
            (ensure-gethash prefix database (make-vector))))))
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



(defun firebase-get (url)
  (-> url
    drakma:http-request
    (flex:octets-to-string :external-format :utf-8)
    yason:parse))

(defun hn-top ()
  (firebase-get "https://hacker-news.firebaseio.com/v0/topstories.json"))

(defun hn-item (id)
  (firebase-get
    (format nil "https://hacker-news.firebaseio.com/v0/item/~d.json" id)))

(defun hn-story (story-id)
  (hn-item story-id))

(defun hn-comment (story-id)
  (hn-item story-id))

(defun hn-text (comment)
  (-> (gethash "text" comment)
    sanitize:clean
    html-entities:decode-entities))

(defparameter *errors* 0)

(defun hn-comments (story-id)
  (iterate
    (with story = (hn-story story-id))
    (with children = (gethash "kids" story))
    (repeat 50)
    ; (sleep 0.1)
    (while children)
    (for child-id = (pop children))
    (for child = (handler-case (hn-comment child-id)
                   (drakma::drakma-simple-error () (incf *errors*) nil)))
    (when child
      (collect child)
      (setf children (append children (gethash "kids" child))))))

(defvar *hn* nil)

(defun build-hn-corpus ()
  (length (setf *hn* (-<> (hn-top)
                       (take 15 <>)
                       (mapcan #'hn-comments <>)
                       (mapcar #'hn-text <>)
                       (format nil "~{~a~%~}" <>)))))


(defun ratebeer-get (page)
  (-<> (format nil "http://www.ratebeer.com/beer-ratings/0/~d/" page)
    drakma:http-request
    plump:parse))

(defun ratebeer-clean (raw)
  (-<> raw
    (plump:get-elements-by-tag-name <> "table")
    car
    (plump:get-elements-by-tag-name <> "td")
    (mapcar (rcurry #'plump:get-elements-by-tag-name "span") <>)
    (remove-if-not #'identity <>)
    (mapcar #'first <>)
    (mapcar #'plump:text <>)))

(defvar *beer* nil)

(defun build-beer-corpus ()
  (length
    (setf *beer*
          (iterate
            (for page :from 1 :to 30)
            (appending (ratebeer-clean (ratebeer-get page)) :into reviews)
            (finally (return (format nil "~{~A~%~}" reviews)))))))



(defun wine-get-list (page-number)
  (-<> (format nil "http://www.winemag.com/?s=&drink_type=wine&page=~D"
               page-number)
    drakma:http-request
    plump:parse))

(defun wine-get-review (url)
  (-<> url
    drakma:http-request
    plump:parse))


(defun wine-clean-list (list-page)
  (-<> list-page
    (clss:select "a.review-listing" <>)
    (map 'list (rcurry #'plump:attribute "href") <>)))

(defun wine-clean-review (review-page)
  (plump:text (elt (clss:select "#review .description" review-page) 0)))


(defparameter *wine* nil)
(defun build-wine-corpus ()
  (length
    (setf *wine*
          (iterate
            (for page :from 1 :to 10)
            (for review-links = (wine-clean-list (wine-get-list page)))
            (appending (mapcar (compose #'wine-clean-review #'wine-get-review)
                               review-links)
                       :into reviews)
            (finally (return (format nil "~{~A~%~}" reviews)))))))

; (defparameter *m*
;   (build-markov-generator (concatenate 'string *hn* *wine*) 2))


; (iterate (repeat 50)
;          (for sentence = (generate-sentence *m*))
;          (when (<= (length sentence) 140)
;            (terpri)
;            (terpri)
;            (princ sentence)))
