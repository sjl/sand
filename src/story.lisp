(in-package :sand.story)

;;; Basically a Lispy version of Tracery https://github.com/galaxykate/tracery
;;; without the nutty string-parsing stuff.
;;;
;;; (define-rule name ...expressions...)
;;;
;;; strings evaluate to themselves: "foo bar" -> "foo bar"
;;;
;;; symbols funcall their symbol-function: animal -> "mouse"
;;;
;;; lists evaluate their contents and concatenate them with spaces in between:
;;;     ("foo" animal "bar") -> "foo mouse bar"
;;;
;;; the magic keyword :. inside a list suppresses the space there:
;;;
;;;     ("foo" "bar" :. "baz") -> "foo barbaz"
;;;
;;; vectors evaluate the head and pipe it through all the functions in the tail:
;;;
;;;     #(animal capitalize pos) -> "Mouse's"


;;;; Utils ---------------------------------------------------------------------
(defun emptyp (string)
  (zerop (length string)))

(defun cat (&rest strings)
  "Concatenate `strings` into a string."
  (apply #'concatenate 'string strings))

(defun ch (string index)
  "Return the character of `string` at `index`.  Allows negative indices."
  (if (emptyp string)
    nil
    (aref string (if (minusp index)
                   (+ (length string) index)
                   index))))

(defun chop (string n)
  "Chop `n` characters off the end of `string`"
  (subseq string 0 (max 0 (- (length string) n))))

(defun vowelp (character)
  (ensure-boolean (member character '(#\a #\e #\i #\o #\u))))

(defun mapcar% (function list)
  (typecase list
    (null nil)
    (cons (cons (funcall function (car list))
                (mapcar% function (cdr list))))
    (t (funcall function list))))


(defmacro assert-nonempty (place message)
  `(assert (not (emptyp ,place)) (,place) ,message))


;;;; Guts ---------------------------------------------------------------------
(defun separate (list)
  (-<> list
    (split-sequence:split-sequence :. <>)
    (mapcar (rcurry #'riffle " ") <>)
    (apply #'append <>)))

(defun string-pre (contents)
  (separate contents))

(defun string-post (contents)
  (apply #'cat contents))


(defparameter *rule-types* (make-hash-table))


(defun evaluate-combination (list environment)
  (-<> list
    (funcall (getf environment :combination-pre) <>)
    (mapcar% (rcurry #'evaluate-expression environment) <>)
    (funcall (getf environment :combination-post) <>)))

(defun evaluate-modifiers (vector environment)
  (reduce (flip #'funcall) vector
          :start 1
          :initial-value (evaluate-expression (aref vector 0) environment)))

(defun evaluate-expression (expr environment)
  (typecase expr
    ((or string keyword null) expr)
    (symbol (funcall expr))
    (vector (evaluate-modifiers expr environment))
    (list (if (eq (first expr) 'quote)
            (second expr)
            (evaluate-combination expr environment)))
    (t expr)))


(defmacro define-rule (name-and-options &rest expressions)
  (destructuring-bind (name &key type) name-and-options
    `(defun ,name ()
       (evaluate-expression
         (random-elt ,(coerce expressions 'vector))
         (gethash ,type *rule-types*)))))

(defun add-rule-type (type &key combination-pre combination-post)
  (setf (gethash type *rule-types*)
        `(:combination-pre ,combination-pre :combination-post ,combination-post))
  (values))


(add-rule-type :string
               :combination-pre #'string-pre
               :combination-post #'string-post)

(add-rule-type :data
               :combination-pre #'identity
               :combination-post #'identity)

(defmacro define-string (name &rest body)
  `(define-rule (,name :type :string) ,@body))

(defmacro define-data (name &rest body)
  `(define-rule (,name :type :data) ,@body))


;;;; Modifiers ----------------------------------------------------------------
(defun cap (string)
  "Capitalize the first character of `string`."
  (assert-nonempty string "Cannot capitalize an empty string.")
  (string-capitalize string :end 1))

(defun cap-all (string)
  "Capitalize each word of `string`."
  (assert-nonempty string "Cannot capitalize-all an empty string.")
  (string-capitalize string))

(defun q (string)
  "Wrap `string` in quotation marks."
  (cat "\"" string "\""))

(defun a (string)
  "Add an indefinite article (a or an) to the front of `string`."
  (assert-nonempty string "Cannot add an article to an empty string.")
  (cat (if (vowelp (ch string 0))
         "an "
         "a ")
       string))

(defun s (string)
  "Pluralize `string`."
  (assert-nonempty string "Cannot pluralize an empty string.")
  (case (ch string -1)
    (#\y (if (vowelp (ch string -2))
           (cat string "s")
           (cat (chop string 1) "ies")))
    (#\x (cat (chop string 1) "en"))
    ((#\z #\h) (cat (chop string 1) "es"))
    (t (cat string "s"))))

(defun pos (string)
  "Make `string` posessive by adding an apostrophe (and possibly an s)."
  (assert-nonempty string "Cannot make an empty string posessive.")
  (cat string
       (if (eql #\s (ch string -1))
         "'"
         "'s")))


;;;; Example ------------------------------------------------------------------
(define-string name
  "arjun"
  "yuuma"
  "jess"
  "bob smith")

(define-string nature-noun
  "ocean"
  "mountain"
  "forest"
  "cloud"
  "river"
  "tree"
  "sky"
  "sea"
  "desert")

(define-string animal
  "unicorn"
  "raven"
  "turkey"
  "wallaby"
  "sparrow"
  "scorpion"
  "coyote"
  "eagle"
  "owl"
  "lizard"
  "zebra"
  "duck"
  "kitten")

(define-string color
  "orange"
  "blue"
  "white"
  "black"
  "grey"
  "purple"
  "indigo"
  "turquoise")

(define-string activity
  "running"
  "jumping"
  "flying"
  "carousing")

(define-string sentence
  ("The" color animal "of the" nature-noun "is called" #(name cap-all q) :. ".")
  ("The" animal "was" activity "in the" #(nature-noun s) :. ".")
  (#(name cap-all pos) "favorite color is" color :. ".")
  (#(nature-noun cap) "air is fresh.")
  ("The" #(animal s) "were" activity "in the" nature-noun :. "."))


; (iterate (repeat 30) (pr (sentence)))

(define-data monster-type
  :bat :kobold :goblin)

(define-data monster-health
  #(50 random))

(define-data monster
  (monster-type :hp monster-health))

(define-data amount
  5 6 7 8 9 10)

(define-data money
  (#(100 random) :gold)
  (#(500 random) :silver))

(define-string potion-type
  "healing"
  "levitation"
  "detect magic"
  "confusion")

(define-string potion-quality
  "strong" "weak" "small")

(define-string potion
  (potion-quality "potion of" potion-type)
  ("potion of" potion-type))

(define-string enchant
  "+1" "+2" "+3")

(define-string armor-piece
  "shield"
  "breastplate"
  "suit of chain mail"
  "belt"
  "helmet")

(define-string armor
  armor-piece
  (enchant armor-piece))

(define-string item
  armor
  potion)

(define-data single-loot
  money
  item)

(define-data loot
  (single-loot)
  (single-loot . loot))

(define-data encounter
  (:monster monster
   :amount amount
   :loot loot))


(iterate (repeat 30) (pr (encounter)))
