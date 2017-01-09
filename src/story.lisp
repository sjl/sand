(in-package :sand.story)

;;; Basically a Lispy version of Tracery https://github.com/galaxykate/tracery
;;; without the nutty string-parsing stuff.
;;;
;;; (define-symbol name ...expressions...)
;;;
;;; strings evaluate to themselves: "foo bar" -> "foo bar"
;;;
;;; symbols funcall their symbol-function: animal -> "mouse"
;;;
;;; vectors evaluate their contents and concatenate them with spaces in between:
;;;     #("foo" animal "bar") -> "foo mouse bar"
;;;
;;; the magic keyword :. inside a vector suppresses the space there:
;;;
;;;     #("foo" "bar" :. "baz") -> "foo barbaz"
;;;
;;; lists evaluate the head and pipe it through all the functions in the tail:
;;;
;;;     (animal capitalize pos) -> "Mouse's"


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

(defmacro assert-nonempty (place message)
  `(assert (not (emptyp ,place)) (,place) ,message))


;;;; Guts ---------------------------------------------------------------------
(defun separate (list)
  (-<> list
    (split-sequence:split-sequence :. <>)
    (mapcar (rcurry #'riffle " ") <>)
    (apply #'append <>)))

(defun evaluate-vector (vector)
  (-<> (coerce vector 'list)
    (separate <>)
    (mapcar #'evaluate-expression <>)
    (apply #'cat <>)))

(defun evaluate-list (list)
  (destructuring-bind (expr &rest modifiers) list
    (reduce (flip #'funcall) modifiers
            :initial-value (evaluate-expression expr))))

(defun evaluate-expression (expr)
  (etypecase expr
    (string expr)
    (vector (evaluate-vector expr))
    (list (evaluate-list expr))
    (symbol (funcall expr))))


(defun generate (symbol)
  (evaluate-expression symbol))


(defmacro define-symbol (name &rest expressions)
  `(defun ,name ()
     (evaluate-expression
       (random-elt ,(coerce expressions 'vector)))))


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
(define-symbol name
  "arjun"
  "yuuma"
  "jess"
  "bob smith")

(define-symbol nature-noun
  "ocean"
  "mountain"
  "forest"
  "cloud"
  "river"
  "tree"
  "sky"
  "sea"
  "desert")

(define-symbol animal
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

(define-symbol color
  "orange"
  "blue"
  "white"
  "black"
  "grey"
  "purple"
  "indigo"
  "turquoise")

(define-symbol activity
  "running"
  "jumping"
  "flying"
  "carousing")

(define-symbol sentence
  #("The" color animal "of the" nature-noun "is called" (name cap-all q) :. ".")
  #("The" animal "was" activity "in the" (nature-noun s) :. ".")
  #((name cap-all pos) "favorite color is" color :. ".")
  #((nature-noun cap) "air is fresh.")
  #("The" (animal s) "were" activity "in the" nature-noun :. "."))


(generate 'sentence)
