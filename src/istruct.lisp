(in-package :sand.istruct)

;;;; Equality -----------------------------------------------------------------
(defgeneric equal? (a b))

(defmethod equal? ((a t) (b t))
  nil)

(defmethod equal? ((a number) (b number))
  (= a b))

(defmethod equal? ((a string) (b string))
  (equal a b))

(defmethod equal? ((a symbol) (b symbol))
  (eq a b))


;;;; Retrieval/Modification ---------------------------------------------------
(defun iget (instance slot)
  (slot-value instance slot))

(defun iget-in (instance slot-path)
  (iterate
    (for result :first instance :then (slot-value result slot))
    (for slot :in slot-path)
    (finally (return result))))


;; (defgeneric iset (instance slot new-value))
(defun iset (instance slot new-value)
  (let ((result (copy-structure instance)))
    (setf (slot-value result slot) new-value)
    result))

(defun iset-in (instance slot-path new-value)
  (destructuring-bind (slot . remaining) slot-path
    (if (null remaining)
      (iset instance slot new-value)
      (iset instance slot (iset-in (iget instance slot)
                                   remaining new-value)))))


(defun iupdate (instance slot function &rest args)
  (iset instance slot (apply function (iget instance slot) args)))

(defun iupdate-in (instance slot-path function &rest args)
  (destructuring-bind (slot . remaining) slot-path
    (if (null remaining)
      (apply #'iupdate instance slot function args)
      (iset instance slot
            (apply #'iupdate-in (iget instance slot)
                   remaining function args)))))


;;;; Definition ---------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun required (name)
    (error "Slot ~S is required" name))

  (defun build-slot (slot-spec)
    (destructuring-bind (slot-name &key default type) slot-spec
      `(,slot-name
        ,(if default
           default
           `(required ',slot-name))
        :read-only t
        ,@(when type `(:type ,type)))))

  (defun build-immutable-struct-form (name slots)
    `(defstruct ,name
       ,@(mapcar #'build-slot slots)))

  (defun build-equal? (name slots)
    `(defmethod equal? ((a ,name) (b ,name))
       (and ,@(iterate (for (slot . nil) :in slots)
                       (collect `(equal?
                                   (slot-value a ',slot)
                                   (slot-value b ',slot)))))))

  (defun build-constructor (name slots)
    (let ((slot-names (mapcar #'first slots)))
      `(defun ,name ,slot-names
         (,(symb 'make- name)
          ,@(iterate (for slot :in slot-names)
                     (collect (ensure-keyword slot))
                     (collect slot)))))))


(defmacro define-istruct (name-and-options &rest slots)
  "Define an immutable structure."
  (destructuring-bind (name) (ensure-list name-and-options)
    (let ((slots (mapcar #'ensure-list slots)))
      `(progn
         ,(build-immutable-struct-form name slots)
         ,(build-equal? name slots)
         ,(build-constructor name slots)
         ',name))))


;;;; Scratch ------------------------------------------------------------------
(define-istruct sword
  material)

(define-istruct monster
  (hp :default 10)
  species
  (weapon :default nil))

