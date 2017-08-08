(in-package :sand.serializing-functions)

(conspack:defencoding st-fun::function-referrer
  st-fun::function-info
  st-fun::root)

(conspack:defencoding st-fun:code-information)

(conspack:defencoding st-fun::function-info)

(conspack:defencoding st-fun::lambda-info
  st-fun::body
  st-fun::lambda-list)

(conspack:defencoding st-fun::named-lambda-info
  st-fun::name
  st-fun::body
  st-fun::lambda-list)

(conspack:defencoding st-fun::function-call-info
  st-fun::function-name
  st-fun::values)

(conspack:defencoding st-fun::quoted-function-info
  st-fun::body)

;;;; Closures -----------------------------------------------------------------
(defun get-closure-info-children (info)
  (st-fun::unset-weak-list (st-fun::info-children-weak-list info))
  (unwind-protect (copy-list (st-fun::info-children-weak-list info))
    (st-fun::set-weak-list (st-fun::info-children-weak-list info))))


(defmethod conspack:encode-object ((object st-fun::closure-info)
                                   &key &allow-other-keys)
  (acons 'st-fun::children
         (get-closure-info-children object)
         (conspack:slots-to-alist (object)
                                  st-fun::type
                                  st-fun::declarations)))

(defmethod conspack:decode-object ((class (eql 'st-fun::closure-info)) alist
                                   &key &allow-other-keys)
  (conspack:alist-to-slots (alist :class st-fun::closure-info)
                           st-fun::type
                           st-fun::declarations
                           st-fun::children))


(defmethod conspack:encode-object ((object st-fun::flet-closure-info)
                                   &key &allow-other-keys)
  (acons 'st-fun::children
         (get-closure-info-children object)
         (conspack:slots-to-alist (object)
                                  st-fun::type
                                  st-fun::functions
                                  st-fun::declarations)))

(defmethod conspack:decode-object ((class (eql 'st-fun::flet-closure-info)) alist
                                   &key &allow-other-keys)
  (st-fun:restore-code-info
    (conspack:alist-to-slots (alist :class st-fun::flet-closure-info)
                             st-fun::type
                             st-fun::functions
                             st-fun::declarations
                             st-fun::children)))


(defmethod conspack:encode-object ((object st-fun::macro-closure-info)
                                   &key &allow-other-keys)
  (acons 'st-fun::children
         (get-closure-info-children object)
         (conspack:slots-to-alist (object)
                                  st-fun::type
                                  st-fun::macros
                                  st-fun::declarations)))

(defmethod conspack:decode-object ((class (eql 'st-fun::macro-closure-info)) alist
                                   &key &allow-other-keys)
  (st-fun:restore-code-info
    (conspack:alist-to-slots (alist :class st-fun::macro-closure-info)
                             st-fun::type
                             st-fun::macros
                             st-fun::declarations
                             st-fun::children)))


(defmethod conspack:encode-object ((object st-fun::let-closure-info)
                                   &key &allow-other-keys)
  (-<> (conspack:slots-to-alist (object)
                                st-fun::type
                                st-fun::declarations
                                st-fun::variables)
    (acons 'st-fun::values
           (funcall (st-fun::info-values-accessor object) object)
           <>)
    (acons 'st-fun::children
           (get-closure-info-children object)
           <>)))

(defmethod conspack:decode-object ((class (eql 'st-fun::let-closure-info)) alist
                                   &key &allow-other-keys)
  (st-fun:restore-code-info
    (conspack:alist-to-slots (alist :class st-fun::let-closure-info)
                             st-fun::type
                             st-fun::values
                             st-fun::declarations
                             st-fun::variables
                             st-fun::children)))


;;;; Main Encoding Entry Point ------------------------------------------------
(defmethod conspack:encode-object
    ((object function) &key &allow-other-keys)
  (let ((ref (st-fun:get-function-referrer object)))
    (if ref
      (acons 'referrer ref nil)
      (error "Function ~A is not storable." object))))

(defmethod conspack:decode-object
    ((class (eql 'function)) alist &key &allow-other-keys)
  (let ((ref (cdr (assoc 'referrer alist))))
    (st-fun:restore-code-info ref)))


;;;; Scratch ------------------------------------------------------------------

(defparameter *test*
  (st-fun:st-let
    ((acc 0))
    (st-fun:st-flet
      ((add (x) (incf acc x))
       (sub (x) (decf acc x)))
      (cons #'add #'sub))))

(funcall (car *test*) 1)
(funcall (cdr *test*) 1)

(defparameter *encoded*
  (st-fun:with-storable-functions-storage ()
    (conspack:tracking-refs ()
      (conspack:encode *test*))))

(conspack:explain *encoded*)

(defparameter *decoded*
  (st-fun:with-storable-functions-restorage ()
    (conspack:tracking-refs ()
      (conspack:decode *encoded*))))

(funcall (car *decoded*) 1)
(funcall (cdr *decoded*) 1)
