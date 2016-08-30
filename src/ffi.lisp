(in-package #:sand.ffi)


;;;; Library ------------------------------------------------------------------
(define-foreign-library linenoise
  (:darwin "~/src/linenoise/linenoise.dylib"))

(use-foreign-library linenoise)


;;;; Simple Functions ---------------------------------------------------------
(defcfun ("linenoiseClearScreen" linenoise-clear-screen) :void)

(defcfun ("linenoiseSetMultiLine" %linenoise-set-multi-line) :void
  (ml :int))

(defun linenoise-set-multi-line (flag)
  (%linenoise-set-multi-line (convert-to-foreign flag :boolean)))

(defcfun ("linenoiseHistorySetMaxLen" linenoise-history-set-max-length) :int
  (len :int))

(defcfun ("linenoiseHistorySave" linenoise-history-save) :int
  (filename :string))

(defcfun ("linenoiseHistoryLoad" linenoise-history-load) :int
  (filename :string))

(defcfun ("linenoiseHistoryAdd" linenoise-history-add) :int
  (line :string))

(defcfun ("linenoiseFree" linenoise-free) :void
  (pointer :pointer))

(defun linenoise (prompt &key (add-to-history t))
  (let ((ptr (foreign-funcall "linenoise" :string prompt (:pointer :char))))
    (unwind-protect
        (let ((result (convert-from-foreign ptr :string)))
          (when add-to-history
            (linenoise-history-add result))
          result)
      (linenoise-free ptr))))


;;;; Completion Callbacks -----------------------------------------------------
(defparameter *linenoise-completion-callback* nil)


(defcfun ("linenoiseAddCompletion" linenoise-add-completion) :void
  (lc :pointer)
  (str :string))

(defcfun ("linenoiseSetCompletionCallback" linenoise-set-completion-callback)
    :void
  (callback :pointer))

(defcallback linenoise-completion-callback :void
    ((prefix (:pointer :char))
     (lc :pointer))
  (when *linenoise-completion-callback*
    (mapc (curry #'linenoise-add-completion lc)
          (funcall *linenoise-completion-callback*
                   (convert-from-foreign prefix :string))))
  (values))

(linenoise-set-completion-callback (callback linenoise-completion-callback))


;;;; Hints Callbacks ----------------------------------------------------------
(defparameter *linenoise-hints-callback* nil)

(defcfun ("linenoiseSetHintsCallback" linenoise-set-hints-callback) :void
  (callback :pointer))

(defcfun ("linenoiseSetFreeHintsCallback" linenoise-set-free-hints-callback)
    :void
  (callback :pointer))

(defcallback linenoise-hints-callback :string
    ((prefix (:pointer :char))
     (color (:pointer :int))
     (bold (:pointer :int)))
  (if *linenoise-hints-callback*
    (multiple-value-bind (hint hint-color hint-bold)
        (funcall *linenoise-hints-callback*
                 (convert-from-foreign prefix :string))
      (if hint
        (prog1
            (foreign-string-alloc hint)
          (when hint-color
            (setf (mem-ref color :int) (ecase hint-color
                                         (:red 31)
                                         (:green 32)
                                         (:yellow 33)
                                         (:blue 34)
                                         (:magenta 35)
                                         (:cyan 36)
                                         (:white 37))))
          (when hint-bold
            (setf (mem-ref bold :boolean) hint-bold)))
        (null-pointer)))
    (null-pointer)))

(defcallback linenoise-free-hints-callback :void
    ((hint-string :pointer))
  (foreign-string-free hint-string))

(linenoise-set-hints-callback (callback linenoise-hints-callback))
(linenoise-set-free-hints-callback (callback linenoise-free-hints-callback))


;;;; Scratch ------------------------------------------------------------------
(defun test-compl (prefix)
  (let ((result nil))
    (when (not (string= "" prefix))
      (when (char= #\f (aref prefix 0))
        (pushnew "foo" result)
        (pushnew "frob" result)
        (pushnew "flip" result))
      (when (char= #\b (aref prefix 0))
        (pushnew "bar" result)))
    result))

(defun test-hint (prefix)
  (when (string= "cp " prefix)
    (values "<source> <dest>" :blue t)))


(setf *linenoise-completion-callback* 'test-compl)
(setf *linenoise-hints-callback* 'test-hint)
