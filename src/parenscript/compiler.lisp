(in-package :sand.parenscript)

(defun compile-parenscript-file (source)
  (let* ((source-path (pathname source))
         (target-path (make-pathname :type "js"
                                     :defaults source-path)))
    (with-open-file (output target-path
                            :direction :output
                            :if-exists :supersede)
      (write-string (ps-compile-file source-path) output)))
  (values))
