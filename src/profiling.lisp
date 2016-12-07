(in-package :sand.profiling)

#+sbcl
(defun dump-profile ()
  (with-open-file (*standard-output* "sand.prof"
                                     :direction :output
                                     :if-exists :supersede)
    (sb-sprof:report :type :graph
                     :sort-by :cumulative-samples
                     :sort-order :ascending)
    (sb-sprof:report :type :flat
                     :min-percent 0.5)))

#+sbcl
(defun start-profiling ()
  (sb-sprof::reset)
  ; (sb-sprof::profile-call-counts "SILT")
  (sb-sprof::start-profiling :max-samples 50000
                             :mode :cpu
                             ; :mode :time
                             :sample-interval 0.01
                             :threads :all))

#+sbcl
(defun stop-profiling ()
  (sb-sprof::stop-profiling)
  (dump-profile))


(defmacro profile (&body body)
  `(prog2
    (start-profiling)
    (progn ,@body)
    (stop-profiling)))
