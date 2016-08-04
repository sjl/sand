; (rename-package :charms :hunchentoot '(:ht))


(defpackage #:sand.utils
  (:use
    #:cl
    #:iterate
    #:cl-arrows
    #:sand.quickutils)
  (:export
    #:zap%
    #:%
    #:recursively
    #:recur
    #:dis
    #:bits
    #:spit

    #:dlambda

    #:hash-set
    #:make-set
    #:set-contains-p
    #:set-add
    #:set-remove
    #:set-add-all
    #:set-remove-all
    #:set-random
    #:set-pop
    #:set-empty-p
    #:set-clear

    #:averaging
    #:timing
    #:real-time
    #:run-time
    #:since-start-into
    #:per-iteration-into
    #:in-whatever

    #:queue
    #:queue-contents
    #:enqueue
    #:dequeue
    #:queue-empty-p
    #:queue-append

    )
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:sand.random-numbers
  (:use
    #:cl
    #:iterate
    #:cl-arrows
    #:sand.quickutils
    #:sand.utils)
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:sand.parenscript
  (:use
    #:cl
    #:sand.quickutils
    #:cl-arrows
    #:cl-fad

    #:parenscript)
  (:shadowing-import-from #:cl-arrows
    #:->)
  (:import-from #:sand.utils)
  )


(defpackage #:sand.ascii
  (:use #:cl
        #:iterate
        #:cl-arrows
        #:sand.quickutils
        #:sand.utils))


(defpackage #:sand.sketch
  (:use
    #:cl
    #:sketch
    #:iterate
    #:sand.quickutils
    #:sand.utils)
  (:shadowing-import-from #:iterate
    #:in))
