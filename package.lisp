(defpackage #:sand.utils
  (:use
    #:cl
    #:defstar
    #:iterate
    #:cl-arrows
    #:sand.quickutils)
  (:export
    #:zap%
    #:%
    #:recursively
    #:recur
    #:dis

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

    #:queue
    #:queue-contents
    #:enqueue
    #:dequeue
    #:queue-empty-p
    #:queue-append

    ))

(defpackage #:sand.random-numbers
  (:use
    #:cl
    #:defstar
    #:iterate
    #:cl-arrows
    #:sand.quickutils
    #:sand.utils))

