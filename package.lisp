; (rename-package :charms :hunchentoot '(:ht))


(defpackage #:sand.utils
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:sand.quickutils)
  (:export
    #:average4)
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:sand.random-numbers
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:sand.quickutils
    #:sand.utils)
  (:shadowing-import-from #:cl-arrows
    #:->))

(defpackage #:sand.parenscript
  (:use
    #:cl
    #:losh
    #:sand.quickutils
    #:cl-arrows
    #:cl-fad
    #:parenscript)
  (:shadowing-import-from #:cl-arrows
    #:->)
  (:shadowing-import-from #:losh
    #:%))

(defpackage #:sand.ascii
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:sand.quickutils
    #:sand.utils))

(defpackage #:sand.terrain.diamond-square
  (:use
    #:cl
    #:losh
    #:iterate
    #:cl-arrows
    #:sand.quickutils
    #:sand.utils))

(defpackage #:sand.sketch
  (:use
    #:cl
    #:losh
    #:sketch
    #:iterate
    #:sand.quickutils
    #:sand.utils)
  (:shadowing-import-from #:iterate
    #:in))

(defpackage #:sand.markov
  (:use
    #:cl
    #:losh
    #:iterate
    #:split-sequence
    #:sand.quickutils
    #:sand.utils))
