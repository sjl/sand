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
    #:cl-arrows
    #:losh
    #:iterate
    #:split-sequence
    #:sand.quickutils
    #:sand.utils))

(defpackage #:sand.dijkstra-maps
  (:use
    #:cl
    #:cl-arrows
    #:losh
    #:iterate
    #:sand.quickutils
    #:sand.utils)
  (:export
    #:dijkstra-map
    #:make-dijkstra-map
    #:dm-maximum-value
    #:dm-map
    #:dm-ref))

(defpackage #:sand.graphviz
  (:use
    #:cl
    #:cl-arrows
    #:losh
    #:iterate
    #:sand.quickutils
    #:sand.utils)
  (:export
    #:graphviz-digraph))

(defpackage #:sand.binary-decision-diagrams
  (:use
    #:cl
    #:cl-arrows
    #:losh
    #:iterate
    #:sand.graphviz
    #:sand.quickutils
    #:sand.utils)
  (:export
    ))
