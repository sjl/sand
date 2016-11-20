; (rename-package :charms :hunchentoot '(:ht))


(defpackage :sand.utils
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :sand.quickutils)
  (:export
    :average4))

(defpackage :sand.primes
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :primep))

(defpackage :sand.random-numbers
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :sand.quickutils
    :sand.utils))

(defpackage :sand.generic-arithmetic
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :sandalphon.compiler-macro
    :sand.quickutils
    :sand.utils)
  (:shadow
    :+
    :-
    :/
    :*))

(defpackage :sand.parenscript
  (:use
    :cl
    :losh
    :sand.quickutils
    :cl-arrows
    :cl-fad
    :parenscript) 
  (:shadowing-import-from :losh
    :%))

(defpackage :sand.ascii
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :sand.quickutils
    :sand.utils))

(defpackage :sand.terrain.diamond-square
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :sand.quickutils
    :sand.utils))

(defpackage :sand.sketch
  (:use
    :cl
    :losh
    :sketch
    :iterate
    :sand.quickutils
    :sand.utils)
  (:shadowing-import-from :iterate
    :in)
  (:shadowing-import-from :sketch
    :degrees
    :radians))

(defpackage :sand.markov
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :split-sequence
    :sand.quickutils
    :sand.utils))

(defpackage :sand.dijkstra-maps
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :dijkstra-map
    :make-dijkstra-map
    :dm-maximum-value
    :dm-map
    :dm-ref))

(defpackage :sand.graphs
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.graphviz
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :graphviz-digraph))

(defpackage :sand.binary-decision-diagrams
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.graphviz
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.zero-suppressed-decision-diagrams
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.graphviz
    :sand.quickutils
    :sand.utils)
  (:export
    )
  (:nicknames :sand.zdd))

(defpackage :sand.huffman-trees
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.graphviz
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.streams
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.primes
    :sand.quickutils
    :sand.utils)
  (:export
    ))

#+sbcl
(defpackage :sand.ffi
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :cffi
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.color-difference
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :rs-colors
    :sand.quickutils
    :sand.utils)
  (:export
    ))


#+sbcl
(defpackage :sand.profiling
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :start-profiling
    :stop-profiling
    :profile))


(defpackage :sand.number-letters
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :function-cache
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.urn
  (:use
    :cl
    :cl-arrows
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))


