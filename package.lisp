(defpackage :sand.utils
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils)
  (:export
    :average4))

(defpackage :sand.random-numbers
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))

(defpackage :sand.generic-arithmetic
  (:use
    :cl
    :losh
    :iterate
    :sandalphon.compiler-macro
    :sand.quickutils
    :sand.utils)
  (:shadow
    :+
    :-
    :/
    :*))

(defpackage :sand.sorting
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))

(defpackage :sand.parenscript
  (:use
    :cl
    :losh
    :sand.quickutils
    :cl-fad
    :parenscript) 
  (:shadowing-import-from :losh
    :%))

(defpackage :sand.ascii
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))

(defpackage :sand.terrain.diamond-square
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))

(defpackage :sand.dijkstra-maps
  (:use
    :cl
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
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.graphviz
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :graphviz-digraph))

(defpackage :sand.ropes
  (:use
    :cl
    :losh
    :iterate
    :trivia
    :sand.quickutils
    :sand.utils)
  (:shadowing-import-from :losh
    :<>)
  (:export
    ))

(defpackage :sand.hanoi
  (:use
    :cl
    :losh
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.binary-decision-diagrams
  (:use
    :cl
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
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

#+sbcl
(defpackage :sand.ffi
  (:use
    :cl
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
    :losh
    :iterate
    :rs-colors
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.number-letters
  (:use
    :cl
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
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.qud
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.istruct
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.names
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.surreal-numbers
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.easing
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))

(defpackage :sand.serializing-functions
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    ))


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

(defpackage :sand.mandelbrot
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


#+sbcl
(defpackage :sand.profiling
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils)
  (:export
    :start-profiling
    :stop-profiling
    :profile))


(defpackage :sand.turing-omnibus.wallpaper
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

(defpackage :sand.turing-omnibus.monto-carlo
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))

(defpackage :sand.turing-omnibus.minimax
  (:use
    :cl
    :losh
    :iterate
    :sand.quickutils
    :sand.utils))
