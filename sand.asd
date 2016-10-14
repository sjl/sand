(asdf:defsystem #:sand
  :name "sand"
  :description "A little sandbox to play around in."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (#:cl-charms
               #:iterate
               #:cl-arrows
               #:cl-fad
               #:split-sequence
               #:parenscript
               #:sketch
               #:losh
               #:drakma
               #:function-cache
               #:yason
               #:flexi-streams
               #:sanitize
               #:html-entities
               #:plump
               #:clss
               #:cl-algebraic-data-type
               #:rs-colors
               #:cffi
               #+sbcl #:sb-sprof
               )

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "primes")
                 (:file "graphs")
                 (:file "graphviz")
                 (:file "random-numbers")
                 (:file "ascii")
                 (:file "markov")
                 (:file "dijkstra-maps")
                 #+sbcl (:file "ffi")
                 #+sbcl (:file "profiling")
                 (:file "binary-decision-diagrams")
                 (:file "zero-suppressed-decision-diagrams")
                 (:file "huffman-trees")
                 (:file "streams")
                 (:file "color-difference")
                 (:file "number-letters")
                 (:file "rubiks")
                 (:module "terrain"
                  :serial t
                  :components ((:file "diamond-square")))
                 (:module "parenscript"
                  :serial t
                  :components ((:file "compiler")))
                 (:file "sketch")))))
