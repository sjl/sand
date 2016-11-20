(asdf:defsystem #:sand
  :name "sand"
  :description "A little sandbox to play around in."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "0.0.1"

  :depends-on (

               #+sbcl :sb-sprof
               :cffi
               :cl-algebraic-data-type
               :cl-arrows
               :cl-charms
               :cl-fad
               :clss
               :compiler-macro
               :drakma
               :flexi-streams
               :function-cache
               :html-entities
               :iterate
               :losh
               :parenscript
               :plump
               :rs-colors
               :sanitize
               :sketch
               :split-sequence
               :yason

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
                 (:file "urn")
                 (:file "random-numbers")
                 (:file "generic-arithmetic")
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
                 (:module "terrain"
                  :serial t
                  :components ((:file "diamond-square")))
                 (:module "parenscript"
                  :serial t
                  :components ((:file "compiler")))
                 (:file "sketch")))))
