(asdf:defsystem :sand
  :name "sand"
  :description "A little sandbox to play around in."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (

               #+sbcl :sb-sprof
               ;; :cffi
               ;; :cl-algebraic-data-type
               ;; :cl-charms
               ;; :cl-fad
               ;; :cl-ppcre
               ;; :clss
               ;; :compiler-macro
               ;; :cl-conspack
               ;; :drakma
               ;; :easing
               ;; :flexi-streams
               ;; :function-cache
               :iterate
               :losh
               ;; :parenscript
               ;; :parse-float
               ;; :plump
               ;; :rs-colors
               ;; :sanitize
               ;; :sketch
               ;; :split-sequence
               ;; :storable-functions
               ;; :trivia
               ;; :trivial-main-thread
               ;; :vex
               ;; :yason

               )

  :serial t
  :components
  ((:module "vendor"
    :serial t
    :components ((:file "quickutils")))
   ))
