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
               #:parenscript
               #:sketch
               #:losh)

  :serial t
  :components
  ((:file "quickutils") ; quickutils package ordering crap
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "random-numbers")
                 (:file "ascii")
                 (:module "terrain"
                  :serial t
                  :components ((:file "diamond-square")))
                 (:module "parenscript"
                  :serial t
                  :components ((:file "compiler")))
                 (:file "sketch")))))
