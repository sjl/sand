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
               #:sketch)

  :serial t
  :components
  ((:file "quickutils") ; quickutils package ordering crap
   (:file "package")
   (:module "src"
    :serial t
    :components ((:file "utils")
                 (:file "random-numbers")
                 (:file "ascii")
                 (:file "sketch")
                 (:module "parenscript"
                  :serial t
                  :components ((:file "compiler")))))))
