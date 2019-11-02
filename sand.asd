(asdf:defsystem :sand
  :name "sand"
  :description "A little sandbox to play around in."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT"
  :version "0.0.1"

  :depends-on (:alexandria
               :iterate
               :losh)

  :serial t
  :components ((:module "vendor"
                :serial t
                :components ((:file "quickutils")))))
