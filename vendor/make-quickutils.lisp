(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(
               :with-gensyms
               :once-only
               :compose
               :curry
               :rcurry
               :n-grams
               :define-constant
               :riffle
               :tree-collect
               :ensure-gethash
               ; :switch
               ; :while
               ; :ensure-boolean
               ; :iota
               ; :zip
               )
  :package "SAND.QUICKUTILS")
