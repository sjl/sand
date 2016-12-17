(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-array
               :curry
               :define-constant
               :ensure-gethash
               :ensure-list
               :extremum
               :hash-table-alist
               :hash-table-keys
               :hash-table-plist
               :hash-table-values
               :iota
               :n-grams
               :once-only
               :range
               :rcurry
               :read-file-into-string
               :required-argument
               :riffle
               :subdivide
               :symb
               :tree-collect
               :with-gensyms

               )
  :package "SAND.QUICKUTILS")
