(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :copy-array
               :curry
               :define-constant
               :ensure-boolean
               :ensure-gethash
               :ensure-keyword
               :ensure-list
               :extremum
               :flip
               :hash-table-alist
               :hash-table-keys
               :hash-table-plist
               :hash-table-values
               :iota
               :mappend
               :n-grams
               :once-only
               :range
               :rcurry
               :read-file-into-string
               :required-argument
               :riffle
               :separated-string-append
               :subdivide
               :symb
               :tree-collect
               :with-gensyms
               :write-string-into-file

               )
  :package "SAND.QUICKUTILS")
