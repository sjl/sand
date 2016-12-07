(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :define-constant
               :ensure-gethash
               :ensure-list
               :hash-table-alist
               :hash-table-keys
               :hash-table-plist
               :hash-table-values
               :n-grams
               :once-only
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
