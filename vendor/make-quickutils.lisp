(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :define-constant
               :ensure-gethash
               :hash-table-alist
               :hash-table-plist
               :hash-table-keys
               :hash-table-values
               :n-grams
               :once-only
               :rcurry
               :read-file-into-string
               :required-argument
               :riffle
               :tree-collect
               :with-gensyms

               )
  :package "SAND.QUICKUTILS")
