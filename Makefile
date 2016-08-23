.PHONY: vendor

vendor: vendor/quickutils.lisp

vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && ros run -L sbcl --load make-quickutils.lisp  --eval '(quit)'
