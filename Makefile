.PHONY:

vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && ros run -L sbcl --load make-quickutils.lisp  --eval '(quit)'
