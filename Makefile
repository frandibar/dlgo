.PHONY: test

test:
	sbcl --load tests/run.lisp

build:
	./build-binary.sh
