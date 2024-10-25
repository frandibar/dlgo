(defpackage dlgo/tests
  (:use #:common-lisp
	#:dlgo
	#:dlgo.util
	#:dlgo.constant
	#:dlgo.zobrist
	#:dlgo.point
	#:dlgo.tui
	#:dlgo.sgf)
  (:import-from #:dlgo.constant
		#:black
		#:white)
  (:import-from #:alexandria
		#:define-constant
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:1am
		#:is
		#:signals
		#:test)
  (:export #:run-tests))
