(defpackage dlgo/tests
  (:use #:common-lisp

	#:dlgo
	#:dlgo.agent
	#:dlgo.board
	#:dlgo.constant
	#:dlgo.game
	#:dlgo.point
	#:dlgo.sgf
	#:dlgo.tui
	#:dlgo.util
	#:dlgo.zobrist)

  (:import-from #:1am
		#:is
		#:signals
		#:test)
  (:import-from #:alexandria
		#:define-constant
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)

  (:import-from #:dlgo.constant
		#:black
		#:white)

  (:export #:run-tests))
