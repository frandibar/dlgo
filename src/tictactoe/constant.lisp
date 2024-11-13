(defpackage #:dlgo.tictactoe.constant
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:define-constant)
  (:export #:cross
	   #:nought
	   #:tie
	   #:win
	   #:lose
	   #:user-error
	   #:text
	   #:+board-size+))

(in-package #:dlgo.tictactoe.constant)

(define-constant +board-size+ 3)

;; Be able to identify user errors such as invalid moves from bugs.
(define-condition user-error (error)
  ((text :initarg :text
	 :reader text)))
