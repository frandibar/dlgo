(defpackage #:dlgo.tictactoe.board
  (:use #:common-lisp)
  (:import-from #:dlgo.tictactoe.constant
		#:+board-size+)
  (:import-from #:dlgo.tictactoe.point
		#:point-index)
  (:export #:make-board
	   #:make-board
	   #:deep-copy-board
	   #:get-at
	   #:empty-p))

(in-package #:dlgo.tictactoe.board)

(defun make-board ()
  (make-array (* +board-size+ +board-size+)
	      :initial-element nil))

(defun deep-copy-board (board)
  "Return a deep copy of BOARD."
  (copy-seq board))

(defun get-at (point board)
  "Return the group of stones at POINT on BOARD or NIL if empty."
  (aref board
	(point-index point)))

(defun empty-p (point board)
  "Return T if POINT at BOARD is NIL."
  (not (get-at point board)))
