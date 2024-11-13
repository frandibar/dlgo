(defpackage #:dlgo.board
  (:use #:common-lisp)
  (:import-from #:dlgo.point
		#:point-index)
  (:import-from #:dlgo.zobrist
		#:+empty-board-hash+)
  (:export #:make-board
	   #:board-size
	   #:board-hash
	   #:board-grid
	   #:deep-copy-board
	   #:board-equal-p
	   #:get-at
	   #:empty-p))

(in-package #:dlgo.board)

(defstruct (board (:constructor make-board (board-size)))
  ;; The grid map holds the points as keys and the groups those points
  ;; belong to as values.
  (size board-size)
  (grid (make-array (* board-size board-size)
		    :initial-element nil))
  (hash +empty-board-hash+))

(defun deep-copy-board (board)
  "Return a deep copy of BOARD."
  (let ((new-board (make-board (board-size board))))
    (setf (board-hash new-board)
	  (board-hash board))
    (setf (board-grid new-board)
	  (copy-seq (board-grid board)))
    new-board))

(defun board-equal-p (board-a board-b)
  "Return T if BOARD-A is equivalent to BOARD-B"
  (and (= (board-size board-a)
	  (board-size board-b))
       ;; Instead of comparing the grid, we compare the zobrist hash.
       ;; (grid-equal-p (board-grid board-a) (board-grid board-b))
       (= (board-hash board-a)
	  (board-hash board-b))))

(defun get-at (point board)
  "Return the group of stones at POINT on BOARD or NIL if empty."
  (aref (board-grid board)
	(point-index point (board-size board))))

(defun empty-p (point board)
  "Return T if POINT at BOARD is NIL."
  (not (get-at point board)))
