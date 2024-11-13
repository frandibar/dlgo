(defpackage #:dlgo.tictactoe.point
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:rcurry
		#:define-constant)
  (:import-from #:dlgo.tictactoe.constant
		#:+board-size+)
  (:export #:make-point
	   #:point-col
	   #:point-row
	   #:point-p
	   #:point-equal-p
	   #:point-on-grid
	   #:point-to-coords
	   #:point-index
	   #:index-to-point
	   #:+column-labels+))

(in-package #:dlgo.tictactoe.point)

(defstruct point
  "A point on the board."
  col row)

(defun point-equal-p (point-a point-b)
  "Return T if POINT-A is equal to POINT-B."
  (and
   (= (point-col point-a)
      (point-col point-b))
   (= (point-row point-a)
      (point-row point-b))))

(defun point-neighbors (point)
  "Return list of points adjacent to POINT."
  (let ((col (point-col point))
	(row (point-row point)))
    (list
     (make-point :col col :row (1- row))
     (make-point :col col :row (1+ row))
     (make-point :col (1- col) :row row)
     (make-point :col (1+ col) :row row))))

(defun point-neighbors-on-grid (point size)
  (remove-if-not (rcurry #'point-on-grid-p size)
		 (point-neighbors point)))

(define-constant +column-labels+ "ABC"
  :test #'equal)

(defun point-to-coords (point)
  "Return a string with the board coordinates respresenting POINT.
i.e. C4 corresponds to (make-point :col 3 :row 4)."
  (format nil "~c~d"
	  (char +column-labels+ (1- (point-col point)))
	  (point-row point)))

(defun point-index (point)
  "Return the corresponding vector index for POINT in the board grid."
  ;; The order is A1..A3,...,C1..C3.
  (+ (* (1- (point-col point)) +board-size+)
     (1- (point-row point))))

(defun index-to-point (index)
  "Return the point that corresponds to INDEX. This is the inverse of POINT-INDEX. It assumes that a 2D board is implemented as a 1D array."
  (multiple-value-bind (col row) (floor index +board-size+)
    (make-point :col (1+ col)
		:row (1+ row))))

(defun point-on-grid-p (point)
  "Return POINT if it's within the bounds of the board,
otherwise NIL."
  (let ((col (point-col point))
	(row (point-row point)))
    (when (and (<= 1 col +board-size+)
	       (<= 1 row +board-size+))
      point)))
