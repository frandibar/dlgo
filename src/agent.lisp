(defpackage #:dlgo.agent
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:curry
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:dlgo.constant
		#:pass)
  (:import-from #:dlgo
		#:board-size
		#:game-board
		#:group-color
		#:game-turn
		#:get-at
		#:valid-move-p
		#:empty-p)
  (:import-from #:dlgo.point
		#:point-col
		#:point-row
		#:point-on-grid-p
		#:point-neighbors-on-grid
		#:make-point)
  (:export #:select-move))

(in-package #:dlgo.agent)

(defun diagonal-neighbors (point)
  (let ((col (point-col point))
	(row (point-row point)))
    (list
     (make-point :col (1- col) :row (1- row))
     (make-point :col (1- col) :row (1+ row))
     (make-point :col (1+ col) :row (1- row))
     (make-point :col (1+ col) :row (1+ row)))))

(defun eye-p (point color board)
  "Return T if POINT is an eye.
That is, it is empty on the BOARD and all adjacent points and at least
three out of four diagonally adjacent points are of the same COLOR on
BOARD."
  (let ((size (board-size board)))
    (and (empty-p point board)
	 ;; All neigbors of same color as COLOR.
	 (let* ((side-neighbors (point-neighbors-on-grid point size))
		(side-groups (->> side-neighbors
			       (mapcar (rcurry #'get-at board))
			       (remove nil))))
	   (if (< 4 (length side-groups))
	       nil
	       (->> side-groups
		 (mapcar #'group-color)
		 (every (curry #'eq color)))))
	 ;; If point is in the middle of the board, 3 diagonal neighbors
	 ;; should be of same color.
	 ;; On the edge, all of them.
	 (let* ((diag-neighbors-on-grid
		  (->> (diagonal-neighbors point)
		    (remove-if-not (rcurry #'point-on-grid-p size))))
		(diag-groups (->> diag-neighbors-on-grid
			       (mapcar (rcurry #'get-at board))
			       (remove nil)))
		(off-board (- 4 (length diag-neighbors-on-grid)))
		(friendly (->> diag-groups
			    (mapcar #'group-color)
			    (remove-if-not (curry #'eq color))
			    length)))
	   (if (> 0 off-board)
	       (= 4 (+ off-board friendly))
	       (>= friendly 3))))))

(defun select-move (game)
  "Return a random valid move that preserves a group's eyes."
  (let* ((size (board-size (game-board game)))
	 (rowcols (alexandria:iota size :start 1))
	 (candidates
	   (->>
	     (apply #'append (mapcar (lambda (a) (mapcar (lambda (b) (list a b))
						    rowcols))
				     rowcols))
	     (mapcar #'(lambda (rowcol) (make-point :col (first rowcol)
					       :row (second rowcol))))
	     (remove-if-not (rcurry #'valid-move-p game))
	     (remove-if (rcurry #'eye-p
				(game-turn game)
				(game-board game))))))
    (if candidates
	(nth (random (length candidates)) candidates)
	'pass)))
