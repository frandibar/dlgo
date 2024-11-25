(defpackage #:dlgo.agent
  (:use #:common-lisp)

  (:import-from #:alexandria
		#:curry
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)

  (:import-from #:dlgo
		#:board-size
		#:get-at
		#:valid-move-p
		#:empty-p)
  (:import-from #:dlgo.constant
		#:pass)
  (:import-from #:dlgo.game
		#:group-color
		#:game-board
		#:game-turn)
  (:import-from #:dlgo.point
		#:point-col
		#:point-row
		#:point-on-grid-p
		#:point-neighbors
		#:make-point)
  (:import-from #:dlgo.util
		#:and-then)

  (:export #:select-move))

(in-package #:dlgo.agent)

(defun all-diagonal-neighbors (point)
  (let ((col (point-col point))
	(row (point-row point)))
    (list
     (make-point :col (1- col) :row (1- row))
     (make-point :col (1- col) :row (1+ row))
     (make-point :col (1+ col) :row (1- row))
     (make-point :col (1+ col) :row (1+ row)))))

(defun diagonal-neighbors (point size)
  (remove-if-not (rcurry #'point-on-grid-p size)
		 (all-diagonal-neighbors point)))

(defun eye-p (point color board)
  "Return T if POINT is an eye.
That is, it is empty on the BOARD and all adjacent points and at least
three out of four diagonally adjacent points are of the same COLOR on
BOARD."
  ;; Points marked as 'e' are eyes.

  ;; . . . . .       . . . . .       . . .
  ;; . ○ ○ ○ .       . . . . .       ○ ○ .
  ;; . ○ e ○ .       . . . . .       e ○ .
  ;; . ○ ○ . .       . ○ ○ ○ .
  ;; . . . . .       . ○ e ○ .

  ;; If point is in the middle of the board, 3 diagonal neighbors
  ;; should be of same color.  On the edge, all 2 of them, on the
  ;; corner, the only one.
  (let* ((size (board-size board))
	 (adjacent-neighbors-same-color-p
	   (->> (point-neighbors point size)
	     (mapcar (rcurry #'get-at board))
	     (mapcar (rcurry #'and-then #'group-color))
	     (every (curry #'eq color))))
	 (diagonal-neighbors-same-color-p
	   (let* ((diag-neighbors (diagonal-neighbors point size))
		  (diag-groups (->> diag-neighbors
				 (mapcar (rcurry #'get-at board))))
		  (off-board (- 4 (length diag-neighbors)))
		  (friendly (->> diag-groups
			      (remove nil)
			      (mapcar #'group-color)
			      (remove-if-not (curry #'eq color))
			      length)))
	     (if (> off-board 0)
		 (= 4 (+ off-board friendly))
		 (>= friendly 3)))))

    (and (empty-p point board)
	 adjacent-neighbors-same-color-p
	 diagonal-neighbors-same-color-p)))

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
