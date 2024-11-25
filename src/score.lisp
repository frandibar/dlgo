(defpackage #:dlgo.score
  (:use #:common-lisp)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:alexandria
		#:rcurry)
  (:import-from #:dlgo.util
		#:and-then)
  (:import-from #:dlgo.constant
		#:black
		#:white)
  (:import-from #:dlgo.game
		#:game-winner
		#:game-board
		#:game-info
		#:game-komi
		#:game-moves
		#:group-color
		#:group-stones)
  (:import-from #:dlgo.point
		#:point-neighbors
		#:point-index
		#:index-to-point)
  (:import-from #:dlgo.board
		#:board-size
		#:board-grid)
  (:export #:game-result))

(in-package #:dlgo.score)

(defstruct (area-score (:conc-name score-))
  (black-territory 0)
  (white-territory 0)
  (black-stones 0)
  (white-stones 0)
  (dame 0))

(defun game-result (game)
  "Return a cons with the result string such as B+4 and the winner."
  (if (eq 'resign (first (game-moves game)))
      (case (game-winner game)
	(black (cons "B+Resign" 'black))
	(white (cons "W+Resign" 'white))
	(t (cons "" nil)))
      (let* ((score (score-game game))
	     (black-score (+ (score-black-territory score)
			     (score-black-stones score)))
	     (white-score (+ (game-komi (game-info game))
			     (score-white-territory score)
			     (score-white-stones score)))
	     (diff (abs (- black-score white-score)))
	     (result (if (> black-score white-score)
			 (cons "B" 'black)
			 (cons "W" 'white))))
	(cons (format nil "~a+~a" (car result) diff)
	      (cdr result)))))

(defun score-game (game)
  (calculate-score (evaluate-territory (game-board game))))

(defun calculate-score (board-status)
  (make-area-score
   :black-territory (count 'black-territory board-status)
   :white-territory (count 'white-territory board-status)
   :black-stones (count 'black board-status)
   :white-stones (count 'white board-status)
   :dame (count 'dame board-status)))

(defun evaluate-territory (board)
  "Each point on the board is either a:
- stone
- black or white territory
- dame.
It assumes there are no dead groups."

  ;; board-status [i] holds either 'black, 'white, 'territory-black,
  ;; 'territory-white, 'dame.
  (let* ((grid (board-grid board))
	 (board-status (map 'vector
			    (rcurry #'and-then #'group-color)
			    grid)))
    ;; board-status now holds 'black and 'white values only. We must
    ;; now deal with the empty points to determine if they're dame or
    ;; B/W territory.
    (dotimes (idx (length grid))
      (unless (aref board-status idx)
	(let* ((region (collect-region idx board '()))
	       (is-territory (= 1 (length (second region))))
	       (fill-with
		 (if is-territory
		     (ecase (first (second region))
		       (black 'black-territory)
		       (white 'white-territory))
		     'dame)))
	  (dolist (index (first region))
	    (setf (aref board-status index)
		  fill-with)))))
    board-status))

(defun collect-region (start-index board visited)
  "Return a cons where the first element is a list of contiguous points
that belong to the same region, and the second a set of delimiting
border colors."
  (if (member start-index visited)
      (list nil nil)
      (let* ((size (board-size board))
	     (all-indexes (list start-index))
	     (all-borders '())
	     (grid (board-grid board))
	     (here (and-then (aref grid start-index)
			     #'group-color))
	     (neighbor-indexes (->>
				 (index-to-point start-index size)
				 (funcall (rcurry #'point-neighbors size))
				 (mapcar (rcurry #'point-index size)))))
	(pushnew start-index visited)
	(loop for neighbor-index in neighbor-indexes do
	  (let ((neighbor (and-then (aref grid neighbor-index)
				    #'group-color)))
	    (if (eq neighbor here)
		(progn
		  (let ((region (collect-region neighbor-index
						board
						visited)))
		    (dolist (i (first region))
		      (push i all-indexes))
		    (dolist (border-color (second region))
		      (pushnew border-color all-borders))))
		(pushnew (and-then (aref grid neighbor-index)
				   #'group-color)
			 all-borders))))
	(list all-indexes all-borders))))
