(defpackage dlgo.sgf
  (:use #:common-lisp)
  (:import-from #:dlgo.constant
		#:pass
		#:resign)
  (:import-from #:dlgo.point
		#:point-p
		#:point-col
		#:point-row)
  (:import-from #:dlgo
		#:game-moves
		#:game-board
		#:game-komi
		#:board-size)
  (:export #:make-sgf))

(in-package :dlgo.sgf)

(defun make-sgf (game)
  "Return a Smart Game Format representation of GAME, as a string."
  (format nil "(~a~%~a)"
	  (sgf-header game)
	  (sgf-moves (game-moves game)
		     (board-size (game-board game)))))

(defun current-date-as-yyyy-mm-dd ()
  "Return the current date as string in yyyy-mm-dd format."
  (let* ((date-parts (multiple-value-list (get-decoded-time)))
	 (year (sixth date-parts))
	 (month (fifth date-parts))
	 (day (fourth date-parts)))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun sgf-header (game)
  "Return headers for GAME."
  ;; Use 2 decimals for komi (KM)
  (format nil ";GM[1]
FF[4]
CA[UTF-8]
US[dlgo]
RU[Chinese]
SZ[~d]
KM[~$]
HA[]
PW[]
PB[]
DT[~a]
"
	  (board-size (game-board game))
	  (game-komi game)
	  (current-date-as-yyyy-mm-dd)))

(defun sgf-moves (game-moves board-size)
  "Return string with moves in sgf format."
  (let* ((moves-without-resign (if (eq 'resign (first game-moves))
				   (rest game-moves)
				   game-moves))
	 (moves (mapcar #'(lambda (move) (move-annotation board-size
							  move))
			(reverse moves-without-resign))))
    (loop for move in moves
	  for i from 0 below (length moves)
	  for turn = (cond
		       ((eq 'resign move) "")
		       ((evenp i) ";B")
		       (t ";W"))
	  collect (format nil "~a~a" turn move))))

(defun move-annotation (board-size move)
  (if (eq 'resign move)
      ""
      (format nil "[~a]"
	      (cond
		((point-p move) (sgf-coordinate board-size
						      move))
		((eq 'pass move) "")
		(t (error "Invalid move."))))))

(defun sgf-coordinate (board-size point)
  "Return the sgf coordinate representation of POINT.
Biggest board size is 19."

  ;; For a board of size 19, the mappings are:
  ;; A1->as, A19->aa, T1->ss, T19->sa

  ;; For a board of size 9, the mappings are:
  ;; A1->ai, A9->aa, J1->ii, J9->ia

  (let* ((letters (subseq "abcdefghijklmnopqrs" 0 board-size))
	 (ascending-cols (format nil " ~a" letters))
	 (descending-rows (format nil " ~a" (reverse letters))))
    (format nil
	    "~c~c"
	    (char ascending-cols (point-col point))
	    (char descending-rows (point-row point)))))
