(defpackage #:dlgo.tui
  (:use #:common-lisp)

  (:import-from #:alexandria
		#:define-constant
		#:switch)
  (:import-from #:binding-arrows
		#:->>)

  (:import-from #:dlgo
		#:game-over-p
		#:get-at
		#:undo-last-move
		#:resign-move
		#:pass-move
		#:place-stone-move
		#:opponent)
  (:import-from #:dlgo.mcts
		#:select-move)
  (:import-from #:dlgo.board
		#:board-size)
  (:import-from #:dlgo.constant
		#:+small-board+
		#:+medium-board+
		#:+big-board+
		#:user-error
		#:text
		#:black
		#:white
		#:pass
		#:resign)
  (:import-from #:dlgo.game
		#:make-game
		#:game-board
		#:game-captures
		#:game-turn
		#:game-winner
		#:game-moves
		#:group-color)
  (:import-from #:dlgo.point
		#:+column-labels+
		#:make-point
		#:point-equal-p
		#:point-p
		#:point-col
		#:point-row
		#:point-on-grid-p
		#:point-to-coords)
  (:import-from #:dlgo.sgf
		#:make-sgf)

  (:export #:start-game
	   #:draw-game))

(in-package #:dlgo.tui)

;; Given that EQL returns NIL for strings, we provide EQUAL as :test.
;; Stones look good on black background, but reversed with light
;; background. I still prefer this to using X and O.
(define-constant +black-stone+ "○" :test #'equal)
(define-constant +white-stone+ "●" :test #'equal)
(define-constant +empty-point+ "." :test #'equal)
(define-constant +star-point+ "+" :test #'equal)

(define-condition user-error-invalid-command (error)
  ((text :initarg :text
	 :reader text)))

(defun start-game ()
  "Start a game of Go."
  (format t "Start a new game of GO.~%")
  (let ((game (make-game (ask-for-board-size)))
	(against-human-p (not (ask-for-bot-opponent))))
    (format t "~%")
    (show-help)
    (loop
      (format t (draw-game game))
      (force-output)
      (when (game-over-p game)
	(show-goodbye)
	(return))
      (format t "~%")
      (handler-case
	  (let ((move (if (or against-human-p
			      (eq 'black (game-turn game)))
			  (ask-for-next-move game)
			  (select-move game))))
	    (case move
	      (pass (setf game (pass-move game)))
	      (resign (setf game (resign-move game)))
	      (undo (setf game (undo-last-move game)))
	      (help (show-help))
	      (quit (progn (show-goodbye)
			   (return)))
	      (save (save-game game))
	      (debug (print game))
	      (t (setf game (place-stone-move move game)))))
	(user-error-invalid-command (condition)
	  (format t "~&~a~%" (text condition))
	  (show-help))
	(user-error (condition)
	  (format t "~&~a~%" (text condition)))))))

(defun text-stone (color)
  (ecase color
    (black +black-stone+)
    (white +white-stone+)))

(defun draw-stone (point board)
  "Return the string representation of POINT on BOARD."
  (assert (point-on-grid-p point
			   (board-size board)) ()
			   "Point out of bounds.")
  (let* ((group (get-at point board)))
    (if group
	(ecase (group-color group)
	  (black +black-stone+)
	  (white +white-stone+))
	(if (star-point-p point (board-size board))
	    +star-point+
	    +empty-point+))))

(defun star-point-p (point size)
  "Return T if POINT is a star point on a board of size SIZE."
  (let ((star-points
	  (mapcar #'(lambda (p) (make-point :col (first p)
				       :row (second p)))
		  (cond ((= size +small-board+)
			 '((3 3)
			   (7 3)
			   (5 5)
			   (3 7)
			   (7 7)))
			((= size +medium-board+)
			 '((4 4)
			   (10 4)
			   (7 7)
			   (4 10)
			   (10 10)))
			((= size +big-board+)
			 '((4 4)
			   (4 10)
			   (16 4)
			   (10 4)
			   (10 10)
			   (10 16)
			   (4 16)
			   (16 10)
			   (16 16)))))))
    (member point star-points
	    :test #'point-equal-p)))

(defun point-on-left (point)
  "Return point to the left of POINT.
Does not check if it's out of the board."
  (make-point :col (1- (point-col point))
	      :row (point-row point)))

(defun draw-board (game)
  "Return a string representation of the board. Maximum size is
+big-board+."
  (let* ((board (game-board game))
	 (size (min +big-board+ (board-size board)))
	 (column-format "~&~3t~{~a ~}")
	 (column-labels (subseq
			 (coerce +column-labels+ 'list) 0 size))
	 (last-move (first (game-moves game))))
    (with-output-to-string (sb)
      (format sb column-format column-labels)
      (when (game-moves game)
	(format sb "     Last move: ~a ~a"
		(opponent (game-turn game))
		(if (member last-move (list 'pass 'resign))
		    last-move
		    (point-to-coords last-move))))
      (format sb "~%")
      (loop for row from size downto 1 do
	(format sb "~2d" row)
	(loop for col from 1 to size do
	  (let ((point (make-point :col col :row row))
		(board-unchanged-p (member last-move (list 'pass 'resign))))
	    (if board-unchanged-p
		(format sb " ")
		(if (and last-move (point-equal-p point
						  last-move))
		    (format sb "(")
		    (if (and last-move
			     (point-equal-p (point-on-left point)
					    last-move))
			(format sb ")")
			(format sb " "))))
	    (format sb "~a" (draw-stone point board))
	    ;; Draw closing ) for last move on last column.
	    (when (= col size)
	      (if (and last-move
		       (not board-unchanged-p)
		       (point-equal-p point last-move))
		  (format sb ")")
		  (format sb " ")))))
	(format sb "~2d~%" row))
      (format sb column-format column-labels)
      sb)))

(defun draw-captures (game)
  "Return a string legend with total captures."
  (let ((captures (game-captures game))
	;; Use ~p directive to deal with plural.
	(format-string "~3t~a (~a) has captured ~d stone~p~%"))
    (with-output-to-string (string-buffer)
      (dolist (color (list 'black 'white))
	(format string-buffer format-string
		color
		(text-stone color)
		(slot-value captures color)
		(slot-value captures color)))
      string-buffer)))

(defun draw-game (game)
  "Return a string representation of the game."
  (with-output-to-string (sb)
    (format sb "~%")
    (format sb (draw-captures game))
    (format sb "~%")
    (format sb (draw-board game))
    (format sb "~%")
    (when (game-over-p game)
      ;; TODO: show game result
      (format sb "~%Game is over. Winner is ~a~%" (game-winner game)))
    sb))

(defun ask-for-input (question parser default)
  (loop
    (handler-case
	(progn
	  (format t question)
	  (force-output)
	  (return (funcall parser
			   (string-trim " " (read-line))
			   default)))
      (user-error (condition)
	(format t "~&~a~%" (text condition))))))

(defun parse-board-size (answer default)
  (switch (answer :test #'equal)
    ("" default)
    ("9" +small-board+)
    ("13" +medium-board+)
    ("19" +big-board+)
    (t (error 'user-error
	      :text "Please enter '9', '13' or '19'."))))

(defun ask-for-board-size ()
  (ask-for-input (format nil "~&Size (~a/~a/~a): "
			 +small-board+
			 +medium-board+
			 +big-board+)
		 #'parse-board-size
		 +small-board+))

(defun column-to-int (column-label)
  "Return the number corresponding to COLUMN-LABEL."
  (1+ (position (char-upcase column-label)
		+column-labels+)))

(defun parse-move (answer default)
  (switch ((string-downcase answer) :test #'equal)
    ("pass" 'pass)
    ("resign" 'resign)
    ("undo" 'undo)
    ("help" 'help)
    ("quit" 'quit)
    ("save" 'save)
    ("debug" 'debug)
    ("" default)
    (t
     (handler-case
	 (let ((col (column-to-int (char answer 0)))
	       (row (parse-integer (subseq answer 1))))
	   (make-point :col col :row row))
       (error (condition)
	 ;; Convert the error into a user error.
	 (declare (ignore condition))
	 (error 'user-error-invalid-command
		:text "Please enter a valid command."))))))

(defun ask-for-next-move (game)
  (let ((move
	  (ask-for-input (format nil "~&~a(~d): "
				 (game-turn game)
				 (1+ (length (game-moves game))))
			 #'parse-move
			 'help)))
    (if (and (point-p move)
	     (not (point-on-grid-p move
				   (board-size (game-board game)))))
	(error 'user-error
	       :text "Please enter a valid move.")
	move)))

(defun parse-yes-or-no (answer default)
  "Return T if ANSWER is y, NIL if n, otherwise DEFAULT."
  (switch ((string-downcase answer) :test #'equal)
    ("y" t)
    ("n" nil)
    ("" default)
    (t (error 'user-error
	      :text "Please answer 'y' or 'n'."))))

(defun ask-for-bot-opponent ()
  (ask-for-input "Play against computer (Y/n): "
		 #'parse-yes-or-no
		 t))

(defun show-goodbye ()
  (format t "~&Goodbye!~%")
  (force-output))

(defun show-help ()
  (format t "Available commands: pass, resign, undo, help, save, debug, quit.~%")
  (force-output))

(defun save-game (game)
  (format t "~&Output file name: ")
  (force-output)
  (let ((filename (read-line))
	(sgf (make-sgf game)))
    (with-open-file (*standard-output* filename
				       :direction :output
				       :if-exists :supersede)
      (format t "~a" sgf))
    (format t "~&Saved game as ~a~%" filename)
    (force-output)))
