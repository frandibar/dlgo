(defpackage #:dlgo.tictactoe.tui
  (:use #:common-lisp)
  (:import-from #:dlgo.tictactoe.constant
		#:user-error
		#:text
		#:cross
		#:nought
		#:+board-size+)
  (:import-from #:dlgo.tictactoe.agent
		#:select-move)
  (:import-from #:dlgo.tictactoe.point
		#:make-point
		#:point-p
		#:point-row
		#:point-col
		#:point-on-grid-p
		#:point-to-coords
		#:point-equal-p
		#:+column-labels+)
  (:import-from #:dlgo.tictactoe
		#:make-game
		#:game-board
		#:game-turn
		#:game-winner
		#:game-moves
		#:game-over-p
		#:get-at
		#:undo-last-move
		#:place-stone-move
		#:opponent)
  (:import-from #:alexandria
		#:define-constant
		#:switch)
  (:import-from #:binding-arrows
		#:->>)
  (:export #:start-game
	   #:draw-game))

(in-package #:dlgo.tictactoe.tui)

;; Given that EQL returns NIL for strings, we provide EQUAL as :test.
;; Stones look good on black background, but reversed with light
;; background. I still prefer this to using X and O.
(define-constant +cross-stone+ "X" :test #'equal)
(define-constant +nought-stone+ "O" :test #'equal)
(define-constant +empty-point+ "." :test #'equal)

(define-condition user-error-invalid-command (error)
  ((text :initarg :text
	 :reader text)))

(defun start-game ()
  "Start a game of Go."
  (format t "Start a new game of TicTacToe.~%")
  (let ((game (make-game))
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
			      (eq 'cross (game-turn game)))
			  (ask-for-next-move game)
			  (select-move game))))
	    (case move
	      (undo (setf game (undo-last-move game)))
	      (help (show-help))
	      (debug (print game))
	      (quit (progn (show-goodbye)
			   (return)))
	      (t (setf game (place-stone-move move game)))))
	(user-error-invalid-command (condition)
	  (format t "~&~a~%" (text condition))
	  (show-help))
	(user-error (condition)
	  (format t "~&~a~%" (text condition)))))))

(defun text-stone (color)
  (ecase color
    (:cross +cross-stone+)
    (:nought +nought-stone+)))

(defun draw-stone (point board)
  "Return the string representation of POINT on BOARD."
  (assert (point-on-grid-p point) ()
			   "Point out of bounds.")
  (let ((stone (get-at point board)))
    (if stone
	(ecase stone
	  (cross +cross-stone+)
	  (nought +nought-stone+))
	+empty-point+)))

(defun point-on-left (point)
  "Return point to the left of POINT.
Does not check if it's out of the board."
  (make-point :col (1- (point-col point))
	      :row (point-row point)))

(defun draw-board (game)
  "Return a string representation of the board. Maximum size is
+big-board+."
  (let* ((board (game-board game))
	 (column-format "~&~3t~{~a ~}")
	 (column-labels (coerce +column-labels+ 'list))
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
      (loop for row from +board-size+ downto 1 do
	(format sb "~2d" row)
	(loop for col from 1 to +board-size+ do
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
	    (when (= col +board-size+)
	      (if (and last-move
		       (not board-unchanged-p)
		       (point-equal-p point last-move))
		  (format sb ")")
		  (format sb " ")))))
	(format sb "~2d~%" row))
      (format sb column-format column-labels)
      sb)))

(defun draw-game (game)
  "Return a string representation of the game."
  (with-output-to-string (sb)
    (format sb "~%")
    (format sb (draw-board game))
    (format sb "~%")
    (when (game-over-p game)
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

(defun column-to-int (column-label)
  "Return the number corresponding to COLUMN-LABEL."
  (1+ (position (char-upcase column-label)
		+column-labels+)))

(defun parse-move (answer default)
  (switch ((string-downcase answer) :test #'equal)
    ("undo" 'undo)
    ("help" 'help)
    ("quit" 'quit)
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
	     (not (point-on-grid-p move)))
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
  (format t "Available commands: undo, help, quit.~%")
  (force-output))
