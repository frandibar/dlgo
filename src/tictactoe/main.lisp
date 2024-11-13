(defpackage #:dlgo.tictactoe
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:define-constant
		#:curry
		#:rcurry
		#:set-equal)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:dlgo.tictactoe.constant
		#:cross
		#:nought
		#:user-error)
  (:import-from #:dlgo.tictactoe.board
		#:make-board
		#:deep-copy-board
		#:empty-p
		#:get-at)
  (:import-from #:dlgo.tictactoe.point
		#:point-p
		#:point-on-grid-p
		#:point-index)
  (:export #:place-stone-move
	   #:undo-last-move
	   #:valid-move-p
	   #:game-over-p
	   #:make-game
	   #:game-turn
	   #:game-winner
	   #:game-moves
	   #:game-board
	   #:during-game
	   #:make-snapshot
	   #:copy-game
	   #:opponent))

(in-package #:dlgo.tictactoe)

(defmacro during-game (game &rest body)
  `(progn
     (assert (not (game-over-p ,game)) () "Game is over.")
     (progn ,@body)))

(defun opponent (color)
  "Return the opponent color of COLOR."
  (ecase color
    (cross 'nought)
    (nought 'cross)))

(defstruct (game (:constructor make-game ()))
  (:turn 'cross)
  (:board (make-board))
  (:moves nil)
  (:snapshots nil)
  (:winner nil))

(defstruct (snapshot (:constructor make-snapshot (game)))
  (:turn (game-turn game))
  (:board (deep-copy-board (game-board game)))
  (:moves (copy-seq (game-moves game))))

(defun equal-stones-p (board indexes)
  (all-equal (mapcar #'(lambda (index) (aref board index))
		     indexes)))

(defun all-equal (lst)
  "Return T if all elements in LST are eq or LST is empty."
  (or (endp lst)
      (reduce #'(lambda (x y) (and (eq x y) y))
	      lst)))

(defun game-over-p (game)
  "A GAME is over if there is a winner or there are no more empty spaces."
  (let ((board (game-board game)))
    (or (game-winner game)
	(equal-stones-p board '(0 1 2)) ;; Col 1
	(equal-stones-p board '(3 4 5)) ;; Col 2
	(equal-stones-p board '(6 7 8)) ;; Col 3
	(equal-stones-p board '(0 3 6)) ;; Row 1
	(equal-stones-p board '(1 4 7)) ;; Row 2
	(equal-stones-p board '(2 5 8)) ;; Row 3
	(equal-stones-p board '(0 4 8)) ;; Diag 1
	(equal-stones-p board '(2 4 6)) ;; Diag 2
	;; This one must go last in case last move is a winning move.
	(when (not (some #'null board))
	  'tie))))

(defun undo-last-move (game)
  "Return a copy of GAME with the last move removed."
  (unless (game-moves game)
    (error 'user-error
	   :text "No moves to undo."))

  (let ((new-game (copy-game game))
	(last-state (first (game-snapshots game))))
    (setf (game-turn new-game)
	  (snapshot-turn last-state))
    (setf (game-board new-game)
	  (snapshot-board last-state))
    (pop (game-moves new-game))
    (pop (game-snapshots new-game))
    new-game))

(defun append-to-slot (item slot-getter game)
  "Return a list of ITEM in front of SLOT-GETTER for GAME."
  (append (list item) (funcall slot-getter game)))

(defun place-stone-move (point game)
  "Return a copy of GAME with a stone placed at POINT.
The color is determined by the turn in GAME."
  (during-game game
	       (let* ((snapshot (make-snapshot game))
		      (new-game (->> (copy-game game)
				  (place-stone point))))
		 (setf (game-winner new-game)
		       (game-over-p new-game))
		 (setf (game-turn new-game)
		       (opponent (game-turn new-game)))
		 (setf (game-moves new-game)
		       (append-to-slot point #'game-moves new-game))
		 (setf (game-snapshots new-game)
		       (append-to-slot snapshot #'game-snapshots new-game))
		 new-game)))

(defun place-stone (point game)
  "Return GAME after placing a stone at POINT."
  (let ((new-board (try-place-stone point
				    (game-turn game)
				    (game-board game))))
    (setf (game-board game) new-board)
    game))


(defun try-place-stone (point color board)
  "Return a copy of BOARD with a COLOR stone added at POINT on BOARD."

  (unless (point-on-grid-p point)
    (error 'user-error
	   :text "Cannot place a stone out of the board."))

  (unless (empty-p point board)
    (error 'user-error
	   :text "Cannot place a stone on an occupied point."))

  (let ((new-board (deep-copy-board board)))

    ;; Place stone
    (setf (aref new-board
		(point-index point))
	  color)

    new-board))

(defun move-p (move)
  "Return T if MOVE is a move.
Does not check if the move is valid in the context of a game."
  (point-p move))

(defun valid-move-p (move game)
  "Return T if MOVE is valid in GAME"
  (and (not (game-over-p game))
       (empty-p move (game-board game))))
