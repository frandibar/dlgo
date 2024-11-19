(defpackage #:dlgo
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:define-constant
		#:curry
		#:rcurry
		#:set-equal)
  (:import-from #:split-sequence
		#:split-sequence-if)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:dlgo.constant
		#:black
		#:white
		#:pass
		#:resign
		#:user-error)
  (:import-from #:dlgo.util
		#:partition)
  (:import-from #:dlgo.board
		#:make-board
		#:board-size
		#:board-grid
		#:board-hash
		#:deep-copy-board
		#:board-equal-p
		#:empty-p
		#:get-at)
  (:import-from #:dlgo.point
		#:point-p
		#:point-on-grid-p
		#:point-index
		#:point-equal-p
		#:point-neighbors-on-grid)
  (:import-from #:dlgo.zobrist
		#:zobrist-hash)
  (:export #:pass-move
	   #:resign-move
	   #:place-stone-move
	   #:undo-last-move
	   #:valid-move-p
	   #:game-over-p
	   #:make-game
	   #:game-turn
	   #:game-captures
	   #:game-winner
	   #:game-moves
	   #:game-board
	   #:during-game
	   #:captures-black
	   #:captures-white
	   #:make-snapshot
	   #:copy-game
	   #:opponent
	   #:make-player-info
	   #:make-game-info
	   #:game-info
	   #:game-komi
	   #:game-handicap
	   #:game-player-black
	   #:game-player-white
	   #:player-name
	   #:player-level))

(in-package #:dlgo)

(defmacro during-game (game &rest body)
  `(progn
     (assert (not (game-over-p ,game)) () "Game is over.")
     (progn ,@body)))

(defun opponent (color)
  "Return the opponent color of COLOR."
  (ecase color
    (black 'white)
    (white 'black)))

;; This figure shows three black groups (○) and two white ones
;; (●). White's big group has 6 liberties and the single white stone
;; has 3.

;; . . . . . .
;; . ● . ● ○ .
;; . . . ● ○ .
;; . . ● . ○ .
;; . . ● ○ . .
;; . . . . . .

(defstruct (group (:constructor make-group (color)))
  "A group represents a chain of connected stones of the same color."
  color
  stones
  liberties)

(defmacro set-group-unique-list
    (slot-getter list-of-points group)
  "Macro to enforce that a group slot that requires it's elements to be unique holds."
  `(let ((new-group (copy-group ,group)))
     (setf (,slot-getter new-group)
	   (remove-duplicates ,list-of-points
			      :test #'point-equal-p))
     new-group))

(defun set-group-stones (stones group)
  (set-group-unique-list group-stones stones group))

(defun set-group-liberties (liberties group)
  (set-group-unique-list group-liberties liberties group))

(defstruct captures
  (black 0)
  (white 0))

(defstruct (player-info (:conc-name player-))
  name
  level)

(defstruct (game-info (:conc-name game-))
  (komi 6.5)
  (handicap 0)
  (player-black (make-player-info))
  (player-white (make-player-info)))

(defstruct (game (:constructor make-game (size)))
  (turn 'black)
  (board (make-board size))
  (captures (make-captures))
  (moves '())
  (info (make-game-info))
  (snapshots '())
  (winner nil))

(defstruct (snapshot (:constructor make-snapshot (game)))
  (turn (game-turn game))
  (board (deep-copy-board (game-board game)))
  (captures (copy-captures (game-captures game)))
  (moves (copy-seq (game-moves game))))

(defun game-over-p (game)
  "A GAME is over if there is a winner or last two moves where a pass."
  (let ((moves (game-moves game)))
    (or (equal (list (first moves) (second moves))
	       (list 'pass 'pass))
	(game-winner game))))

(defun group-captured-p (group)
  "Return T if GROUP has no liberties left."
  (null (group-liberties group)))

(defun group-equal-p (group-a group-b)
  "Return T if GROUP-A is the same as GROUP-B."
  (and (equal (group-color group-a)
	      (group-color group-b))

       (set-equal (group-stones group-a)
		  (group-stones group-b)
		  :test #'point-equal-p)

       (set-equal (group-liberties group-a)
		  (group-liberties group-b)
		  :test #'point-equal-p)))

(defun group-equal-nonstrict-p (group-a group-b)
  "Return T if GROUP-A is the same as GROUP-B, allows NIL values."
  (if (equal (mapcar #'type-of (list group-a group-b))
	     '(group group))
      (group-equal-p group-a group-b)
      (equal group-a group-b)))

(defun remove-liberty (point group)
  "Return a unique list of liberties of GROUP with POINT removed."
  (set-difference (group-liberties group)
		  (list point)
		  :test #'point-equal-p))

(defun add-liberty (point group)
  "Return a unique list of liberties of GROUP with POINT added."
  (adjoin point
	  (group-liberties group)
	  :test #'point-equal-p))

(defun merge-groups (group-a group-b)
  "Return a group that results from merging GROUP-A and GROUP-B."
  (assert (eq (group-color group-a)
	      (group-color group-b))
	  ()
	  "Cannot merge groups of different color.")
  (let* ((combined-stones (append (group-stones group-a)
				  (group-stones group-b)))
	 (combined-liberties (append (group-liberties group-a)
				     (group-liberties group-b)))
	 (liberties (set-difference combined-liberties
				    combined-stones
				    :test #'point-equal-p)))

    (->> (make-group (group-color group-a))
      (set-group-stones combined-stones)
      (set-group-liberties liberties))))

(defun grid-equal-p (grid-a grid-b)
  (loop for group-a across grid-a
	for group-b across grid-b
	always (group-equal-nonstrict-p group-a group-b)))

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
    (setf (game-captures new-game)
	  (snapshot-captures last-state))
    (pop (game-moves new-game))
    (pop (game-snapshots new-game))
    new-game))

(defun append-to-slot (item slot-getter game)
  "Return a list of ITEM in front of SLOT-GETTER for GAME."
  (append (list item) (funcall slot-getter game)))

(defun pass-move (game)
  "Return a copy of GAME with an additional pass move."
  (during-game game
	       (let ((new-game (copy-game game))
		     (snapshot (make-snapshot game)))
		 (setf (game-turn new-game)
		       (opponent (game-turn new-game)))
		 (setf (game-moves new-game)
		       (append-to-slot 'pass #'game-moves new-game))
		 (setf (game-snapshots new-game)
		       (append-to-slot snapshot #'game-snapshots new-game))
		 new-game)))

(defun resign-move (game)
  "Return a copy of GAME with an additional resign move."
  (during-game game
	       (let ((new-game (copy-game game)))
		 (setf (game-moves new-game)
		       (append-to-slot 'resign #'game-moves new-game))
		 (setf (game-winner new-game)
		       (opponent (game-turn new-game)))
		 new-game)))

(defun place-stone-move (point game)
  "Return a copy of GAME with a stone placed at POINT.
The color is determined by the turn in GAME."
  (during-game game
	       (let* ((snapshot (make-snapshot game))
		      (new-game (->> (copy-game game)
				  (place-stone point))))
		 (setf (game-turn new-game)
		       (opponent (game-turn new-game)))
		 (setf (game-moves new-game)
		       (append-to-slot point #'game-moves new-game))
		 (setf (game-snapshots new-game)
		       (append-to-slot snapshot #'game-snapshots new-game))
		 new-game)))

(defun place-stone (point game)
  "Return GAME after placing a stone at POINT."
  (multiple-value-bind (new-board captures)
      (try-place-stone point
		       (game-turn game)
		       (game-board game))
    (cond
      ((move-violates-ko-rule new-board game)
       (error 'user-error
	      :text "Cannot place a stone that violates the KO rule."))
      ((self-capture-p point new-board)
       (error 'user-error
	      :text "Cannot place a stone with no liberties."))
      (t
       (setf (game-board game) new-board)
       (setf (game-captures game)
	     (score-capture (game-turn game)
			    captures
			    (game-captures game)))
       game))))

(defun move-violates-ko-rule (board game)
  "Return T if BOARD is in a state in which the situational superko rule
is violated. That is, the BOARD is the same as in a previous state in
GAME, for the player whose turn is next."
  (let ((prev-states (->> (game-snapshots game)
		       (remove-if #'(lambda (snapshot)
				      (equal (game-turn game)
					     (snapshot-turn snapshot))))
		       (mapcar #'snapshot-board))))
    (when prev-states
      (not (loop for state in prev-states
		 never (board-equal-p board state))))))

(defun unique-groups (points board)
  "Return groups at POINTS on BOARD without duplicates.
It is assumed that GET-AT returns non NIL for each point in POINTS."
  (remove-duplicates
   (mapcar (rcurry #'get-at board)
	   points)
   :test #'group-equal-p))

(defun apply-hash (point color board)
  "Return a new hash that results from adding/removing a stone of COLOR
at POINT on BOARD."
  (logxor (board-hash board)
	  (zobrist-hash point color)))

(defun try-place-stone (point color board)
  "Return a copy of BOARD with a COLOR stone added at POINT on BOARD, and
the number of captured stones."

  (unless (point-on-grid-p point (board-size board))
    (error 'user-error
	   :text "Cannot place a stone out of the board."))

  (unless (empty-p point board)
    (error 'user-error
	   :text "Cannot place a stone on an occupied point."))

  (let ((new-board (deep-copy-board board))
	(neighbors (point-neighbors-on-grid point
					    (board-size board))))

    (setf (board-hash new-board)
	  (apply-hash point color new-board))

    ;; Split between occupied and empty neighbors.
    (multiple-value-bind (occupied-neighbors
			  empty-neighbors)
	(partition (rcurry #'get-at board)
		   neighbors)

      ;; Split occupied black or white neighbors.
      (multiple-value-bind (same-color-neighbors
			    opponent-neighbors)
	  (partition #'(lambda (neighbor)
			 (eq color
			     (group-color (get-at neighbor board))))
		     occupied-neighbors)

	;; Merge placed stone with adjacent same color groups.
	(let* ((adjacent-same-color-groups
		 (unique-groups same-color-neighbors board))

	       (single-stone-group
		 (->> (make-group color)
		   (set-group-stones (list point))
		   (set-group-liberties empty-neighbors)))

	       (new-group
		 (reduce #'merge-groups
			 adjacent-same-color-groups
			 :initial-value single-stone-group)))

	  (add-group! new-group new-board))

	;; Deal with opponent groups:
	;; - Reduce liberties of opponent groups adjacent to point.
	;; - Remove opponent groups that have no liberties.
	(let* ((adjacent-opponent-groups
		 (unique-groups opponent-neighbors board))

	       (groups-to-update
		 (mapcar #'(lambda (group)
			     (->> (copy-group group)
			       (set-group-liberties
				(remove-liberty point group))))
			 adjacent-opponent-groups))

	       (total-captured-stones
		 (reduce #'+
			 (->> groups-to-update
			   (remove-if-not #'group-captured-p)
			   (mapcar #'group-stones)
			   (mapcar #'length)))))

	  (dolist (group groups-to-update)
	    (add-group! group new-board))

	  ;; Remove captured groups.
	  (dolist (group (remove-if-not #'group-captured-p
					groups-to-update))
	    (remove-group! group new-board))

	  (values new-board
		  total-captured-stones))))))

(defun add-group! (group board)
  "Update BOARD with GROUP, that is, for each stone in GROUP, update
BOARD grid.

ATTENTION: this function mutates BOARD."
  (dolist (stone (group-stones group))
    (setf (aref (board-grid board)
		(point-index stone (board-size board)))
	  group)))

(defun group-hash (group)
  "Obtain the zobrist hash of the group."
  (->> (group-stones group)
    (mapcar (rcurry #'zobrist-hash (group-color group)))
    (reduce #'logxor)))

(defun remove-group! (group board)
  "Removes GROUP from BOARD.
ATTENTION: this function mutates BOARD.
"
  (setf (board-hash board)
	(logxor (board-hash board) (group-hash group)))

  ;; Increment the liberties of groups adjacent to GROUP.
  (let ((size (board-size board)))
    (dolist (point (group-stones group))
      (let ((groups-to-update
	      (->> (point-neighbors-on-grid point size)
		(mapcar (rcurry #'get-at board))
		(remove nil)
		(remove-if (curry #'group-equal-p group)))))

	(dolist (group (remove-duplicates groups-to-update
					  :test #'group-equal-p))
	  (let ((new-group (copy-group group)))
	    (setf (group-liberties new-group)
		  (add-liberty point new-group))
	    (add-group! new-group board))))

      ;; Mark removed stone as empty on board.
      (setf (aref (board-grid board)
		  (point-index point size))
	    nil))))

(defun score-capture (color number-of-stones captures)
  "Return a copy of CAPTURES with NUMBER-OF-STONES added to COLOR."
  (let ((new-captures (copy-captures captures)))
    (incf (slot-value new-captures color) number-of-stones)
    new-captures))

(defun self-capture-p (point board)
  "Return T if the group at POINT on BOARD has no liberties left."
  (= 0 (length (group-liberties (get-at point board)))))

(defun move-p (move)
  "Return T if MOVE is a move.
Does not check if the move is valid in the context of a game."
  (or (member move (list 'pass 'resign))
      (point-p move)))

(defun valid-move-p (move game)
  "Return T if MOVE is valid in GAME"
  (and (not (game-over-p game))
       (or (member move (list 'pass 'resign))
	   (and (empty-p move (game-board game))
		(let ((new-board
			(try-place-stone move
					 (game-turn game)
					 (game-board game))))
		  (not (or (move-violates-ko-rule new-board game)
			   (self-capture-p move new-board))))))))
