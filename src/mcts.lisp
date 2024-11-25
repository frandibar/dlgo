;; Monte Carlo Tree Search implementation

(defpackage #:dlgo.mcts
  (:use #:common-lisp)

  (:import-from #:alexandria
		#:define-constant
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)

  (:import-from #:dlgo
		#:game-board
		#:game-over-p
		#:game-winner
		#:pass-move
		#:place-stone-move
		#:resign-move
		#:valid-move-p)
  (:import-from #:dlgo.board
		#:board-size)
  (:import-from #:dlgo.constant
		#:black
		#:white
		#:pass
		#:resign)
  (:import-from #:dlgo.game
		#:game-turn)
  (:import-from #:dlgo.point
		#:index-to-point
		#:point-to-coords)
  (:import-from #:dlgo.score
		#:game-result)
  (:import-from #:dlgo.util
		#:remove-at-index)

  (:export #:select-move))

(in-package #:dlgo.mcts)

;; This number should be at least 10000 but we keep it low so it
;; doesn't take so long for each move, at the expense of being weaker.
(define-constant +num-rounds+ 1000)

;; UCT (upper confidence for trees) formula strikes a balance between conflicting goals in choosing nodes.
;; uct = w + c √((log N)/n)
;; w: winning percentage
;; N: total number of rollouts
;; n: rollouts started under node under consideration

;; The TEMPERATURE (c) represents the balance between depth search
;; (exploitation) and breadth search (exploration). A large value
;; focusses on the least-explored nodes. A small one on better
;; evaluating the most promising node.
(define-constant +temperature+ 1.5)

;; Below this score the bot resigns.
(define-constant +resign-score+ 0.3)

(defun legal-moves (game)
  "Return a list of valid moves for next player in GAME."
  (let ((size (board-size (game-board game))))
    (->>
      (alexandria:iota (* size size))
      (mapcar (rcurry #'index-to-point size))
      (remove-if-not (rcurry #'valid-move-p game)))))

(defstruct win-counts
  (black 0)
  (white 0))

(defstruct stats
  (win-counts (make-win-counts))
  (rollouts 0))

(defstruct (node (:constructor make-node (game parent move)))
  (game game)
  (parent parent)
  (move move)
  (children '())
  (stats (make-stats))
  (unvisited-moves (legal-moves game)))

(defun add-random-child! (node)
  (let* ((index (random (length (node-unvisited-moves node))))
	 (new-move (nth index (node-unvisited-moves node)))
	 (new-game (place-stone-move new-move
				     (node-game node)))
	 (child-node (make-node new-game node new-move)))
    (push child-node (node-children node))
    (setf (node-unvisited-moves node)
	  (remove-at-index index
			   (node-unvisited-moves node)))
    child-node))

(defun record-win (winner stats)
  "Return a copy of STATS with an increment in wins for WINNER."
  (let ((new-stats (copy-stats stats))
	(win-counts (stats-win-counts stats)))
    (incf (stats-rollouts new-stats))
    (incf (slot-value win-counts winner))
    new-stats))

(defun can-add-child-p (node)
  "Return T if NODE has remaining moves left."
  (> (length (node-unvisited-moves node))
     0))

(defun is-terminal-p (node)
  "Return T if NODE has no more continuation."
  (game-over-p (node-game node)))

(defun winning-fraction (color stats)
  "Return the wins/rollouts ration from STATS for COLOR."
  (/ (slot-value (stats-win-counts stats) color)
     (stats-rollouts stats)))

(defun apply-move (move game)
  (case move
    (pass (pass-move game))
    (resign (resign-move game))
    (t (place-stone-move move game))))

(defun simulate-random-game (game)
  "Simulate a game starting from GAME using naive random bots against
each other, and return the winner."
  (loop
    (if (game-over-p game)
	(return (cdr (game-result game)))
	(setf game
	      (apply-move (dlgo.agent:select-move game) game)))))

(defun pick-leaf-node (node)
  (cond
    ((is-terminal-p node) node)
    ((can-add-child-p node) (add-random-child! node))
    (t (pick-leaf-node (select-child node)))))

(defun select-move (game)
  "Return the best next move in GAME according to the MCTS algorithm."
  (let ((root (make-node game nil nil)))
    (dotimes (i +num-rounds+)
      (let* ((node (pick-leaf-node root))
	     (winner (simulate-random-game (node-game node))))
	;; Propagate scores back up the tree.
	(loop
	  (if node
	      (progn (setf (node-stats node)
			   (record-win winner (node-stats node)))
		     (setq node (node-parent node)))
	      (return)))))
    (pick-best-move (node-children root)
		    (game-turn game))))

(defun pick-best-move (child-nodes color)
  "Return the move for COLOR with the best score in CHILD-NODES."
  (let ((move-stats
	  (mapcar #'(lambda (node)
		      (list (node-move node)
			    (winning-fraction color
					      (node-stats node))
			    (stats-rollouts (node-stats node))))
		  child-nodes)))

    ;; Print top scores.
    (format t "~&Top 20 UCT Scores for ~a after ~d rounds:~%Move Score Rollouts~%"
	    color
	    +num-rounds+)
    (let* ((sorted-moves
	     (subseq (sort move-stats
			   #'(lambda (x y) (> (second x) (second y))))
		     0
		     (min 20 (length move-stats))))
	   (candidate (first sorted-moves)))
      (dolist (m-c-r sorted-moves)
	(format t "~a ~3$ ~d~%"
		(point-to-coords (first m-c-r))
		(second m-c-r)
		(third m-c-r)))

      ;; Check for resignation.
      (if (< (second candidate)
	     +resign-score+)
	  'resign
	  (first candidate)))))

(defun select-child (node)
  "Return the best child from NODE to continue with the rollouts."
  (let* ((total-rollouts (->>
			   (node-children node)
			   (mapcar #'node-stats)
			   (mapcar #'stats-rollouts)
			   (reduce #'+)))
	 (log-rollouts (log total-rollouts))
	 (color (game-turn (node-game node))))
    (flet ((uct-score (child)
	     (let ((win-percent
		     (winning-fraction color
				       (node-stats child)))
		   (exploration-factor
		     (sqrt (/ log-rollouts
			      (stats-rollouts (node-stats child))))))
	       (+ win-percent
		  (* +temperature+
		     exploration-factor)))))
      (->> (node-children node)
	(mapcar #'(lambda (child) (cons child
				   (uct-score child))))
	(reduce #'(lambda (x y) (if (> (cdr x) (cdr y)) x y)))
	car))))
