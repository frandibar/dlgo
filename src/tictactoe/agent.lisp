(defpackage #:dlgo.tictactoe.agent
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:switch
		#:curry
		#:rcurry)
  (:import-from #:binding-arrows
		#:->>)
  (:import-from #:dlgo.tictactoe
		#:game-over-p
		#:game-winner
		#:game-turn
		#:opponent
		#:place-stone-move
		#:valid-move-p)
  (:import-from #:dlgo.tictactoe.constant
		#:win
		#:lose
		#:tie
		#:+board-size+)
  (:import-from #:dlgo.tictactoe.point
		#:point-col
		#:point-row
		#:index-to-point
		#:make-point)
  (:export #:select-move))

(in-package #:dlgo.tictactoe.agent)

(defun legal-moves (game)
  (->>
    (alexandria:iota (* +board-size+ +board-size+))
    (mapcar #'index-to-point)
    (remove-if-not (rcurry #'valid-move-p game))))

(defun select-random-move (game)
  "Return a random valid move."
  (let ((candidates (legal-moves game)))
    (when candidates
      (nth (random (length candidates))
	   candidates))))

(defun better-result (result-a result-b)
  "Return the best result between RESULT-A and RESULT-B. Order is 'win 'tie 'lose."
  (let ((sorted-results (list 'win 'tie 'lose)))
    (->> (list result-a result-b)
      (mapcar #'(lambda (result) (position result sorted-results)))
      (apply #'min)
      (funcall (rcurry #'nth sorted-results)))))

(defun opposite-result (result)
  (case result
    (win 'lose)
    (lose 'win)
    (t 'tie)))

(defun next-best-result (point game)
  (let* ((new-game (place-stone-move point game))
	 (opponent-best-result (best-result new-game)))
    (opposite-result opponent-best-result)))

(defun best-result (game)
  (if (game-over-p game)
      (switch ((game-winner game))
	((game-turn game) 'win)
	((opponent (game-turn game)) 'lose)
	(t 'draw))

      (reduce #'better-result
	      (mapcar (rcurry #'next-best-result game)
		      (legal-moves game))
	      :initial-value 'lose)))

(defun filter-moves (type results)
  (->>
    results
    (remove-if-not #'(lambda (result)
		       (eq type (cdr result))))
    (mapcar #'first)))

(defun select-minimax-move (game)
  (let* ((results (mapcar #'(lambda (move)
			      (cons move (next-best-result move game)))
			  (legal-moves game)))
	 (winning-moves (filter-moves 'win results))
	 (tie-moves (filter-moves 'tie results))
	 (losing-moves (filter-moves 'lose results)))
    (cond
      (winning-moves (first winning-moves))
      (tie-moves (first tie-moves))
      (t (first losing-moves)))))

(defun select-move (game)
  ;; (select-random-move game)
  (select-minimax-move game))
