(defpackage #:dlgo.tictactoe.minimax
  (:use #:common-lisp)
  (:export #:find-winning-move
	   #:opponent-winning-move-p
	   #:eliminate-losing-moves))

(in-package #:dlgo.tictactoe.minimax)

(defun find-winning-move (game-state next-player)
  (dolist (candidate-move (legal-moves game-state next-player))
    (let ((next-state (apply-move candidate-move game-state)))
      (if (and (game-over next-state)
	       (eq next-player (winner next-state)))
	  (return candidate-move)))))

(defun opponent-winning-move-p (games-state next-player move)
  (find-winning-move (apply-move game-state move)
		     next-player))

(defun eliminate-losing-moves (game-state next-player opponent)
  (remove-if-not #'(lambda (move)
		     (opponent-winning-move-p game-state
					      opponent
					      move))
		 (legal-moves game-state next-player)))

(defun find-two-step-win (game-state next-player)
  (dolist (candidate-move (legal-moves game-state next-player))
    (let* ((next-state (apply-move candidate-move game-state))
	   (good-responses (eliminate-losing-moves
			    next-state
			    (opponent next-player))))
      (when (not good-reponses)
	(return candidate-move)))))
