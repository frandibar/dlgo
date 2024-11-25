(in-package #:dlgo/tests)

(test test-eye-p ()
  ;; 4 . ○ ○ . .
  ;; 3 . ○ . ○ .
  ;; 2 ○ ○ ○ ○ .
  ;; 1 . ○ . ○ .
  ;;   A B C D E
  (let* ((game (do-moves (make-game +small-board+)
		 A2 'pass
		 B1 'pass
		 B2 'pass
		 B3 'pass
		 B4 'pass
		 C2 'pass
		 C4 'pass
		 D1 'pass
		 D2 'pass
		 D3 'pass))
	 (board (game-board game)))

    (is (dlgo.agent::eye-p A1 'black board))
    (is (dlgo.agent::eye-p C1 'black board))
    (is (dlgo.agent::eye-p C3 'black board))
    (is (not (dlgo.agent::eye-p A1 'white board)))
    (is (not (dlgo.agent::eye-p C1 'white board)))
    (is (not (dlgo.agent::eye-p C3 'white board)))))
