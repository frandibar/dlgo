(in-package #:dlgo/tests)


(test test-point-to-coords ()
  (is (string-equal "A1" (dlgo.tui::point-to-coords A1)))
  (is (string-equal "T1" (dlgo.tui::point-to-coords T1)))
  (is (string-equal "A19" (dlgo.tui::point-to-coords A19)))
  (is (string-equal "T19" (dlgo.tui::point-to-coords T19))))


(test test-star-point-p ()
  (is (dlgo.tui::star-point-p C3 +small-board+))
  (is (null (dlgo.tui::star-point-p A1 +small-board+))))


(test test-draw-stone ()
  (let ((board (dlgo::make-board +small-board+))
	(star-point C3)
	(empty-point A1)
	(black-stone B2)
	(white-stone C3))

    (is (string-equal (dlgo.tui::draw-stone star-point board)
		      dlgo.tui::+star-point+))

    (is (string-equal (dlgo.tui::draw-stone empty-point board)
		      dlgo.tui::+empty-point+))

    ;; Place black stone on board
    (let ((new-board
	    (dlgo::try-place-stone black-stone 'black board)))
      (is (string-equal (dlgo.tui::draw-stone black-stone new-board)
			dlgo.tui::+black-stone+)))

    ;; Place white stone on board
    (let ((new-board
	    (dlgo::try-place-stone white-stone 'white board)))
      (is (string-equal (dlgo.tui::draw-stone white-stone new-board)
			dlgo.tui::+white-stone+)))))

(defun setup-1 ()
  (let ((game
	  (->> (make-game +small-board+)
	    (place-stone-move (make-point :col 1 :row 1))
	    (place-stone-move (make-point :col 2 :row 1))
	    (place-stone-move (make-point :col 2 :row 2))
	    ;; Capture black stone
	    (place-stone-move (make-point :col 1 :row 2))
	    (place-stone-move (make-point :col 3 :row 1)))))
    (format t (draw-game game))
    game))

(defun setup-problem ()
  (let ((game (do-moves (make-game +small-board+)
		A1 A2
		B1 C1
		B2 A3)))
    (format t (draw-game game))
    game))

(defun setup-capture ()
  (let ((game (do-moves (make-game +small-board+)
		B1 'pass
		B2 'undo
		A2 C1
		'pass B2)))
    (format t (draw-game game))
    game))

(defun setup-problem-1 ()
  (let ((game (do-moves (make-game +small-board+)
		C1 B1
		B2 A2
		A3 'pass
		A1 D1
		A2 C2
		'pass B1
		'pass B3
		C1 A4)))
    (is (= 1 (captures-white (game-captures game))))
    (format t (draw-game game))))

(defun setup-problem-2 ()
  (let ((game (do-moves (make-game +small-board+)
		A1 B1
		B2 C2
		A2 B3
		C1)))
    (is (= 0 (captures-white (game-captures game))))
    (format t (draw-game game))))
