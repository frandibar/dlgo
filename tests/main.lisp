(in-package #:dlgo/tests)

;; NOTE: To run this test file, execute `(asdf:test-system :dlgo)' in
;; your Lisp.
(defun run-tests ()
  (1am:run))

(defmacro define-point (name value)
  `(define-constant ,name ,value :test #'point-equal-p))

;; Define some constants for points on the board.
(define-point A1 (make-point :col 1 :row 1))
(define-point A2 (make-point :col 1 :row 2))
(define-point A3 (make-point :col 1 :row 3))
(define-point A4 (make-point :col 1 :row 4))
(define-point A5 (make-point :col 1 :row 5))
(define-point A6 (make-point :col 1 :row 6))
(define-point A7 (make-point :col 1 :row 7))
(define-point A8 (make-point :col 1 :row 8))
(define-point A9 (make-point :col 1 :row 9))
(define-point A19 (make-point :col 1 :row 19))
(define-point B1 (make-point :col 2 :row 1))
(define-point B2 (make-point :col 2 :row 2))
(define-point B3 (make-point :col 2 :row 3))
(define-point B4 (make-point :col 2 :row 4))
(define-point B5 (make-point :col 2 :row 5))
(define-point B6 (make-point :col 2 :row 6))
(define-point B7 (make-point :col 2 :row 7))
(define-point B8 (make-point :col 2 :row 8))
(define-point B9 (make-point :col 2 :row 9))
(define-point C1 (make-point :col 3 :row 1))
(define-point C2 (make-point :col 3 :row 2))
(define-point C3 (make-point :col 3 :row 3))
(define-point C4 (make-point :col 3 :row 4))
(define-point C5 (make-point :col 3 :row 5))
(define-point C6 (make-point :col 3 :row 6))
(define-point C7 (make-point :col 3 :row 7))
(define-point C8 (make-point :col 3 :row 8))
(define-point C9 (make-point :col 3 :row 9))
(define-point D1 (make-point :col 4 :row 1))
(define-point D2 (make-point :col 4 :row 2))
(define-point D3 (make-point :col 4 :row 3))
(define-point D4 (make-point :col 4 :row 4))
(define-point D5 (make-point :col 4 :row 5))
(define-point D6 (make-point :col 4 :row 6))
(define-point D7 (make-point :col 4 :row 7))
(define-point D8 (make-point :col 4 :row 8))
(define-point D9 (make-point :col 4 :row 9))
(define-point E1 (make-point :col 5 :row 1))
(define-point E2 (make-point :col 5 :row 2))
(define-point E3 (make-point :col 5 :row 3))
(define-point E4 (make-point :col 5 :row 4))
(define-point E5 (make-point :col 5 :row 5))
(define-point E6 (make-point :col 5 :row 6))
(define-point E7 (make-point :col 5 :row 7))
(define-point E8 (make-point :col 5 :row 8))
(define-point E9 (make-point :col 5 :row 9))
(define-point F1 (make-point :col 6 :row 1))
(define-point F2 (make-point :col 6 :row 2))
(define-point F3 (make-point :col 6 :row 3))
(define-point F4 (make-point :col 6 :row 4))
(define-point F5 (make-point :col 6 :row 5))
(define-point F6 (make-point :col 6 :row 6))
(define-point F7 (make-point :col 6 :row 7))
(define-point F8 (make-point :col 6 :row 8))
(define-point F9 (make-point :col 6 :row 9))
(define-point G1 (make-point :col 7 :row 1))
(define-point G2 (make-point :col 7 :row 2))
(define-point G3 (make-point :col 7 :row 3))
(define-point G4 (make-point :col 7 :row 4))
(define-point G5 (make-point :col 7 :row 5))
(define-point G6 (make-point :col 7 :row 6))
(define-point G7 (make-point :col 7 :row 7))
(define-point G8 (make-point :col 7 :row 8))
(define-point G9 (make-point :col 7 :row 9))
(define-point H1 (make-point :col 8 :row 1))
(define-point H2 (make-point :col 8 :row 2))
(define-point H3 (make-point :col 8 :row 3))
(define-point H4 (make-point :col 8 :row 4))
(define-point H5 (make-point :col 8 :row 5))
(define-point H6 (make-point :col 8 :row 6))
(define-point H7 (make-point :col 8 :row 7))
(define-point H8 (make-point :col 8 :row 8))
(define-point H9 (make-point :col 8 :row 9))
(define-point J1 (make-point :col 9 :row 1))
(define-point J2 (make-point :col 9 :row 2))
(define-point J3 (make-point :col 9 :row 3))
(define-point J4 (make-point :col 9 :row 4))
(define-point J5 (make-point :col 9 :row 5))
(define-point J6 (make-point :col 9 :row 6))
(define-point J7 (make-point :col 9 :row 7))
(define-point J8 (make-point :col 9 :row 8))
(define-point J9 (make-point :col 9 :row 9))
(define-point T1 (make-point :col 19 :row 1))
(define-point T19 (make-point :col 19 :row 19))

(defmacro do-moves (game &rest moves)
  `(->> ,game
     ,@(mapcar #'(lambda (move)
		   (if (eq (type-of move) 'cons)
		       (cond
			 ((eq (second move) 'pass) '(pass-move))
			 ((eq (second move) 'resign) '(resign-move))
			 ((eq (second move) 'undo) '(undo-last-move)))
		       (when (eq (type-of move) 'symbol)
			 `(place-stone-move ,move))))
	       moves)))


(test test-point-on-grid-p ()
  (let ((size +small-board+)
	(within-board 1)
	(out-of-board 0))
    (is (dlgo::point-on-grid-p
	 (make-point :col within-board :row within-board)
	 size))
    (is (dlgo::point-on-grid-p
	 (make-point :col size :row size)
	 size))
    (is (dlgo::point-on-grid-p
	 (make-point :col within-board :row size)
	 size))
    (is (dlgo::point-on-grid-p
	 (make-point :col size :row within-board)
	 size))
    (is (eq (dlgo::point-on-grid-p
	     (make-point :col out-of-board :row within-board)
	     size)
	    nil))
    (is (eq (dlgo::point-on-grid-p
	     (make-point :col within-board :row out-of-board)
	     size)
	    nil))))


(test test-remove-nonexistent-liberty ()
  (let ((group (dlgo::make-group 'black)))
    ;; Removing a liberty that doesn't exist does nothing.
    (is (null (dlgo::remove-liberty A2 group)))))


(test test-pass-move ()
  (let ((next-turn (->> (make-game +small-board+)
		     (pass-move)
		     (game-turn))))
    (is (eq 'white next-turn)))

  (let ((next-turn (->> (make-game +small-board+)
		     (pass-move)
		     (pass-move)
		     (game-turn))))
    (is (eq 'black next-turn))))


(test test-resign-move ()
  (let ((game (do-moves (make-game +small-board+)
		'resign)))

    (is (game-over-p game))
    (is (eq 'white
	    (game-winner game)))))


(test test-game-over ()
  (let ((game (do-moves (make-game +small-board+)
		'resign)))
    ;; Game is over.
    (signals simple-error (place-stone-move A1 game))
    (signals simple-error (pass-move game))
    (signals simple-error (resign-move game)))

  (let ((game (do-moves (make-game +small-board+)
		'pass 'pass)))
    ;; Game is over.
    (signals simple-error (place-stone-move A1 game))
    (signals simple-error (pass-move game))
    (signals simple-error (resign-move game))))


(test test-get-at ()
  (let* ((game (do-moves (make-game +small-board+)
		 A1)))

    (is (get-at A1 (game-board game)))))


(test test-add-liberties ()
  (let* ((group (->> (dlgo::make-group 'black)
		  (dlgo::set-group-liberties (list A1)))))
    ;; Adding a liberty that is already present does nothing.
    (is (= 1 (length (dlgo::add-liberty A1 group))))
    ;; Add a new liberty and verify it get's added.
    (is (member B2 (dlgo::add-liberty B2 group)))))


(test test-group-equal-p ()
  ;; Test empty group is equal to itself.
  (let ((group (dlgo::make-group 'black)))
    (is (dlgo::group-equal-p group group)))

  ;; Test black group is not equal to white group.
  (let ((black-group (dlgo::make-group 'black))
	(white-group (dlgo::make-group 'white)))
    (is (not (dlgo::group-equal-p black-group white-group))))

  ;; Test groups with same stones (in different order) and liberties
  ;; are equal.
  (let* ((group-1 (->> (dlgo::make-group 'black)
		    (dlgo::set-group-stones (list A2 C4))
		    (dlgo::set-group-liberties (list A2 C4))))
	 (group-2 (->> (dlgo::make-group 'black)
		    (dlgo::set-group-stones (list C4 A2))
		    (dlgo::set-group-liberties (list C4 A2)))))

    (is (dlgo::group-equal-p group-1 group-2))))


(test test-merge-groups ()
  ;; Cannot merge black and white groups.
  (let ((white-group (dlgo::make-group 'white))
	(black-group (dlgo::make-group 'black)))

    (signals simple-error (dlgo::merge-groups white-group black-group)))

  ;; Merge empty groups.
  (let ((group-1 (dlgo::make-group 'black))
	(group-2 (dlgo::make-group 'black)))
    (is (dlgo::group-equal-p (dlgo::merge-groups group-1 group-2)
			     group-1)))

  ;; In this figure, A2 forces merging A2 with A1 and with B2

  ;; 3 . . . .
  ;; 2(○)○ ● .
  ;; 1 ○ ● . .
  ;;   A B C D

  ;; Merge non empty groups.
  (let ((group-a1 (->> (dlgo::make-group 'black)
		    (dlgo::set-group-stones (list A1))
		    (dlgo::set-group-liberties nil)))
	(group-a2 (->> (dlgo::make-group 'black)
		    (dlgo::set-group-stones (list A2))))
	(group-b2 (->> (dlgo::make-group 'black)
		    (dlgo::set-group-stones (list B2)))))
    (is (= 2
	   (length (dlgo::group-stones
		    (dlgo::merge-groups group-a1 group-a2)))))
    (is (= 3
	   (length (dlgo::group-stones
		    (dlgo::merge-groups
		     (dlgo::merge-groups group-a1 group-a2)
		     group-b2)))))))


(test test-place-stone-move ()
  (let ((game (make-game +small-board+)))

    ;; No stones placed yet.
    (is (empty-p A1 (game-board game)))
    (is (empty-p B3 (game-board game)))

    ;; Place a stone but game should be left untouched.
    (place-stone-move A1 game)
    (is (empty-p A1 (game-board game)))
    (is (empty-p B3 (game-board game)))

    ;; Now place another stone but set it.
    (setq game (place-stone-move B3 game))
    (is (get-at B3 (game-board game)))

    ;; Add stone where a stone already exists.
    (signals user-error (place-stone-move B3 game))))


(test test-group-captured-p ()
  (let* ((group-without-liberties (dlgo::make-group 'black))
	 (group-with-liberties
	   (->> (dlgo::make-group 'black)
	     (dlgo::set-group-liberties (list A3 B1)))))

    (is (dlgo::group-captured-p group-without-liberties))
    (is (not (dlgo::group-captured-p group-with-liberties)))))


(test test-opponent ()
  (is (eq 'black (opponent 'white)))
  (is (eq 'white (opponent 'black)))
  (signals sb-kernel::case-failure
    (opponent 'invalid-player)))


(test test-point-equal-p ()
  (let ((a2-bis (dlgo.point::copy-point A2)))
    (is (null (point-equal-p A2 C3)))
    (is (point-equal-p A2 a2-bis))))


(test test-undo-last-move ()
  (let ((game (make-game +small-board+)))

    ;; No moves to undo
    (signals user-error (undo-last-move game))
    (is (equal (game-turn game) 'black))
    (setq game (place-stone-move A2 game))
    (is (equal (game-turn game) 'white))
    (setq game (undo-last-move game))
    (is (equal (game-turn game) 'black))

    ;; This should go well as the stone was removed.
    (setq game (place-stone-move A2 game))
    (is (equal (game-turn game) 'white))))


(test test-point-index ()
  (let ((board-size 9))
    (is (= (dlgo::point-index A1
			      board-size)
	   0))
    (is (= (dlgo::point-index (make-point :col board-size
					  :row board-size)
			      board-size)
	   (1- (* board-size board-size))))
    (is (= (dlgo::point-index (make-point :col 1 :row board-size)
			      board-size)
	   (1- board-size)))))


(test test-score-capture ()
  (let ((captures (dlgo::make-captures))
	(black-captures 1)
	(white-captures 2))
    (is (= black-captures
	   (dlgo::captures-black
	    (dlgo::score-capture 'black
				 black-captures
				 captures))))
    (is (= white-captures
	   (dlgo::captures-white
	    (dlgo::score-capture 'white
				 white-captures
				 captures))))))


(test test-capture-0 ()
  ;; 2(○).
  ;; 1 ● ○
  ;;   A B
  (let ((game (do-moves (make-game +small-board+)
		B1 A1
		A2)))
    (is (= 1
	   (dlgo::captures-black (dlgo::game-captures game))))))


(test test-capture-1 ()
  ;; 4 . . . .
  ;; 3(○)○ + .
  ;; 2 ● ● ○ .
  ;; 1 ● ○ . .
  ;;   A B C D
  (let ((game (do-moves (make-game +small-board+)
		B1 A1
		C2 B2
		B3 A2
		A3)))
    (is (= 3
	   (captures-black (game-captures game))))))


(test test-capture-2 ()
  ;; 4 . ○ ● . .
  ;; 3 ○ ● + ● .
  ;; 2 ○ ● . ● .
  ;; 1 ○ ●(●)○ .
  ;;   A B C D E
  (let ((game (do-moves (make-game +small-board+)
		A1 B1
		A2 B2
		A3 B3
		B4 C4
		C3 D3
		C2 D2
		D1 C1)))
    (is (= 2
	   (captures-white (game-captures game))))))


(test test-self-capture ()
  ;; In this figure, (○) cannot be placed.

  ;; 2 ○ .
  ;; 1(●)○
  ;;   A B
  (let ((game (do-moves (make-game +small-board+)
		B1 'pass
		A2)))

    (signals user-error (place-stone-move A1 game))))


(test test-capture-without-liberties ()
  ;; In this figure, (●) captures two ○ groups.

  ;; 3 ○ . . .
  ;; 2 ● ○ . .
  ;; 1(○)● ○ .
  ;;   A B C D
  (let ((game (do-moves (make-game +small-board+)
		C1 B1
		B2 A2
		A3 'pass
		A1)))

    (is (= 2
	   (captures-black (game-captures game))))))


(test test-append-to-slot ()
  (let* ((game (make-game +small-board+)))

    (is (= 1
	   (length (dlgo::append-to-slot A1
					 #'game-moves game))))

    (setf (game-moves game)
	  (dlgo::append-to-slot A1 #'game-moves game))

    (is (= 2
	   (length (dlgo::append-to-slot A2
					 #'game-moves game))))

    (setf (game-moves game)
	  (dlgo::append-to-slot A2 #'game-moves game))

    (is (= 3
	   (length (dlgo::append-to-slot B1
					 #'game-moves game))))))


(test test-deep-copy-board ()
  (let ((board (dlgo::make-board +small-board+)))
    (setf (aref (dlgo::board-grid board) 0) A1)
    (let ((shallow-board (dlgo.board::copy-board board))
	  (deep-board (dlgo.board::deep-copy-board board)))
      ;; We change board...
      (setf (aref (dlgo::board-grid board) 1) A2)

      ;; ...and verify the change also affected shallow-board.
      (is (aref (dlgo::board-grid board) 1))
      (is (aref (dlgo::board-grid shallow-board) 1))
      ;; ...but not deep-board.
      (is (null (aref (dlgo::board-grid deep-board) 1))))))


(test test-zobrist-hashes ()
  (let ((game (do-moves (make-game +small-board+)
		C1 B1
		B2 A2
		A1))			; removes W B1
	(black-hashes (mapcar (rcurry #'zobrist-hash 'black)
			      (list C1 B2 A1)))
	(white-hashes (mapcar (rcurry #'zobrist-hash 'white)
			      (list B1 A2
				    ;; we add B1 because it was
				    ;; captured.
				    B1))))

    (is (= (dlgo::board-hash (game-board game))
	   (reduce #'logxor (append black-hashes
				    white-hashes))))))

(test test-ko-rule-1 ()
  (let ((game (do-moves (make-game +small-board+)
		C1 B1
		B2 A2
		A1)))
    (signals user-error (place-stone-move B1 game))
    (do-moves game
      'pass
      D1 B1)))


(test test-set-group-liberties ()
  (let ((group (->> (dlgo::make-group 'black)
		 (dlgo::set-group-liberties (list A1)))))
    (is (= 1
	   (length (dlgo::group-liberties group))))))


(test test-remove-group ()
  ;; In this scenario, removing (●) should leave black group with 4
  ;; liberties.

  ;; 3 . . .
  ;; 2 ○ ○ .
  ;; 1 ○ ●(○)
  ;;   A B C
  (let* ((game (do-moves (make-game +small-board+)
		 A1 B1
		 A2 'pass
		 B2 'pass
		 C1))
	 (grid (dlgo::board-grid (game-board game))))
    (is (= 4
	   (length (dlgo::group-liberties (aref grid 0)))))
    (is (= 4
	   (length (dlgo::group-liberties (aref grid 1)))))))


(test test-snapback ()
  ;; 3 . ○ ○ ○ . .
  ;; 2 ● ○ . . ○ .
  ;; 1 ● ●(○). ○ .
  ;;   A B C D E F
  (let* ((game (do-moves (make-game +small-board+)
		 B2 A1
		 B3 A2
		 C3 B1
		 D3 C2
		 E2 D2
		 E1 'pass
		 C1 D1
		 C1)))
    (is (= 1
	   (captures-white (game-captures game))))
    (is (= 3
	   (captures-black (game-captures game))))))

(test test-valid-move-p ()
  (let* ((game (do-moves (make-game +small-board+)
		 C1 B1
		 B2 A2
		 A1)))
    ;; 3 . . . .
    ;; 2 ● ○ . .
    ;; 1(○)● ○ .
    ;;   A B C D

    (is (valid-move-p 'pass game))
    (is (valid-move-p 'resign game))
    ;; Not empty
    (is (not (valid-move-p C1 game)))
    ;; Ko violation
    (is (not (valid-move-p B1 game))))

  (let* ((game (do-moves (make-game +small-board+)
		 B1 'pass
		 A2)))
    ;; 3 . . .
    ;; 2(○). .
    ;; 1 . ○ .
    ;;   A B C

    ;; Self capture.
    (is (not (valid-move-p A1 game)))))
