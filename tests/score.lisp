(in-package #:dlgo/tests)

(test test-score ()
  ;; Using area counting
  ;; Black's score = 37
  ;; White's score = 41
  ;; http://home.snafu.de/jasiek/asintro.pdf

  ;; 9 . ○ . ○ ○ ○ ● ● ○
  ;; 8 ○ ○ ○ ○ ● ○ ● . ○
  ;; 7 ● ● ● ● ● ○ ● . ○
  ;; 6 . ● . ● ○ ○ ● ○ ○
  ;; 5 ● ● ● ○ . ○ ○ ● ●
  ;; 4 ● ○ ○ ○ ○ ○ ● ● .
  ;; 3 ○ ○ ● ● ● ● ● . .
  ;; 2 ● ● ○ ○ ○ ○ ● ● ●
  ;; 1 . ● . ○ . ○ ● . .
  ;;   A B C D E F G H J
  (let* ((game (do-moves (make-game +small-board+)
		 D1 B1
		 F1 G1
		 C2 A2
		 D2 B2
		 E2 G2
		 F2 H2
		 A3 J2
		 B3 C3
		 B4 D3
		 C4 E3
		 D4 F3
		 E4 G3
		 F4 A4
		 D5 G4
		 F5 H4
		 G5 A5
		 E6 B5
		 F6 C5
		 H6 H5
		 J6 J5
		 F7 B6
		 J7 D6
		 A8 G6
		 B8 A7
		 C8 B7
		 D8 C7
		 F8 D7
		 J8 E7
		 B9 G7
		 D9 E8
		 E9 G8
		 F9 G9
		 J9 H9))
	 (score (dlgo.score:score-game game)))
    (is (= (dlgo.score::score-black-stones score) 33))
    (is (= (dlgo.score::score-black-territory score) 4))
    (is (= (dlgo.score::score-white-stones score) 33))
    (is (= (dlgo.score::score-white-territory score) 8))
    (is (= (dlgo.score::score-dame score) 3))
    (is (= 7.5 (dlgo:game-komi (game-info game))))
    (is (string= (dlgo.score:game-result game) "W+11.5"))))
