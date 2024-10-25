(in-package #:dlgo/tests)

(test test-sgf-coordinate ()
  (let ((size +small-board+))
    (is (string-equal "ai" (dlgo.sgf::sgf-coordinate size A1)))
    (is (string-equal "aa" (dlgo.sgf::sgf-coordinate size A9)))
    (is (string-equal "ii" (dlgo.sgf::sgf-coordinate size J1)))
    (is (string-equal "ia" (dlgo.sgf::sgf-coordinate size J9))))

  (let ((size +big-board+))
    (is (string-equal "as" (dlgo.sgf::sgf-coordinate size A1)))
    (is (string-equal "aa" (dlgo.sgf::sgf-coordinate size A19)))
    (is (string-equal "ss" (dlgo.sgf::sgf-coordinate size T1)))
    (is (string-equal "sa" (dlgo.sgf::sgf-coordinate size T19)))))


(test test-move-annotation ()
  (let ((size +small-board+))
    (is (string-equal "[ai]"
		      (dlgo.sgf::move-annotation size A1)))
    (is (string-equal "[]"
		      (dlgo.sgf::move-annotation size 'pass)))
    (is (string-equal ""
		      (dlgo.sgf::move-annotation size 'resign)))
    (signals simple-error
      (dlgo.sgf::move-annotation size 'invalid-move))))


(test test-sgf ()
  ;; TODO unfinished
  (let ((game (do-moves (make-game +small-board+)
		A1 A9
		'pass 'resign)))
    (make-sgf game)))
