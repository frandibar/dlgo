;; Go Text Protocol implementation as described here:
;; https://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html#SECTION00030000000000000000

;; This package runs our bot as black against GNU Go as white.

(defpackage #:dlgo.gtp
  (:use #:common-lisp)
  (:import-from #:alexandria
		#:switch
		#:define-constant)
  (:import-from #:dlgo
		#:pass-move
		#:resign-move
		#:game-over-p
		#:make-game
		#:place-stone-move)
  (:import-from #:dlgo.sgf
		#:make-sgf)
  (:import-from #:dlgo.agent
		#:select-move)
  (:import-from #:dlgo.point
		#:coords-to-point
		#:point-to-coords)
  (:import-from #:dlgo.constant
		#:+small-board+
		#:+komi+
		#:pass
		#:resign)
  (:export #:start-game))

(in-package #:dlgo.gtp)

;; GNU Go commands

(define-constant +gnugo-board-size+ "boardsize" :test #'equal)
(define-constant +gnugo-komi+ "komi" :test #'equal)
(define-constant +gnugo-clear-board+ "clear_board" :test #'equal)
(define-constant +gnugo-show-board+ "showboard" :test #'equal)
(define-constant +gnugo-quit+ "quit" :test #'equal)
(define-constant +gnugo-play+ "play" :test #'equal)
(define-constant +gnugo-black+ "black" :test #'equal)
(define-constant +gnugo-white+ "white" :test #'equal)
(define-constant +gnugo-genmove+ "genmove" :test #'equal)
(define-constant +gnugo-final-score+ "final_score" :test #'equal)

(defun start-gnugo ()
  "Start the GNU Go process and return the process object."
  (let ((cmd (format nil "gnugo --mode gtp")))
    (uiop:launch-program cmd
			 :output :stream
			 :input :stream)))

(defun send-command (process command)
  "Send  COMMAND to GNU Go and return the response."
  (let ((input-stream (uiop:process-info-input process))
	(output-stream (uiop:process-info-output process)))

    ;; Send command.
    (write-line command input-stream)
    (force-output input-stream)

    ;; Parse response
    (subseq
     (str:unlines
      (loop for response = (read-line output-stream)
	    until (str:emptyp response)
	    collect response))
     ;; skip =
     2)))

(defun set-board-size (process size)
  (send-command process
		(format nil "~a ~a" +gnugo-board-size+ size)))

(defun set-komi (process komi)
  (send-command process
		(format nil "~a ~a" +gnugo-komi+ komi)))

(defun move-black (process move)
  (send-command process (format nil "~a ~a ~a" +gnugo-play+ +gnugo-black+ move)))

(defun move-white (process)
  (send-command process (format nil "~a ~a" +gnugo-genmove+ +gnugo-white+)))

(defun apply-move (move game)
  (case move
    (pass (pass-move game))
    (resign (resign-move game))
    (t (place-stone-move move game))))

(defun start-game ()
  "Start a game of Go. Bot vs gnugo."
  (let* ((outfile "random-bot-vs-gnugo.sgf")
	 (size +small-board+)
	 (gnugo (start-gnugo))
	 (game (make-game size)))
    (format t "~&Started ~ax~a Go game, bot (black) vs gnugo (white)..." size size)
    (unwind-protect
	 (progn
	   (set-board-size gnugo size)
	   (set-komi gnugo +komi+)
	   (send-command gnugo +gnugo-clear-board+)
	   (loop
	     (format t (send-command gnugo +gnugo-show-board+))
	     (force-output)
	     (if (game-over-p game)
		 (return)
		 (let ((move (select-move game)))
		   (move-black gnugo (move-as-str move))
		   (setf game
			 (apply-move move game))
		   (unless (game-over-p game)
		     (let ((opponent-move
			     (gnugo-move-to-move (move-white gnugo))))
		       (setf game
			     (apply-move opponent-move game)))))))

	   (format t "~&Final score: ~a~%"
		   (send-command gnugo +gnugo-final-score+))
	   ;; Save sgf file.
	   ;; gnugo only saves a snapshot to sgf.
	   (with-open-file (*standard-output* outfile
					      :direction :output
					      :if-exists :supersede)
	     (format t "~a" (make-sgf game)))
	   (format t "~&Saved output to ~a." outfile)
	   (send-command gnugo +gnugo-quit+))
      ;; Cleanup
      (uiop:terminate-process gnugo))))

(defun move-as-str (move)
  (case move
    (pass "pass")
    (resign "resign")
    (t (point-to-coords move))))

(defun gnugo-move-to-move (move)
  (switch ((string-downcase move) :test #'equal)
    ("pass" 'pass)
    ("resign" 'resign)
    (t (coords-to-point move))))
