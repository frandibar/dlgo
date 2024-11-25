(defpackage #:dlgo.game
  (:use #:common-lisp)

  (:import-from #:dlgo.board
		#:make-board
		#:deep-copy-board)
  (:import-from #:dlgo.constant
		#:+komi+
		#:black
		#:white)

  (:export
   ;; defstruct group
   #:group-color
   #:group-stones
   #:group-liberties
   #:make-group

   ;; defstruct captures
   #:captures-black
   #:captures-white

   ;; defstruct player-info
   #:player-name
   #:player-level
   #:make-player-info

   ;; defstruct game-info
   #:game-komi
   #:game-handicap
   #:game-player-black
   #:game-player-white
   #:make-game-info

   ;; defstruct game
   #:game-turn
   #:game-board
   #:game-captures
   #:game-moves
   #:game-winner
   #:game-board
   #:game-info
   #:game-snapshots
   #:game-winner
   #:make-game
   #:copy-game

   ;; defstruct snapshot
   #:snapshot-turn
   #:snapshot-board
   #:snapshot-captures
   #:snapshot-moves
   #:make-snapshot
   ))

(in-package #:dlgo.game)

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

(defstruct captures
  (black 0)
  (white 0))

(defstruct (player-info (:conc-name player-))
  name
  level)

(defstruct (game-info (:conc-name game-))
  (komi +komi+)
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
