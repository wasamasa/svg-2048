;;; svg-2048.el --- A SVG version of the titular HTML5 game

;; Copyright (C) 2014-2015 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/svg-2048
;; Version: 0.0.1
;; Package-Requires: ((dash "2.4.0") (esxml "0.3.0") (emacs "24.3"))
;; Keywords: games

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A port of the titular HTML5 game utilizing Emacs' SVG capabilities.

;; See the README for more info:
;; https://github.com/wasamasa/svg-2048

;;; Code:

(require 'esxml)
(require 'dash)
(require 'cl-lib)

(defgroup svg-2048 nil
  "A port of the titular HTML5 game utilizing Emacs' SVG
capabilities."
  :group 'games
  :prefix "svg-2048-")

(defcustom svg-2048-board-size 4
  "Width of the board in tiles."
  :type 'integer
  :group 'svg-2048)

(defcustom svg-2048-winning-tile 2048
  "Number of the tile required to win the game."
  :type 'integer
  :group 'svg-2048)

(defcustom svg-2048-use-original-background nil
  "When t, use the original background for the entire buffer.
Otherwise the game will use a transparent background."
  :type 'boolean
  :group 'svg-2048)

(defcustom svg-2048-display-score t
  "When t, display the current score using
  `svg-2048-score-mode'."
  :type 'boolean
  :group 'svg-2048)

(defvar svg-2048-score 0
  "Holds the current game score.")
(defvar svg-2048-score-lighter " Score: 0"
  "Holds the current game score lighter.")
(defcustom svg-2048-score-lighter-format " Score: %i"
  "Score lighter format string.
Must contain a %i place holder for the current score."
  :type 'string
  :group 'svg-2048)

(defcustom svg-2048-font "sans-serif"
  "Font to use for tiles."
  :type 'string
  :group 'svg-2048)

(defcustom svg-2048-background-color "#faf8ef"
  "Color of the background.  Not used when
`svg-2048-use-original-background' is set to nil."
  :type 'string
  :group 'svg-2048)

(defcustom svg-2048-board-color "#bbada0"
  "Color of the game board."
  :type 'string
  :group 'svg-2048)

(defcustom svg-2048-tile-color "#eee4da"
  "Default color of the game tile."
  :type 'string
  :group 'svg-2048)

(defcustom svg-2048-padding 14
  "Padding between tiles in pixels."
  :type 'integer
  :group 'svg-2048)

(defcustom svg-2048-tile-size 107
  "Size of each tile in pixels."
  :type 'integer
  :group 'svg-2048)

(defcustom svg-2048-offset 8
  "Offset by which the game board is moved away from the corner
in pixels."
  :type 'integer
  :group 'svg-2048)

(defcustom svg-2048-roundness 6
  "Rounding factor for board edges.
Tiles use half of its value."
  :type 'integer
  :group 'svg-2048)

(defvar svg-2048-board nil
  "Board state.")
(defvar svg-2048-merged-tiles nil
  "Holds all tiles merged in current game move.")
(defvar svg-2048-merged-tile-values '()
  "Holds all tile values merged from current game move.")
(defvar svg-2048-game-over nil
  "When set to t, appropriate overlay is displayed.")
(defvar svg-2048-game-won nil
  "When set to t, appropriate overlay is displayed.")
(defvar svg-2048-game-won-already nil
  "Did the player win and decide to continue this game?")

(defun svg-2048-create-svg ()
  "Create a SVG representation of the current game state."
  (let* ((field-width (+ (* (1+ svg-2048-board-size) svg-2048-padding)
                         (* svg-2048-board-size svg-2048-tile-size)))
         (field-height (+ (* (1+ svg-2048-board-size) svg-2048-padding)
                          (* svg-2048-board-size svg-2048-tile-size)))
         (width (+ field-width (* 2 svg-2048-offset)))
         (height (+ field-width (* 2 svg-2048-offset))))
    (cl-flet ((n->s (n) (number-to-string n)))
      (sxml-to-xml
       `(svg
         (@ (xmlns "http://www.w3.org/2000/svg")
            (width ,(n->s width))
            (height ,(n->s height)))
         ,(when svg-2048-use-original-background
            `(g (rect
                 (@ (width ,(n->s width))
                    (height ,(n->s height))
                    (fill ,svg-2048-background-color)))))
         (g (rect
             (@ (width ,(n->s field-width))
                (height ,(n->s field-height))
                (rx ,(n->s svg-2048-roundness))
                (x ,(n->s svg-2048-offset))
                (y ,(n->s svg-2048-offset))
                (fill ,svg-2048-board-color)))
            ,@(cl-loop for ((i . j) . value) in svg-2048-board collect
                       (svg-2048-tile i j value))
            ,(cond (svg-2048-game-over
                     `(g (rect
                          (@ (width ,(n->s field-width))
                             (height ,(n->s field-height))
                             (rx ,(n->s svg-2048-roundness))
                             (x ,(n->s svg-2048-offset))
                             (y ,(n->s svg-2048-offset))
                             (fill ,svg-2048-tile-color)
                             (fill-opacity "0.73")))
                         (text
                          (@ (text-anchor "middle")
                             (font-family ,svg-2048-font)
                             (font-weight "bold")
                             (font-size "60")
                             (fill "#776e65")
                             (x ,(n->s (/ field-width 2)))
                             (y ,(n->s (+ (/ field-height 2) 30))))
                          "Game over!")))
                    (svg-2048-game-won
                     `(g (rect
                          (@ (width ,(n->s field-width))
                             (height ,(n->s field-height))
                             (rx ,(n->s svg-2048-roundness))
                             (x ,(n->s svg-2048-offset))
                             (y ,(n->s svg-2048-offset))
                             (fill "#edc22e")
                             (fill-opacity "0.5")))
                         (text
                          (@ (text-anchor "middle")
                             (font-family ,svg-2048-font)
                             (font-weight "bold")
                             (font-size "60")
                             (fill "#f9f6f2")
                             (x ,(n->s (/ field-width 2)))
                             (y ,(n->s (+ (/ field-height 2) 30))))
                          "You win!")))
                    (t nil))))))))

(defun svg-2048-tile (x y value)
  "Create a SVG representation of a tile.
X, Y and VALUE are taken in account."
  (cl-flet ((n->s (n) (number-to-string n)))
    (let* ((digits
            (if value (length (number-to-string value)) 0))
           (tile-roundness (/ svg-2048-roundness 2))
           (tile-color (cond
                        ((not value) "#ccc0b4")
                        ((= value 2) "#eee4da")
                        ((= value 4) "#ede0c8")
                        ((= value 8) "#f2b179")
                        ((= value 16) "#f59563")
                        ((= value 32) "#f67c5f")
                        ((= value 64) "#f65e3b")
                        ((= value 128) "#edcf72")
                        ((= value 256) "#edcc61")
                        ((= value 512) "#edc850")
                        ((= value 1024) "#edc53f")
                        ((= value 2048) "#edc22e")
                        (t "#3c3a32")))
           (tile-x (+ (* (1+ y) svg-2048-padding)
                      (* y svg-2048-tile-size)
                      svg-2048-offset))
           (tile-y (+ (* (1+ x) svg-2048-padding)
                      (* x svg-2048-tile-size)
                      svg-2048-offset))
           (text-size (cond
                       ((< digits 3) "50")
                       ((= digits 3) "40")
                       ((or (> value 2048)
                            (> digits 4)) "25")
                       ((= digits 4) "30")))
           (text-color (cond
                        ((or (not value) (< value 8))
                         "#776e65")
                        (t "#f9f6f2")))
           (text-dy (cond
                     ((< digits 2) "12")
                     ((= digits 2) "12")
                     ((= digits 3) "8")
                     ((or (> value 2048) (> digits 4)) "2")
                     ((= digits 4) "5")))
           (text-x (+ (* (1+ y) svg-2048-padding)
                      (/ (* (1- (* (1+ y) 2))
                            svg-2048-tile-size) 2)
                      svg-2048-offset))
           (text-y (+ (* (+ x 2) svg-2048-padding)
                      (/ (* (1- (* (1+ x) 2))
                            svg-2048-tile-size) 2)
                      svg-2048-offset)))
      `(g (rect
           (@ (width ,(n->s svg-2048-tile-size))
              (height ,(n->s svg-2048-tile-size))
              (rx ,(n->s tile-roundness))
              (fill ,tile-color)
              (x ,(n->s tile-x))
              (y ,(n->s tile-y))))
          (text
           (@ (text-anchor "middle")
              (font-family ,svg-2048-font)
              (font-weight "bold")
              (font-size ,text-size)
              (dy ,text-dy)
              (fill ,text-color)
              (x ,(n->s text-x))
              (y ,(n->s text-y)))
           ,(when (and value (numberp value))
              (n->s value)))))))

(defun svg-2048-get-tile-value (x y)
  "Returns the value of a tile at X and Y."
  (cdr (assoc (cons x y) svg-2048-board)))

(defun svg-2048-set-tile-value (x y value)
  "Sets the value of a tile at X and Y."
  (let* ((coord (cons x y))
         (old-tile (assoc coord svg-2048-board))
         (new-tile (cons coord value)))
    (if old-tile
        (setq svg-2048-board
              (-replace-at (-elem-index old-tile svg-2048-board)
                           new-tile svg-2048-board))
      (setq svg-2048-board (-snoc svg-2048-board new-tile)))))

(defun svg-2048-fill-board (rows)
  "Fill the board with the specified values.
This function resets the board first, then inserts values as
specified by ROWS.  ROWS must be a list of lists. For example"
  (setq svg-2048-board '())
  (cl-loop for i from 0 to (1- svg-2048-board-size) do
           (cl-loop for j from 0 to (1- svg-2048-board-size) do
                    (svg-2048-set-tile-value i j (nth j (nth i rows))))))

(defun svg-2048-initialize-board ()
  (svg-2048-fill-board '()))

(defun svg-2048-new-coord (x y direction)
  "Returns the new coordinate as a cons cell if a tile at X and Y
were to move to the direction DIRECTION.  DIRECTION is one of the
following symbols: 'up, 'down, 'left, 'right.  If the tile can't
move, the coordinate is returned unchanged."
  (let ((new-x (cond ((eq direction 'up) (max 0 (1- x)))
                     ((eq direction 'down)
                      (min (1- svg-2048-board-size) (1+ x)))
                     (t x)))
        (new-y (cond ((eq direction 'left) (max 0 (1- y)))
                     ((eq direction 'right)
                      (min (1- svg-2048-board-size) (1+ y)))
                     (t y))))
    (cons new-x new-y)))

(defun svg-2048-movable-p (x y direction)
  "Returns t if the tile at X and Y is movable to the direction
DIRECTION.  See `svg-2048-new-coord' for a list of valid
directions."
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-tile-value x y))
         (new-value (svg-2048-get-tile-value new-x new-y)))
    (when (and value (not (equal (cons x y) new-coord)) (not new-value)) t)))

(defun svg-2048-mergeable-p (x y direction)
  "Returns t if the tile at X and Y is mergeable to the direction
DIRECTION.  See `svg-2048-new-coord' for a list of valid
directions."
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-tile-value x y))
         (new-value (svg-2048-get-tile-value new-x new-y)))
    (when (and value (not (equal (cons x y) new-coord))
               new-value (= value new-value)) t)))

(defun svg-2048-move-tile (x y direction)
  "Move the tile at X and Y one step to the direction DIRECTION.
See `svg-2048-new-coord' for a list of valid directions."
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-tile-value x y)))
    (when (and value new-coord)
      (svg-2048-set-tile-value new-x new-y value)
      (svg-2048-set-tile-value x y nil))))

(defun svg-2048-merge-tile (x y direction)
  "Merge the tile at X and Y into the next tile at DIRECTION.
See `svg-2048-new-coord' for a list of valid directions."
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-tile-value x y)))
    (when (and value new-coord)
      (svg-2048-set-tile-value new-x new-y (* 2 value))
      (svg-2048-set-tile-value x y nil)
      (setq svg-2048-score (+ svg-2048-score (* 2 value)))
      (svg-2048-score-update))))

(defun svg-2048-move-tile-to-end (x y direction)
  "Moves the tile at X and Y as far as possible towards
DIRECTION.  See `svg-2048-new-coord' for a list of valid
directions.  Once the tile isn't movable, a check is done whether
it can be merged towards DIRECTION.  If it is, the tile is merged
once and the move is finished."
  (let ((current-x x)
        (current-y y)
        (new-coord (svg-2048-new-coord x y direction)))
    (while (svg-2048-movable-p current-x current-y direction)
      (setq new-coord (svg-2048-new-coord current-x current-y direction))
      (svg-2048-move-tile current-x current-y direction)
      (setq current-x (car new-coord))
      (setq current-y (cdr new-coord)))
    (when (svg-2048-mergeable-p current-x current-y direction)
      (setq new-coord (svg-2048-new-coord current-x current-y direction))
      (unless (assoc new-coord svg-2048-merged-tiles)
        (svg-2048-merge-tile current-x current-y direction)
        (setq current-x (car new-coord))
        (setq current-y (cdr new-coord))
        (setq svg-2048-merged-tiles
              (cons (cons (cons current-x current-y)
                          (svg-2048-get-tile-value current-x current-y))
                    svg-2048-merged-tiles))))))

(defun svg-2048-move-tiles (direction)
  "Move all tiles towards DIRECTION.  See `svg-2048-new-coord'
for a list of valid directions.  The function progresses from the
row of tiles that are the closest to the side of DIRECTION to the
row of tiles that are the farthest from the side of DIRECTION.
For example if DIRECTION were 'left, it would start with the
leftmost and finish with the rightmost tiles."
  (let ((ij-list
         (cond ((eq direction 'up)
                (cl-loop for i from 1 to (1- svg-2048-board-size) nconc
                         (cl-loop for j from 0 to (1- svg-2048-board-size)
                                  collect (cons i j))))
               ((eq direction 'down)
                (cl-loop for i from (- svg-2048-board-size 2) downto 0 nconc
                         (cl-loop for j from 0 to (1- svg-2048-board-size)
                                  collect (cons i j))))
               ((eq direction 'left)
                (cl-loop for j from 1 to (1- svg-2048-board-size) nconc
                         (cl-loop for i from 0 to (1- svg-2048-board-size)
                                  collect (cons i j))))
               ((eq direction 'right)
                (cl-loop for j from (- svg-2048-board-size 2) downto 0 nconc
                         (cl-loop for i from 0 to (1- svg-2048-board-size)
                                  collect (cons i j)))))))
    (cl-loop for (i . j) in ij-list when (svg-2048-get-tile-value i j) do
             (svg-2048-move-tile-to-end i j direction))))

(defun svg-2048-empty-tiles ()
  "Returns a list of empty tiles."
  (cl-loop for ((i . j) . value) in svg-2048-board
           unless value collect
           (cons i j)))

(defun svg-2048-move-possible-p (directions)
  "Returns t if it's possible for the player to do another move."
  (cl-loop named outer for direction in directions do
           (cl-loop for ((i . j) . value) in svg-2048-board
                    when (or (svg-2048-movable-p i j direction)
                             (svg-2048-mergeable-p i j direction))
                    do (cl-return-from outer t))))

(defun svg-2048-do-move (direction)
  "Handles the game logic.
Makes sure that if the player lost or won, the game does the
correct actions such as displaying the appropriate overlay or
asking how to proceed."
  (when (svg-2048-move-possible-p (list direction))
      (setq svg-2048-merged-tiles '())
      (svg-2048-move-tiles direction)
      (svg-2048-set-random-tile)
      (svg-2048-redraw-board)
      (setq svg-2048-merged-tile-values
            (cl-loop for ((_ . _) . value) in svg-2048-merged-tiles
                     collect value))
      (if (and (memq svg-2048-winning-tile svg-2048-merged-tile-values)
               (not svg-2048-game-won-already))
          (when (not svg-2048-game-won-already)
            (setq svg-2048-game-won t
                  svg-2048-game-won-already t)
            (svg-2048-redraw-board)
            (if (yes-or-no-p "Continue?")
                (progn
                  (setq svg-2048-game-won nil)
                  (svg-2048-redraw-board))
              (setq svg-2048-merged-tiles '())
              (svg-2048-new-game)
              (svg-2048-redraw-board)))
        (unless (svg-2048-move-possible-p '(up down left right))
          (setq svg-2048-game-over t)
          (svg-2048-redraw-board)
          (when (yes-or-no-p "New Game?")
            (svg-2048-new-game)
            (svg-2048-redraw-board))))))

(defun svg-2048-move-left ()
  "Move all tiles left."
  (interactive)
  (svg-2048-do-move 'left))

(defun svg-2048-move-right ()
  "Move all tiles right."
  (interactive)
  (svg-2048-do-move 'right))

(defun svg-2048-move-up ()
  "Move all tiles up."
  (interactive)
  (svg-2048-do-move 'up))

(defun svg-2048-move-down ()
  "Move all tiles down."
  (interactive)
  (svg-2048-do-move 'down))

(defun svg-2048-set-random-tile ()
  "Choose a random empty tile, then set its value to 2 (90% of
the time) or 4."
  (interactive)
  (let* ((empty-tiles (svg-2048-empty-tiles))
         (random-number (random 1001))
         (random-tile-number (mod random-number (length empty-tiles)))
         (random-tile (nth random-tile-number empty-tiles))
         (random-tile-x (car random-tile))
         (random-tile-y (cdr random-tile)))
    (when empty-tiles
      (svg-2048-set-tile-value random-tile-x random-tile-y
                                (if (< random-number 900) 2 4))
      (when (called-interactively-p) (svg-2048-redraw-board)))))

(defun svg-2048-new-game ()
  "Starts a new game.  All game state is reset."
  (interactive)
  (svg-2048-initialize-board)
  (svg-2048-set-random-tile)
  (svg-2048-set-random-tile)
  (setq svg-2048-score 0
        svg-2048-game-over nil
        svg-2048-game-won nil
        svg-2048-game-won-already nil
        svg-2048-merged-tiles '())
  (svg-2048-redraw-board))

(defun svg-2048-redraw-board ()
  "Redraws the game board."
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "\n")
  (insert-image (create-image (svg-2048-create-svg) 'svg t))
  (setq buffer-read-only t))

(define-derived-mode svg-2048-mode special-mode "2048"
  "A SVG game."
  (buffer-disable-undo)
  (when svg-2048-use-original-background
    (buffer-face-set :background svg-2048-background-color))
  (svg-2048-new-game))

(define-minor-mode svg-2048-score-mode
  "Toggles score display for `svg-2048'."
  :lighter svg-2048-score-lighter)

(defun svg-2048-score-update ()
  "Updates the score displayed in the mode line."
  (setq svg-2048-score-lighter
        '(:eval (format svg-2048-score-lighter-format svg-2048-score))))

(define-key svg-2048-mode-map (kbd "w") 'svg-2048-move-up)
(define-key svg-2048-mode-map (kbd "a") 'svg-2048-move-left)
(define-key svg-2048-mode-map (kbd "s") 'svg-2048-move-down)
(define-key svg-2048-mode-map (kbd "d") 'svg-2048-move-right)
(define-key svg-2048-mode-map (kbd "h") 'svg-2048-move-left)
(define-key svg-2048-mode-map (kbd "j") 'svg-2048-move-down)
(define-key svg-2048-mode-map (kbd "k") 'svg-2048-move-up)
(define-key svg-2048-mode-map (kbd "l") 'svg-2048-move-right)
(define-key svg-2048-mode-map (kbd "<up>") 'svg-2048-move-up)
(define-key svg-2048-mode-map (kbd "<left>") 'svg-2048-move-left)
(define-key svg-2048-mode-map (kbd "<down>") 'svg-2048-move-down)
(define-key svg-2048-mode-map (kbd "<right>") 'svg-2048-move-right)
(define-key svg-2048-mode-map (kbd "g") 'svg-2048-redraw-board)
(define-key svg-2048-mode-map (kbd "n") 'svg-2048-new-game)

;;;###autoload
(defun svg-2048 ()
  "Open a new game buffer to play 2048."
  ;; TODO mimick tile styles (inner glow/shadow/stroke)
  ;; TODO score file with best score so far
  ;; TODO implement animation (or at least some hints)
  (interactive)
  (with-current-buffer (get-buffer-create "*svg 2048*")
    (svg-2048-mode)
    (when svg-2048-display-score
      (svg-2048-score-mode))
    (goto-char (point-min)))
  (display-buffer "*svg 2048*"))

(provide 'svg-2048)

;;; svg-2048.el ends here
