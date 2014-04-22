;;; svg-2048.el --- A SVG version of the titular game

;; Copyright (C) 2014 Vasilij Schneidermann <v.schneidermann@gmail.com>

;; Author: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; URL: https://github.com/wasamasa/eyebrowse
;; Version: 0.0.1
;; Package-Requires: ((dash "2.4.0") (s "1.4.0") (esxml "0.3.0") (emacs "24.3"))
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

(defcustom svg-2048-original-background-p nil
  "When t, use the original background for the entire buffer.
Otherwise the game will use a transparent background."
  :type 'boolean
  :group 'svg-2048)

(defcustom svg-2048-font "sans-serif"
  "Font to use for tiles."
  :type 'string
  :group 'svg-2048)

(defvar svg-2048-board-color "#bbada0")
(defvar svg-2048-tile-color "#eee4da")

(defvar svg-2048-padding 8)
(defvar svg-2048-board-size 4)
(defvar svg-2048-padding 14)
(defvar svg-2048-box-size 107)
(defvar svg-2048-offset 8)
(defvar svg-2048-roundness 6)

(defvar svg-2048-board nil)
(defvar svg-2048-merged-tiles nil)
(defvar svg-2048-maximum-merged-tile 0)
(defvar svg-2048-game-over-p nil)
(defvar svg-2048-game-won-p nil)
(defvar svg-2048-score-lighter " Score: 0")
(defvar svg-2048-score 0)

(defun svg-2048-create-svg ()
  (let* ((field-width (+ (* (1+ svg-2048-board-size) svg-2048-padding)
                         (* svg-2048-board-size svg-2048-box-size)))
         (field-height (+ (* (1+ svg-2048-board-size) svg-2048-padding)
                          (* svg-2048-board-size svg-2048-box-size)))
         (width (+ field-width (* 2 svg-2048-offset)))
         (height (+ field-width (* 2 svg-2048-offset))))
    (cl-flet ((n->s (n) (number-to-string n)))
      (sxml-to-xml
       `(svg
         (@ (xmlns "http://www.w3.org/2000/svg")
            (width ,(n->s width))
            (height ,(n->s height)))
         ,(when svg-2048-original-background-p
            `(g (rect
                 (@ (width ,(n->s width))
                    (height ,(n->s height))
                    (fill "#faf8ef")))))
         (g (rect
             (@ (width ,(n->s field-width))
                (height ,(n->s field-height))
                (rx ,(n->s svg-2048-roundness))
                (x ,(n->s svg-2048-offset))
                (y ,(n->s svg-2048-offset))
                (fill ,svg-2048-board-color)))
            ,@(cl-loop for ((i . j) . value) in svg-2048-board collect
                       (svg-2048-tile i j value))
            ,(cond ((not (null svg-2048-game-over-p))
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
                    ((not (null svg-2048-game-won-p))
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
  (cl-flet ((n->s (n) (number-to-string n)))
    (let* ((digits
            (if value (length (number-to-string value)) 0))
           (tile-roundness (/ svg-2048-roundness 2))
           (tile-color (cond
                        ((null value) "#ccc0b4")
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
                      (* y svg-2048-box-size)
                      svg-2048-offset))
           (tile-y (+ (* (1+ x) svg-2048-padding)
                      (* x svg-2048-box-size)
                      svg-2048-offset))
           (text-size (cond
                       ((< digits 3) "50")
                       ((= digits 3) "40")
                       ((or (> value 2048)
                            (> digits 4)) "25")
                       ((= digits 4) "30")))
           (text-color (cond
                        ((or (null value) (< value 8))
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
                            svg-2048-box-size) 2)
                      svg-2048-offset))
           (text-y (+ (* (+ x 2) svg-2048-padding)
                      (/ (* (1- (* (1+ x) 2))
                            svg-2048-box-size) 2)
                      svg-2048-offset)))
      `(g (rect
           (@ (width ,(n->s svg-2048-box-size))
              (height ,(n->s svg-2048-box-size))
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

(defun svg-2048-get-board-value (x y)
  (cdr (assoc (cons x y) svg-2048-board)))

(defun svg-2048-set-board-value (x y value)
  (setq svg-2048-board
        (-replace-at (-elem-index (assoc (cons x y) svg-2048-board)
                                  svg-2048-board)
                     (cons (cons x y) value) svg-2048-board)))

(defun svg-2048-new-coord (x y direction)
  (let ((new-x (cond ((eq direction 'up) (max 0 (1- x)))
                     ((eq direction 'down)
                      (min (1- svg-2048-board-size) (1+ x)))
                     (t x)))
        (new-y (cond ((eq direction 'left) (max 0 (1- y)))
                     ((eq direction 'right)
                      (min (1- svg-2048-board-size) (1+ y)))
                     (t y))))
    (cons new-x new-y)))

(defun svg-2048-movable (x y direction)
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-board-value x y))
         (new-value (svg-2048-get-board-value new-x new-y)))
    (when (and value (not (equal (cons x y) new-coord)) (null new-value))
      new-coord)))

(defun svg-2048-mergeable (x y direction)
  (let* ((new-coord (svg-2048-new-coord x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-board-value x y))
         (new-value (svg-2048-get-board-value new-x new-y)))
    (when (and value (not (equal (cons x y) new-coord))
               new-value (= value new-value))
      new-coord)))

(defun svg-2048-move-tile (x y direction)
  (let* ((new-coord (svg-2048-movable x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-board-value x y)))
    (when (and value new-coord)
      (svg-2048-set-board-value new-x new-y value)
      (svg-2048-set-board-value x y nil)
      new-coord)))

(defun svg-2048-merge-tile (x y direction)
  (let* ((new-coord (svg-2048-mergeable x y direction))
         (new-x (car new-coord))
         (new-y (cdr new-coord))
         (value (svg-2048-get-board-value x y)))
    (when (and value new-coord)
      (svg-2048-set-board-value new-x new-y (* 2 value))
      (svg-2048-set-board-value x y nil)
      (setq svg-2048-score (+ svg-2048-score (* 2 value)))
      (svg-2048-score-update)
      new-coord)))

(defun svg-2048-move-tile-to-end (x y direction)
  (let ((current-x x)
        (current-y y)
        (new-coord (svg-2048-new-coord x y direction)))
    (while (svg-2048-movable current-x current-y direction)
      (setq new-coord (svg-2048-move-tile current-x current-y direction))
      (setq current-x (car new-coord))
      (setq current-y (cdr new-coord)))
    (when (svg-2048-mergeable current-x current-y direction)
      (setq new-coord (svg-2048-new-coord current-x current-y direction))
      (when (null (assoc new-coord svg-2048-merged-tiles))
        (svg-2048-merge-tile current-x current-y direction)
        (setq current-x (car new-coord))
        (setq current-y (cdr new-coord))
        (add-to-list 'svg-2048-merged-tiles
                     (cons (cons current-x current-y)
                           (svg-2048-get-board-value current-x current-y)))))))

(defun svg-2048-move-tiles (direction)
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
    (cl-loop for (i . j) in ij-list when (svg-2048-get-board-value i j) do
             (svg-2048-move-tile-to-end i j direction))))

(defun svg-2048-empty-tiles ()
  (cl-loop for ((i . j) . value) in svg-2048-board
           if (null value) collect
           (cons i j)))

(defun svg-2048-move-possible-p (directions)
  (cl-loop named outer for direction in directions do
           (cl-loop for ((i . j) . value) in svg-2048-board
                    when (or (svg-2048-movable i j direction)
                             (svg-2048-mergeable i j direction))
                    do (cl-return-from outer t))))

(defun svg-2048-do-move (direction)
  (when (svg-2048-move-possible-p (list direction))
      (setq svg-2048-merged-tiles '())
      (svg-2048-move-tiles direction)
      (svg-2048-set-random-tile)
      (svg-2048-redraw-board)
      (setq svg-2048-maximum-merged-tile
            (cl-loop for ((_ . _) . value) in svg-2048-merged-tiles
                     maximize value))
      (if (and (equal svg-2048-maximum-merged-tile svg-2048-winning-tile)
               (null svg-2048-game-won-p))
          (progn
            (setq svg-2048-game-won-p t)
            (svg-2048-redraw-board)
            (if (yes-or-no-p "Continue?")
                (progn
                  (setq svg-2048-game-won-p nil)
                  (svg-2048-redraw-board))
              (setq svg-2048-merged-tiles '())
              (svg-2048-new-game)
              (svg-2048-redraw-board)))
      (unless (svg-2048-move-possible-p '(up down left right))
        (setq svg-2048-game-over-p t)
        (svg-2048-redraw-board)
        (when (yes-or-no-p "New Game?")
          (svg-2048-new-game)
          (svg-2048-redraw-board))))))

(defun svg-2048-move-left ()
  (interactive)
  (svg-2048-do-move 'left))

(defun svg-2048-move-right ()
  (interactive)
  (svg-2048-do-move 'right))

(defun svg-2048-move-up ()
  (interactive)
  (svg-2048-do-move 'up))

(defun svg-2048-move-down ()
  (interactive)
  (svg-2048-do-move 'down))

(defun svg-2048-set-random-tile ()
  (interactive)
  (let* ((empty-tiles (svg-2048-empty-tiles))
         (random-number (random 1001))
         (random-tile-number (mod random-number (length empty-tiles)))
         (random-tile (nth random-tile-number empty-tiles))
         (random-tile-x (car random-tile))
         (random-tile-y (cdr random-tile)))
    (when empty-tiles
      (svg-2048-set-board-value random-tile-x random-tile-y
                                (if (< random-number 900) 2 4))
      (when (called-interactively-p) (svg-2048-redraw-board)))))

(define-derived-mode svg-2048-mode special-mode "2048"
  "A SVG game"
  (with-current-buffer (get-buffer-create "*svg 2048*")
    (buffer-disable-undo)
    (when svg-2048-original-background-p
      (buffer-face-set :background "#faf8ef"))
    (svg-2048-new-game)))

(define-minor-mode svg-2048-score-mode
  "Toggle score display for `svg-2048'."
  :lighter svg-2048-score-lighter)

(defun svg-2048-score-update ()
  (setq svg-2048-score-lighter
        '(:eval (concat " Score: " (number-to-string svg-2048-score)))))

;;;###autoload
(defun svg-2048 ()
  ;; TODO fix arithmetic error popping up randomly
  ;; TODO add more customizations
  ;; TODO mimick tile styles (inner glow/shadow/stroke)
  ;; TODO score file with best score so far
  ;; TODO implement animation (or at least some hints)
  (interactive)
  (switch-to-buffer "*svg 2048*")
  (svg-2048-mode)
  (svg-2048-score-mode)
  (goto-char (point-max)))

(defun svg-2048-fill-board (rows)
  (cl-loop for row in rows for i = 0 then (1+ i) do
           (cl-loop for tile in row for j = 0 then (1+ j) do
                    (svg-2048-set-board-value i j tile))))

(defun svg-2048-initialize-board ()
  (setq svg-2048-board
        (cl-loop for i from 0 to (1- svg-2048-board-size)
                 nconc
                 (cl-loop for j from 0 to (1- svg-2048-board-size) collect
                          (cons (cons i j) nil)))))

(defun svg-2048-new-game ()
  (interactive)
  (svg-2048-initialize-board)
  (svg-2048-set-random-tile)
  (svg-2048-set-random-tile)
  (setq svg-2048-score 0)
  (setq svg-2048-game-over-p nil)
  (setq svg-2048-game-won-p nil)
  (setq svg-2048-merged-tiles '())
  (svg-2048-redraw-board))

(defun svg-2048-redraw-board ()
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-image (create-image (svg-2048-create-svg) 'svg t))
  (insert "\n")
  (setq buffer-read-only t))

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

(provide 'svg-2048)

;;; svg-2048.el ends here
