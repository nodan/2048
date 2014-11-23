;;; 2048.el --- 2048 in Emacs

;; Copyright (C) 2014 Mathias Kegelmann

;; Author: Mathias Kegelmann
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; The functions run a game of 2048 in its own buffer and major mode.
;; For the original "2048" see http://gabrielecirulli.github.io/2048/ and
;; https://github.com/gabrielecirulli/2048 which is itself based on
;; http://asherv.com/threes/ (http://threesgame.com/).


;;; Installation:

;; Put (load-library "PATH_TO_2048/2048") in your .emacs file.  This
;; loads 2048.elc if a byte compiled version is found or 2048.el,
;; otherwise.
;; Alternatively, ensure that 2048.el[c] is on the load path and add
;; (autoload '2048-play "2048" nil t) to your .emacs file.

(eval-when-compile (require 'cl))

(defvar 2048-board nil "Vector containing the current position.")
(defvar 2048-score 0 "Current game score.
The score is computed as follows: Every time two numbers n are combined
into a new number 2n the score is increased by 2n.

Assuming that only 2s are added, the score is the sum of the values of the
individual numbers and the value of 2^k is (k-1)2^k, e.g. the value of
2048 is 10*2048=20480.

Actually, only 90% of the numbers added randomly are 2s and 10% are 4s.
Taking two 4s (adding nothing to the score) and eighteen 2s that combine
into nine 4s with a score of 4 each, we get an approximate value of 4s as
 1/11(9*4 + 2*0) = 36/11 = 3 3/11 = 3.27...
If we set DELTA := v'(4) - v(4) = 4-36/11 = 8/11, then we get the
slightly lower (asymptotically identical) value of 2^k:
 (k-1)2^k - 2^(k-2)DELTA.

This leads to the following table of number values:

|--------+------------+--------+-------------------|
| Number | Only RND 2 |  Value |     Approx. Value |
|--------+------------+--------+-------------------|
|      4 | 1 * 4      |      4 | 3 27/99 = 3.27... |
|      8 | 2 * 8      |     16 |             14.54 |
|     16 | 3 * 16     |     48 |             45.09 |
|     32 | 4 * 32     |    128 |            122.18 |
|     64 | 5 * 64     |    320 |            308.36 |
|    128 | 6 * 128    |    768 |            744.72 |
|    256 | 7 * 256    |   1792 |           1745.45 |
|    512 | 8 * 512    |   4096 |           4002.90 |
|   1024 | 9 * 1024   |   9216 |           9029.81 |
|   2048 | 10 * 2048  |  20480 |          20107.63 |
|   4096 | 11 * 4096  |  45056 |          44311.27 |
|   8192 | 12 * 8192  |  98304 |          96814.54 |
|  16384 | 13 * 16384 | 212992 |         210013.09 |
|  32768 | 14 * 32768 | 458752 |         452794.18 |
|--------+------------+--------+-------------------|
")
(defvar 2048-buffer-name "*2048*")
(defvar 2048-mode-hook nil "*Hook run when entering 2048 mode.")
(defvar 2048-previous-board nil
  "Vector containing the previous position for undo using `2048-undo'.")
(defvar 2048-previous-score nil "`2048-score' stored for `2048-undo'")
(defvar 2048-random-queue nil "See `2048-undoable-random'")
(defvar 2048-previous-random-queue nil "See `2048-undoable-random'")

(defun 2048-pad (n)
  "Pad string or number N to length 7."
  (let* ((s (if (numberp n) (format "%d" n) (if n n "")))
         (n (length s))
         (l (/ (- 7 n) 2))
         (r (- 7 n l)))
    (concat (make-string l ? ) s (make-string r ? ))))

(defun 2048-update-screen-cell (x y n)
  "Update a number (or string) on the screen, i.e. in the buffer."
  (goto-char (+ 1 (* 34 (+ 2 (* 4 y))) (* 8 x) 1))
  (delete-char 7)
  (insert (2048-pad n)))

(defun 2048-update-all-screen-cells ()
  "Update all numbers is the grid and the score"
  (dotimes (x 4)
    (dotimes (y 4)
      (let ((c (2048-cell-at x y)))
       (2048-update-screen-cell x y (if (> c 0) c)))))
  (goto-char (point-min))
  (search-forward "Score: ")
  (delete-region (point) (line-end-position))
  (insert (format "%d" 2048-score))
  (goto-char 35))

(defun 2048-redisplay ()
  "Redraw the entire screen/buffer."
  (interactive)
  (switch-to-buffer 2048-buffer-name)
  (erase-buffer)
  (let ((separator "+-------+-------+-------+-------+\n")
        (line "|       |       |       |       |\n"))
    (dotimes (i 4)
     (insert separator)
     (dotimes (j 3) (insert line)))
    (insert separator))
  (insert "\nScore: ")
  (2048-update-all-screen-cells))

(defun 2048-cell-at (x y)
  "Get cell at (X, Y)."
  (aref (aref 2048-board y) x))

(defun 2048-set-cell-at (x y n)
  "Set cell at (X, Y) to N."
  (aset (aref 2048-board y) x n))

(defun 2048-free-cells ()
  (let ((r nil))
    (dotimes (x 4)
      (dotimes (y 4)
        (when (= 0 (2048-cell-at x y))
          (setq r (cons (cons x y) r)))))
    r))

(defun 2048-random-drop ()
  "Add a 2 or 4 to an empty cell at random.
If there is no empty cell, then the function does nothing."
  (let* ((slots (2048-free-cells))
         (n (length slots)))
    (when (> n 0)
      (let ((xy (nth (2048-undoable-random n) slots)))
        (2048-set-cell-at (car xy) (cdr xy)
                          (if (< (2048-undoable-random 10) 9) 2 4)))
      (when (= n 1) (2048-handle-gameover)))))

(defun 2048-undoable-random (n)
  "Undoable version of `random'.
It assumes that `2048-random-queue' is non empty:
We store random numbers in `2048-random-queue' (and the backup location
`2048-previous-random-queue' for undo).  The function
`2048-ensure-random-queue' adds random numbers to the queue if needed."
  (assert 2048-random-queue nil "2048-random-queue must not be empty")
  (let ((r (mod (car 2048-random-queue) n)))
    (setq 2048-random-queue (cdr 2048-random-queue))
    r))

(defun 2048-ensure-random-queue (n)
  "Ensure that `2048-random-queue' has at least N elements.
See `2048-undoable-random'."
  (while (< (length 2048-random-queue) n)
    (setq 2048-random-queue (cons (random) 2048-random-queue))))

(defun 2048-handle-gameover ()
  "Insert game over message if there are no more moves."
  (when (2048-gameover)
        (goto-char (point-max))
    (newline)
    (insert "*** Game Over ***")))

(defun 2048-gameover ()
  "Check whether the game is over."
  (catch 'ok
    (dolist (i (number-sequence 1 3))
      (dotimes (j 4)
        (when (or
               (= (2048-cell-at i j) (2048-cell-at (1- i) j))
               (= (2048-cell-at j i) (2048-cell-at j (1- i))))
          (throw 'ok nil))))
    t))

(defun 2048-start ()
  "Set up a game to start or restart it."
  (interactive)
  (setq 2048-board (vector (vector 0 0 0 0)
                           (vector 0 0 0 0)
                           (vector 0 0 0 0)
                           (vector 0 0 0 0)))
  (setq 2048-score 0)
  (random t) ; seed from current time
  (2048-ensure-random-queue 4)
  (2048-random-drop)
  (2048-random-drop)
  (2048-redisplay))

(defun 2048-valid-coordinates-p (x y)
  (and (>= x 0) (< x 4) (>= y 0) (< y 4)))

(defun 2048-fall (dx dy)
  "The main algorithm implementing one step/move."
  (interactive)
  (2048-ensure-random-queue 2)
  (2048-backup-board)
  (let ((up (number-sequence 0 3))
        (down (number-sequence 3 0 -1))
        blacklist moved)
    (dolist (x (if (< dx 0) up down))
      (dolist (y (if (< dy 0) up down))
        (let ((n (2048-cell-at x y)))
          (when (> n 0)
            (let ((x2 x) (y2 y) (x3 (+ x dx)) (y3 (+ y dy)))
              (while (and (2048-valid-coordinates-p x3 y3)
                          (= 0 (2048-cell-at x3 y3)))
                (setq x2 x3 y2 y3 x3 (+ x3 dx) y3 (+ y3 dy)))
              (2048-set-cell-at x y 0)
              (if (and (2048-valid-coordinates-p x3 y3)
                       (not (member (cons x3 y3) blacklist))
                       (= n (2048-cell-at x3 y3)))
                  (progn
                    (2048-set-cell-at x3 y3 (* 2 n))
                    (setq 2048-score (+ 2048-score (* 2 n)))
                    (setq blacklist (cons (cons x3 y3) blacklist))
                    (setq moved t))
                (2048-set-cell-at x2 y2 n)
                (setq moved (or moved (not (= x x2)) (not (= y y2))))))))))
    (when moved (2048-random-drop))
    (2048-update-all-screen-cells)))

(defun 2048-backup-board ()
  "Make a copy for undo."
  (setq 2048-previous-board (vconcat (mapcar #'copy-sequence 2048-board)))
  (setq 2048-previous-score 2048-score)
  (setq 2048-previous-random-queue 2048-random-queue))

(defun 2048-play ()
  "Play 2048!"
  (interactive)
  (switch-to-buffer 2048-buffer-name)
  (2048-mode)
  (if 2048-board
      (2048-redisplay)
    (2048-start)))

;; devel helper: (makunbound '2048-mode-map)
(defvar 2048-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right] '2048-cmd-right)
    (define-key map [left] '2048-cmd-left)
    (define-key map [down] '2048-cmd-down)
    (define-key map [up] '2048-cmd-up)
    (define-key map "s" '2048-start)
    (define-key map "S" '2048-start-server)
    (define-key map "r" '2048-redisplay)
    (define-key map (kbd "RET") '2048-redisplay)
    (define-key map "u" '2048-undo)
    (define-key map (kbd "\C-_") '2048-undo)
    (define-key map "q" #'(lambda () (interactive)
                            (kill-buffer (current-buffer))))
    (suppress-keymap map)
    map)
  "Keymap for 2048 mode.")

(defun 2048-cmd-right () (interactive) (2048-fall 1 0))
(defun 2048-cmd-left () (interactive) (2048-fall -1 0))
(defun 2048-cmd-down () (interactive) (2048-fall 0 1))
(defun 2048-cmd-up () (interactive) (2048-fall 0 -1))

(defun 2048-undo ()
  (interactive)
  (setq 2048-board 2048-previous-board)
  (setq 2048-score 2048-previous-score)
  (setq 2048-random-queue 2048-previous-random-queue)
  (2048-update-all-screen-cells))

(defface 2048-face-2
  '((t (:foreground "black" :weight bold :background "gray95")))
  "2048:2" :group '2048-faces)
(defface 2048-face-4
  '((t (:foreground "black" :weight bold :background "gray85")))
  "2048:4" :group '2048-faces)
(defface 2048-face-8
  '((t (:foreground "white" :weight bold :background "orange1")))
  "2048:8" :group '2048-faces)
(defface 2048-face-16
  '((t (:foreground "white" :weight bold :background "orange2")))
  "2048:16" :group '2048-faces)
(defface 2048-face-32
  '((t (:foreground "white" :weight bold :background "orange3")))
  "2048:32" :group '2048-faces)
(defface 2048-face-64
  '((t (:foreground "white" :weight bold :background "red")))
  "2048:64" :group '2048-faces)
(defface 2048-face-128
  '((t (:foreground "white" :weight bold :background "goldenrod2")))
  "2048:128" :group '2048-faces)
(defface 2048-face-256
  '((t (:foreground "white" :weight bold :background "goldenrod1")))
  "2048:256" :group '2048-faces)
(defface 2048-face-512
  '((t (:foreground "white" :weight bold :background "gold1")))
  "2048:512" :group '2048-faces)
(defface 2048-face-1024
  '((t (:foreground "white" :weight bold :background "yellow2")))
  "2048:1024" :group '2048-faces)
(defface 2048-face-2048
  '((t (:foreground "white" :weight bold :background "yellow1")))
  "2048:2048" :group '2048-faces)
(defface 2048-face-big
  '((t (:foreground "white" :weight bold :background "black")))
  "2048:big numbers" :group '2048-faces)


;; devel helper: (makunbound '2048-mode-font-lock-keywords)
(defvar 2048-mode-font-lock-keywords
  '((" 2 " . '2048-face-2)
    (" 4 " . '2048-face-4)
    (" 8 " . '2048-face-8)
    (" 16 " . '2048-face-16)
    (" 32 " . '2048-face-32)
    (" 64 " . '2048-face-64)
    (" 128 " . '2048-face-128)
    (" 256 " . '2048-face-256)
    (" 512 " . '2048-face-512)
    (" 1024 " . '2048-face-1024)
    (" 2048 " . '2048-face-2048)
    (" [0-9]+ " . '2048-face-big))
  "*2048 mode font lock patterns.")


(defun 2048-mode ()
  "Major mode to play 2048 in Emacs.

Keymap:
\\{2048-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode '2048-mode)
  (setq mode-name "2048 mode")
  (use-local-map 2048-mode-map)
  (setq font-lock-defaults '(2048-mode-font-lock-keywords 't))
  (put '2048-mode 'mode-class 'special)
  (text-scale-set
   (floor (/ (log
              (min (/ (window-width) 33.0)
                   (/ (window-height) 20.0)))
             (log 1.2))))
  (setq cursor-type)
  (run-hooks '2048-mode-hook))

;;; remote control via TCP connection
(defvar 2048-server-name "2048 server")
(defvar 2048-server-port 2048)

(defun 2048-filter (proc msg)
  (dolist (line (split-string msg))
      (let ((cmd (read line)))
        (when cmd
          (message "2048 remote command: %s" cmd)
          (with-current-buffer 2048-buffer-name
            (cond
             ((eq cmd :up) (2048-cmd-up))
             ((eq cmd :down) (2048-cmd-down))
             ((eq cmd :left) (2048-cmd-left))
             ((eq cmd :right) (2048-cmd-right))
             ((eq cmd :board)
              (process-send-string proc (prin1-to-string 2048-board)))
             ((eq cmd :score)
              (process-send-string proc (prin1-to-string 2048-score)))
             ((eq cmd :gameover)
              (process-send-string proc (if (2048-gameover) "1" "0")))
             ((eq cmd :start) (2048-start))
             (t (process-send-string proc "Error: Command not recognized"))))
          (when (memq cmd '(:up :down :left :right :start))
            (process-send-string proc "OK"))
          (process-send-string proc "\n")))))

(defun 2048-start-server ()
  "Start the 2048 remote control"
  (interactive)
  (make-network-process
   :server t
   :name 2048-server-name
   :service 2048-server-port
   :filter #'2048-filter
   :host 'local))

;;; 2048.el ends here