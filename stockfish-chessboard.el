(require 'chess)
(require 'svg)

(defvar svg nil)
(defvar board-pixels 512)
(defvar stockfish-piece-image-directory
  "images/pieces/fresca/")
(defvar stockfish-board-perspective 1)

(defun stockfish-image-of-piece (piece)
  (let ((file
	 (and (not (eq ?\s piece))
	      (string
	       (if (< piece 97) ?w ?b)
	       (if (< piece 97) piece (- piece 32))))))
    (when file
      (concat stockfish-piece-image-directory
	      file
	      ".svg"))))

(defun embed-background (board)
  (svg-embed board
	     "images/board.jpg"
	     "image/jpeg"
	     nil
	     :width board-pixels
	     :height board-pixels
	     :x 0
	     :y 0))

(defun embed-piece (board position index)
  (let ((file (stockfish-image-of-piece (aref position index)))
	(i (mod index 8))
	(j (/ index 8)))
    (when file
      (svg-embed board
		 file
		 "image/svg"
		 nil
		 :width (/ board-pixels 8)
		 :height (/ board-pixels 8)
		 :x (* i (/ board-pixels 8))
		 :y (* j (/ board-pixels 8))))))

(defun stockfish-display-position (position)
  (setq svg (svg-create board-pixels board-pixels))
  (embed-background svg)
  (dotimes (i 8)
    (dotimes (j 8)
      (embed-piece svg position (+ j (* 8 i)))))
  (save-excursion
    (goto-char (point-max))
    (insert-image
     (svg-image svg))))

;; (setq board-pixels 512)
;; (stockfish-display-position
;;  (chess-fen-to-pos stockfish-fen))
