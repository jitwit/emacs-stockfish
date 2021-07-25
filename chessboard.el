(require 'chess)
(require 'svg)

(defvar svg nil)
(defvar board-pixels 400)
(defvar stockfish-piece-image-directory
  "images/pieces/fresca/")

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
	(i (/ index 8))
	(j (mod index 8)))
    (when file
      (svg-embed board
		 file
		 "image/svg"
		 nil
		 :width (/ board-pixels 8)
		 :height (/ board-pixels 8)
		 :x (* j (/ board-pixels 8))
		 :y (* i (/ board-pixels 8))))))

(defun stockfish-display-position (position)
  (setq svg (svg-create board-pixels board-pixels))
  (embed-background svg)
  (dotimes (i 7)
    (dotimes (j 7)
      (embed-piece svg position (+ j (* 8 i)))))
  (save-excursion
    (goto-char (point-max))
    (insert-image
     (svg-image svg))))

(stockfish-display-position
 (chess-fen-to-pos stockfish-fen))
