(require 'chess)

(defvar stockfish-process nil)
(defvar stockfish-evaluation-table (make-hash-table))
(defvar stockfish-analysis-buffer (get-buffer-create "*stockfish-analysis*"))
(defvar stockfish-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(defvar stockfish-multipv 5)

(defun stockfish-filter (process string)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      ;; append output to buffer, saving point
      (save-excursion
	(goto-char (point-max))
	(insert string))
      (while (> (point-max) (+ 1 (point)))
	(let ((eval (stockfish-process-line (thing-at-point 'line))))
	  (when eval
	    (stockfish-draw-eval eval)))
	(forward-line)))))

(defun stockfish-initialize ()
  (unless (and (bufferp stockfish-analysis-buffer)
	       (buffer-live-p stockfish-analysis-buffer))
    (get-buffer-create "*stockfish-analysis*")
    (stockfish-draw-new-analysis-buffer))
  (unless (and (processp stockfish-process)
	       (process-live-p stockfish-process))
    (setq stockfish-process
	  (make-process :name "stockfish"
			:buffer "*stockfish*"
			:command `(,(executable-find "stockfish"))
			:filter 'stockfish-filter))))

(defun stockfish-command (command-text)
  (stockfish-initialize)
  (process-send-string stockfish-process (concat command-text "\n")))

(defun stockfish-uci ()
  (stockfish-command "uci")
  (stockfish-command (format "setoption name MultiPV value %s" stockfish-multipv)))

(defun stockfish-quit ()
  (stockfish-command "quit")
  (setq stockfish-process nil))

(defun stockfish-set-position (&optional fen)
  (interactive "sFEN: ")
  ;; race condition because filter may still be processing output?
  (stockfish-command "stop")
  (stockfish-command (format "position fen %s" fen))
  (setq stockfish-fen fen)
  (stockfish-draw-new-analysis-buffer))

(defun stockfish-lookup-evaluation (fen)
  (gethash fen stockfish-evaluation-table '()))

(defvar example-info
  "info depth 33 seldepth 42 multipv 3 score cp -29 nodes 33868860 nps 564471 hashfull 965 tbhits 0 time 60001 pv a1a2 d2c3 f6g7 b2d3 a2a3 c3d4 a3b3 e3e5 e6c4 d3f4 b3b4 e5e8 c4d3 d4e3 d3g6 e8b8 b4b3 e3f2 b3b2 f2g3 b2b3 b8b6 b3a3 b6b5 g6e4 f4h5 g7f8 g3f4 e4c6 b5b6 a3f3 f4e5 f3e3 e5d4 e3e4 d4c5 e4g4 c5c6 g4h4")
(defvar example-info-mate
  "info depth 30 seldepth 13 multipv 1 score mate -6 nodes 1766743 nps 1616416 hashfull 471 tbhits 0 time 1093 pv d1d3 e4d3 f2f3 d3d2 e1f1 f4e2 g1g2 g4h2 g2f2 g5h4 f2e3 h4d4")

(defun stockfish-read-nth (n tokens)
  (read (or (nth n tokens) "err")))

;; tokens 
(defun stockfish-parse-evaluation (tokens)
  (let* ((depth (stockfish-read-nth 1 (member "depth" tokens)))
	 (seldepth (stockfish-read-nth 1 (member "seldepth" tokens)))
	 (multipv (stockfish-read-nth 1 (member "multipv" tokens)))
	 (eval-type (stockfish-read-nth 1 (member "score" tokens)))
	 ;; todo look for upperbound/lowerbound
	 (eval (stockfish-read-nth 2 (member "score" tokens)))
	 (nodes (stockfish-read-nth 1 (member "nodes" tokens)))
	 (nps (stockfish-read-nth 1 (member "nps" tokens)))
	 (time (stockfish-read-nth 1 (member "time" tokens)))
	 (pv (cdr (member "pv" tokens)))
	 (move (car pv)))
    `((move . ,move)
      (depth . ,depth)
      (seldepth . ,seldepth)
      (multipv . ,multipv)
      (eval-type . ,eval-type)
      (eval . ,eval)
      (nodes . ,nodes)
      (nps . ,nps)
      (time . ,time)
      (pv . ,pv))))

(defun stockfish-process-line (line)
  (let ((tokens (split-string line)))
    (if (member "score" tokens) ; iffy, but maybe works
	(stockfish-parse-evaluation tokens)
      nil)))

(defun stockfish-draw-new-analysis-buffer ()
  (with-current-buffer stockfish-analysis-buffer
    (save-excursion
      (delete-region (point-min) (point-max))
      (insert (format "FEN:   %s\n" stockfish-fen))
      (dotimes (+ 10 stockfish-multipv)
	(newline))
      (goto-char (point-min))
      (forward-line 4)
      (insert "\tmove\teval\tdepth")
      (stockfish-display-position
       (chess-fen-to-pos stockfish-fen)))))

(defun stockfish-draw-nodes (eval)
  (with-current-buffer stockfish-analysis-buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (format "(nodes, nps, time): (%s, %s, %s)"
		      (alist-get 'nodes eval)
		      (alist-get 'nps eval)
		      (alist-get 'time eval))))))

(defun stockfish-draw-eval (eval)
  (stockfish-draw-nodes eval)
  (let* ((line (alist-get 'multipv eval))
	 (move (alist-get 'move eval)))
    (when (stringp move) ;; sometimes move and pv fields are null...
      (with-current-buffer stockfish-analysis-buffer
	(save-excursion
	  (condition-case nil
	      (let ((move-text
		     (chess-ply-to-algebraic
		      (chess-algebraic-to-ply
		       (chess-fen-to-pos stockfish-fen)
		       (alist-get 'move eval))
		      :fan)))
		(goto-char (point-min))
		(forward-line (+ line 4))
		(delete-region (line-beginning-position) (line-end-position))
		(insert (format "\t%s\t%s%d\t%2d/%2d"
				move-text
				(if (eq 'mate (alist-get 'eval-type eval)) "#" "")
				(alist-get 'eval eval)
				(alist-get 'depth eval)
				(alist-get 'seldepth eval))))
	    ((error) ;; still a dodgy way to handle errors. sometimes
		     ;; changing the position results in problems
		     ;; because like a dummy i am using global
		     ;; variables
	     nil)))))))

(defun stockfish-stop ()
  (interactive)
  (stockfish-command "stop"))

(defun stockfish-run (seconds)
  (interactive)
  (stockfish-initialize)
  (stockfish-stop)
  (stockfish-draw-new-analysis-buffer)
  (with-current-buffer (get-buffer "*stockfish*")
    (goto-char (point-max)))
  (stockfish-command (format "go movetime %s" (* 1000 seconds)))
  (display-buffer stockfish-analysis-buffer)
  'stockfish)

(defun stockfish-go ()
  (interactive)
  (stockfish-initialize)
  (stockfish-stop)
  (stockfish-draw-new-analysis-buffer)
  (with-current-buffer (get-buffer "*stockfish*")
    (goto-char (point-max)))
  (stockfish-command "go")
  (display-buffer stockfish-analysis-buffer)
  'stockfish)

(defun stockfish-yank-position ()
  (interactive)
  (let ((fen (current-kill 0)))
    (if (not (chess-fen-to-pos fen))
	(message "chess: current kill is not a valid FEN")
      (stockfish-set-position fen)
      (stockfish-go)
      (message "chess: current FEN updated"))))

(defun stockfish-gogo ()
  (stockfish-uci)
  (stockfish-run 120))

(global-set-key (kbd "C-c f e n") 'stockfish-yank-position)
