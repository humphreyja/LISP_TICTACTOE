(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
          (new-board (make-move *user* pos board)))
      (display-board new-board)
      (cond ((winner-p new-board)
             (format t "~&You Win!"))
            ((board-full-p new-board)
             (format t "~&Tie game."))
            (t (computer-move new-board)))))
