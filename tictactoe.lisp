(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(setf *computer* 10)
(setf *user* 1)

(defun print-row (x y z)
  (format t "~% ~a | ~a | ~a "
    (convert-to-character x)
    (convert-to-character y)
    (convert-to-character z)))

(defun convert-to-character (x)
  (cond
    ((= x 10) "X")
    ((= x 1) "O")
    (t " ")))

(defun display-board (board)
  (print-row (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~%-----------")
  (print-row (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~%-----------")
  (print-row (nth 7 board) (nth 8 board) (nth 9 board))
  )

(setf b (make-board))

(defun make-move (player pos board)
    (setf (nth pos board) player)
    board)

(setf *triplets*
  '((1 2 3) (4 5 6) (7 8 9)
    (1 4 7) (2 5 8) (3 6 9)
    (1 5 9) (3 5 7)))

(defun sum-triplet (board triplet)
  (+
    (nth (first triplet) board)
    (nth (second triplet) board)
    (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
                (sum-triplet board triplet))
                  *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or
        (member (* 3 *computer*) sums)
        (member (* 3 *user*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first?")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
          (new-board (make-move *user* pos board)))
      (display-board new-board)
      (cond ((winner-p new-board)
             (format t "~&You Win!"))
            ((board-full-p new-board)
             (format t "~&Tie game."))
            (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos) (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
          (pos (first best-move))
          (strategy (second best-move))
          (new-board (make-move *computer* pos board)))
      (format t "~&My move: ~s" pos)
      (format t "~&My strategy: ~a~%" strategy)
      (display-board new-board)
      (cond ((winner-p new-board)
             (format t "~%I win!"))
            ((board-full-p new-board)
             (format t "~%Tie game."))
            (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (random-move-strategy board))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "Random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos
      (pick-random-empty-position board))))
