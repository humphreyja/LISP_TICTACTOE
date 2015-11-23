(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board) "Random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
      pos
      (pick-random-empty-position board))))

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

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
      (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *user*))))
      (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                    #'(lambda (trip)
                          (equal (sum-triplet board trip) target-sum))
                          *triplets*)))
      (when triplet
        (find-empty-position board triplet))))

(defun find-empty-position (board triplet)
  (find-if #'(lambda (pos)
                (zerop (nth pos board)))
                triplet))
