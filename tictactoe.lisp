(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(setf *computer* 10)
(setf *user* 1)

;;; =====================  Output ==============================================
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



(defun make-move (player pos board)
    (setf (nth pos board) player)
    board)

;;posible solutions
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



(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first?")
      (opponent-move (make-board))
      (computer-move (make-board))))

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

;;; ========================  Predicates  ======================================
(defun board-full-p (board)
  (not (member 0 board)))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or
        (member (* 3 *computer*) sums)
        (member (* 3 *user*) sums))))
