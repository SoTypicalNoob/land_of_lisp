;; Implementing Dice of Doom, Version 1
;;; Defining Some Global Varibles
(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Representing the Game Board
;;; Clean/Functional
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))
;;; Dirty/Imperative
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))
;;; Clean/Functional
(defun player-letter (n)
  (code-char (+ 97 n)))
;;; Dirty/Imperative
(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ " "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))
;;; Generating a Game Tree
;;; Clean/Functional
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))
;;; Calculating Passing Moves
;;; Clean/Functional
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

;;; Calculating Attacking Moves
;;; Clean/Functional
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list (list src dst)
                                   (game-tree (board-attack board cur-player src dst (dice src))
                                              cur-player
                                              (+ spare-dice (dice dst))
                                              nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

;;; Finding the Neighbors
;;; Clean/Functional
(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
            collect p)))

;;; Attacking
;;; Clean/Functional
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

;;; Reinforcements
;;; Clean/Functional
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
               (cond ((null lst) nil)
                     ((zerop n) lst)
                     (t (let ((cur-player (caar lst))
                              (cur-dice (cadar lst)))
                          (if (and (eq cur-player player) (< cur-dice *max-dice*))
                              (cons (list cur-player (1+ cur-dice))
                                    (f (cdr lst) (1- n)))
                              (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

;; Playing Dice of Doom Against Another Human
;;; The Main Loop
;;; Dirty/Imperative
(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

;;; Giving Information About the State of the Game
;;; Dirty/Imperative
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

;;; Handling Input from Human Players
;;; Dirty/Imperative
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop :for move :in moves
          :for n :from 1
          :do (let ((action (car move)))
                (fresh-line)
                (format t "~a. " n)
                (if action
                    (format t "~a -> ~a" (car action) (cadr action))
                    (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

;;; Determingin the Winner
;;; Clean/Functional
(defun winners (board)
  (let* ((tally (loop :for hex :across board
                      :collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

;; Creating an Intelligent Computer Opponent
;;; The Minimax Algorithm / Turning Minimax into Actual Code
;;; Clean/Functional
(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))

;;; Clean/Functional
(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

;;; Creating a Game Loop with an AI Player
;;; Dirty/Imperative
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings)  (caddr tree)))))

;;; Dirty/Imperative
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; Making Dice of Doom Faster
(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Closures / Memoization

;;; Memoizing the neighbors Function
(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))
;; Be careful not to declare the memoized version of the neighbors function more than once,
;; without also redeclaring the original version of the function.
;; Otherwise, the neighbors function will be wrapped in multiple unsightly layers of memoization,
;; since there are no checks if the memoization has already been done.

;;; Memoizing the Game Tree
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))

;;; Memoizing the rate-position Function
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))
