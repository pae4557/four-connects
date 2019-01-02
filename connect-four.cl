;; status = 0,1,2 = playing, won, drawn
;; depth = search depth
;; score = score for a win
;; moves = number of moves taken
;; board = game board (array, 6x7, contains 'x', 'o', or NIL)
(defstruct game status depth score moves board)

;;Game start:
(defparameter g (make-game 
         :status 0
         :moves 0 
         :board (make-array '(6 7))))

;;Game event loop
(loop while (<= (game-status g) 0)
      ;; print the board for the user
      do (print (game-status g))
      (setf (game-status g) 1))

(defun print-board (board)
  ;; TODO
  (dotimes (i 6)
    (dotimes (j 7)
     (print (aref (board) i j )))))

(defun get-options (board)
  ;; TODO
  (defparameter options '())
  (dotimes (c 5)
    (if (aref board c 6)
        (setq options (append options i))))
  options)

(defun player-move(options)
    ;; TODO
    ;;get their input/move option
   (interactive)
   (message "%s" (ido-completing-read "Choose move" 'options )))

(defun place_move(board choice piece):
  ;;TODO
  ;;place the piece in the board
  (dotimes (c 5)
    (if (not(aref board choice c))
      ;; set piece AND then break the loop--progn does both of these
      ;; statements  
      (progn (setq (aref board choice c) piece) 
        (return)))
    ))

(defun check_for_threes(state piece)
    ;; TODO
    ;; state should be a game-board instance
    ;; piece should be "X" or "O"
    ;; returns number of connect threes of piece in state
    (defparameter acc 0)
    (defparameter rows 6)
    (defparameter columns 7)
    ;; look for horizontals
    (dotimes (x rows)
    (dotimes (i (- rows 3))
    (if (string= (aref state x i) piece)
         (if (string= (aref state x (+ i 1)) piece)
               (if (string= (aref state x (+ i 2)) piece)
                  ;; a connect three is found, increment
                  (setq acc (+ acc 1))))))      
   ;; look for verticals
   (dotimes (x (- rows 2))
     (dotimes (i columns)
       (if (string= (aref state x i) piece)
          (if (string= (aref state (+ x 1) i) piece)
               (if (string= (aref state (+ x 2) i) piece)
                   ;; a connect three is found, increment
                   (setq acc (+ acc 1)))))))
   ;; look for diagonal '\' left to right
   (dotimes (x (- rows 2))
    (dotimes (i (- columns 2))
      (if (string= (aref state x i) piece)
          (if (string= (aref state (+ x 1) (+ i 1)) piece)
            (if (string= (aref state (+ x 2) (+ i 2)) piece)
              ;; a connect three is found, increment
              (setq acc (+ acc 1)))))))  
   ;; other diagonal '/' right to left
   (dotimes (x (- rows 2))
     (dotimes (i (- columns 2))
       (if (string= (aref state (- rows x) (- columns i)) piece)
         (if (string= (aref state (- rows (+ x 1)) (- columns (+ i 1))) piece)
           (if (string= (aref state (- rows (+ x 2)) (- columns (+ i 2))) piece)
             ;; a connect three is found, increment
             (setq acc (+ acc 1)))))))
  
    ;;return acc
  (acc))

(defun check-for-fours(state piece)
  ;;TODO
  ;;state should be a game-board state
  ;;piece should be "X" or "O"
  ;;returns number of connect-4s of piece in state
  (defparameter acc 0)
  (defparameter rows 6)
  (defparameter columns 7)
  ;; check horizontal
  (dotimes (x rows)
    (dotimes (i (- columns 4))
      (if (string= (aref state x i) piece)
          (if (string= (aref state x (+ i 1)) piece)
              (if (string= (aref state x (+ i 2)) piece)
                  (if (string= (aref state x (+ i 3)) piece)
                      ;; a connect four is found, increment acc
                      (setq acc (+ acc 1))))))))
  ;; check vertical
  (dotimes (x (- rows 3))
    (dotimes (i columns)
      (if (string= (aref state x i) piece)
        (if (string= (aref state (+ x  1) i) piece)
          (if (string= (aref state (+ x 2) i) piece)
            (if string= (aref state (+ x 3) i) piece)
              ;; a connect four is found, increment acc
              (setq acc (+ acc 1)))))))
  ;; check diagonal in "\" direction left to right
  (dotimes (x (- rows 3))
    (dotimes (i (- columns 3))
      (if (string= (aref state x i) piece)
        (if (string= (aref state (+ x 1) (+ i 1)) piece)
          (if (string= (aref state (+ x 2) (+ i 2)) piece)
            (if (string= (aref state (+ x 3) (+ i 3)) piece)
              ;; a connect four is found, increment acc
              (setq acc (+ acc 1))))))))
  ;; check diagonal in "/" direction right to left
  (dotimes (x (- rows 3))
    (dotimes (i (- columns 3))
      (if (string= (aref state (- rows (+ x 1)) (- columns (+ i 1))) piece)
        (if (string= (aref state (- rows (+ x 2)) (- columns (+ i 2))) piece)
          (if (string= (aref state (- rows (+ x 3)) (- columns (+ i 3))) piece)
            (if (string= (aref state (- rows (+ x 4)) (- columns (+ i 4))) piece)
              ;; a connect four is found, increment acc
              (setq acc (+ acc 1))))))))   
  ;; return acc-->how many connect fours of piece are in state      
  (acc)))

(defun value(state piece)
  ;; return a hueristice value for 'piece' player in given state
  (defparameter c3val (check-for-threes(state piece)))
  (defparameter c4val (check-for-fours(state piece)))
  ;; switch piece to get opponets hueristic value
  (if (string= piece "X")
      (defparameter other-piece "O")  ;; change to O
      (defparameter other-piece "X")) ;; else change to X
  (defparameter p3val (check-for-threes(state other-piece)))
  (defparameter p4val (check-for-threes(state other-piece)))
  ;; assign biases to each of these values
  ;; weight fours much higher than 3s, as they win the game
  ;; weight opponet moves more for now, since human player
  ;;     always goes first. This means it is more important 
  ;;     to stop the opponet from winning than to win.
  (defparemeter four-bias 1000)
  (defparameter three-bias 100)
  (defparameter defense-bias 100)
  (setq c3val (* c3val three-bias))
  (setq p3val (* p3val three-bias))
  (setq c4val (* c4val four-bias))
  (setq p4val (* p4val four-bias))
  (setq p3val (* p3val defense-bias))
  (setq p4val (* p4val defense-bias))
  ;; flip the opponet weights, they are 'bad' not 'good'
  ;; in relation to the value of state
  (setq p3val (- 0 p3val))
  (setq p4val (- 0 p4val))
  ;; sum all the hueristics for a combined value of the state 
  (defparameter value (+ c3val c4val p3val p4val))
  ;; return value
  (value))

(defun best-move(options state piece &depth 0)
  ;; determine best move by: 
  ;;     1. searching each option
  ;;     2. determining its value through a search of future states
  ;;            and getting those state heuristic value
  ;;     3. weight earlier states with a higher value
  ;;            as winning sooner is more valuable
  ;;     4. return the move with the highest combined heurisic value
  (if (= options ()) ;; no options left
      (return)) ;; break function return nothing
  (if (> depth 4)
    ;; limit depth of search to 4, just return first option
    (return-from options 0))
  ;; TODO: short-circuit logic for tactical moves
  ;;       such as three horizontal across the bottom
  ;;       these could a lot of time in searching
  (defparameter option-values ()) ;; empty list to be filled with value
  (dotimes (move (length options))
    ;; copy the board to avoid changing the original
    (defparameter future-board (state))
    (place-move(future-board move piece))
    ;; get value of potential move by searching its future states
    (defparameter value (search(future-board piece &depth (+ depth 1))))
    ;; append value to list
    (setq option-values (append option-values value)))
  ;; find the highest value in option-values 
  ;;     and return its corresponding move
  (defparameter highest -9999999)
  (defparameter index-highest -1)
  (dotimes (i (length option-values))
    (if (> (nth option-values i) highest)
      ;; option is higher value, assign it to best move
      (progn (setq highest (nth option-values i))
        (setq index-highest (nth options i)))))
  ;; return the highest value move
  (index-highest))

(defun search(state, piece, &depth 0)
  ;; search the future of this move and return its value 
  ;; until depth nodes of search
  ;; weight earlier moves as more valuable
  (defparameter move0val (* (value(state piece)) 1250))
  ;;place two moves at a time based on 'best move'
  (defparameter move1val 0)
  (defparameter move2val 0)
  (defparameter move3val 0)
  (defparameter move4val 0)
  (place-move(state (best-move(get-options(state) state "O" &depth depth) "0")))
  (place-move(state (best-move(get-options(state) state "X" &depth depth) "X")))
  (setq move1val (* (value(state piece)) 250))
  (place-move(state (best-move(get-options(state) state "O" &depth (+ depth 1)) "O")))
  (place-move(state (best-move(get-options(state) state "X" &depth (+ depth 1)) "X")))
  (setq move2val (* (value(state piece)) 50))
  (place-move(state (best-move(get-options(state) state "O" &depth (+ depth 2)) "O")))
  (place-move(state (best-move(get-options(state) state "X" &depth (+ depth 2)) "X")))
  (setq move3val (* (value(state piece)) 10))
  (place-move(state (best-move(get-options(state) state "O" &depth (+ depth 3)) "O")))
  (place-move(state (best-move(get-options(state) state "X" &depth (+ depth 3)) "X")))
  (setq move4val (* (value(state piece)) 2))
  
  ;; sum all the values and return the total heuristic
  (defparameter value (+ move1val move2val move3val move4val)))

(defun check-win(state piece)
  ;; return string "WIN" if the game has been won
  (if (> (check-for-fours(state piece)) 0)
      ("WIN")
    (return)))

;; game event loop
(loop while (< status 1)
    do ((print-board(game-board g))
        (setq their-choice player-move(get-options(game-board g)))
        (place-move((game-board g) their-choice "X"))
        (if (string= (check-win((game-board g) "X")) "WIN")
            (setq status 2)(print "YOU WON!")) ;;end game
        (if (> status 1)
          ;; break game loop and exit
          (return)
          ;; keep playing game
          (setq comp-choice best-move(get-options(game-board g) (game-board g)))
          (place-move((game-board g) comp-choice "O"))
          (if (string= (check-win((game-board g) "O")) "WIN")
              (setq status 2)(print "COMPUTER WON!"));;endgame
          )))

(print "END OF SCRIPT")


