(in-package :sand.turing-omnibus.minimax)

(declaim (optimize (safety 1) (debug 3) (speed 3)))
(declaim (optimize (safety 0) (debug 0) (speed 3)))


;;;; API ----------------------------------------------------------------------
(defgeneric initial-state (game)
  (:documentation "Return the initial state of the game."))

(defgeneric successor-states (game state)
  (:documentation "Return the successor states of the game."))

(defgeneric evaluate-state (game state role)
  (:documentation "Return the value of `state` for `role`."))

(defgeneric control (game state)
  (:documentation "Return the role in control at the given `state`."))


(defun minimax (game role)
  (recursively ((path (list (initial-state game))))
    (let* ((state (car path))
           (control (control game state))
           (successors (successor-states game state)))
      (if (null successors)
        (cons (evaluate-state game state role) path)
        (iterate (for successor :in successors)
                 (for next = (recur (cons successor path)))
                 (finding next maximizing #'car :into max)
                 (finding next minimizing #'car :into min)
                 (finally (return (if (eql role control) max min))))
        #+more-consing (extremum (-<> successors
                                   (mapcar (rcurry #'cons path) <>)
                                   (mapcar #'recur <>))
                                 (if (eql role control) #'> #'<)
                                 :key #'car)))))


;;;; Tic Tac Toe --------------------------------------------------------------
(defstruct (tic-tac-toe-state (:conc-name ttts-))
  board control)

(define-with-macro (tic-tac-toe-state :conc-name ttts)
  board control)


(defmethod initial-state ((game (eql 'tic-tac-toe)))
  (make-tic-tac-toe-state
    :board #2A((nil nil nil)
               (nil nil nil)
               (nil nil nil))
    :control 'x))


(defun ttt-all-role-p (board role indexes)
  (every (lambda (index)
           (eql role (apply #'aref board index)))
         indexes))

(defun ttt-line-p (board role)
  (or (ttt-all-role-p board role '((0 0) (0 1) (0 2))) ; horizontal
      (ttt-all-role-p board role '((1 0) (1 1) (1 2)))
      (ttt-all-role-p board role '((2 0) (2 1) (2 2)))
      (ttt-all-role-p board role '((0 0) (1 0) (2 0))) ; vertical
      (ttt-all-role-p board role '((0 1) (1 1) (2 1)))
      (ttt-all-role-p board role '((0 2) (1 2) (2 2)))
      (ttt-all-role-p board role '((0 0) (1 1) (2 2))) ; diagonals
      (ttt-all-role-p board role '((2 0) (1 1) (0 2)))))

(defun ttt-other-role (role)
  (ecase role
    (x 'o)
    (o 'x)))


(defmethod evaluate-state ((game (eql 'tic-tac-toe)) state role)
  (cond
    ((ttt-line-p (ttts-board state) role) 1)
    ((ttt-line-p (ttts-board state) (ttt-other-role role)) -1)
    (t 0)))

(defmethod successor-states ((game (eql 'tic-tac-toe)) state)
  (with-tic-tac-toe-state (state)
    (iterate
      (with other-role = (ttt-other-role control))
      (for (mark x y) :in-array board)
      (when (null mark)
        (let ((new-board (copy-array board)))
          (setf (aref new-board x y) control)
          (collect (make-tic-tac-toe-state :board new-board :control other-role)))))))

(defmethod control ((game (eql 'tic-tac-toe)) state)
  (ttts-control state))


;;;; Nim -----------------------------------------------------------------------
(defstruct (nim-state (:conc-name ns-))
  piles control)

(define-with-macro (nim-state :conc-name ns)
  piles control)

(defun nim-other-role (role)
  (ecase role
    (x 'o)
    (o 'x)))


(defmethod initial-state ((game (eql 'nim)))
  (make-nim-state
    :piles #(4 3 2 2)
    :control 'x))

(defmethod evaluate-state ((game (eql 'nim)) state role)
  (with-nim-state (state)
    (if (every #'zerop piles)
      (if (eql role control)
        1
        -1)
      0)))


(defun take-from-pile (piles index amount)
  (let ((piles (copy-array piles)))
    ; (declare (type (simple-array fixnum (*)) piles)
    ;          (type fixnum index amount))
    (decf (aref piles index) amount)
    piles))


(defmethod successor-states ((game (eql 'nim)) state)
  (with-nim-state (state)
    (iterate
      (with other-role = (nim-other-role control))
      (for (pile i) :in-array piles)
      (appending (iterate
                   (for take :from 1 :to pile)
                   (collect (make-nim-state :piles (take-from-pile piles i take)
                                            :control other-role)))))))

(defmethod control ((game (eql 'nim)) state)
  (ns-control state))

; (start-profiling)
; (stop-profiling)
