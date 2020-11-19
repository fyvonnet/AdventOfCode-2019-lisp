(defpackage :day11
  (:use :cl :aoc-coord :intcode :iterate :trivia)
  (:export main))

(in-package :day11)


(defun run-robot (input)
  (let
    ((is (intstate-from-file "inputs/day11")))
    (iterate
      (with hash = (make-hash-table :test 'equal))
      (with coord = (make-coord 0 0))
      (with direction = 'north)
      (for output = (start-intstate is (list input)))
      (match output
             ((list out-color out-turn)
              (setf (gethash coord hash) out-color)
              (setf direction (turn (if (zerop out-turn) 'left 'right) direction))))
      (setf coord (next-coord direction coord))
      (setf input (gethash coord hash 0))
      (while (running-p is))
      (finally (return hash)))))

(defun main()
  (let*
    ((identifier (run-robot 1))
     (limits
       (get-coords-limits
         (iterate
           (for (coord _) in-hashtable identifier)
           (collect coord)))))
    (princ (hash-table-count (run-robot 0)))
    (terpri)

    (iterate
      (for y from (getf limits :y-min) to (getf limits :y-max))
      (iterate
        (for x from (getf limits :x-min) to (getf limits :x-max))
        (if (= 1 (gethash (make-coord x y) identifier 0))
          (princ "*")
          (princ " ")))
      (terpri))))
