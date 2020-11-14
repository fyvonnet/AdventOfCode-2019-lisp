(defpackage :day03
  (:use :cl :aoc-coord :cl-ppcre)
  (:export main))

(in-package :day03)


(defun decode-line (line)
  (loop
    for str-seg in (split "," line)
    collect (multiple-value-bind (_ matches)
              (scan-to-strings "^(.)(.*)$" str-seg)
              (declare (ignorable _))
              (cons
                (parse-integer (aref matches 1))
                (case (aref (aref matches 0) 0)
                  (#\U 'north)
                  (#\D 'south)
                  (#\L 'west )
                  (#\R 'east ))))))

(defun make-table (line)
  (loop
    for seg in (decode-line line)
    for len   = (car seg)
    for direc = (cdr seg)
    with steps = 0
    with coord = *coord-origin*
    with table = (make-hash-table :test 'equal)
    do (loop
         repeat len
         do (incf steps)
         do (setf coord (next-coord direc coord))
         do (setf (gethash coord table) steps))
    finally (return table)))

(defun get-intersections (tables)
  (loop
    for coordinate being the hash-keys of (first tables)
    for steps  = (gethash coordinate (first  tables))
    for steps2 = (gethash coordinate (second tables))
    when steps2 minimize (manhattan-distance-from-origin coordinate) into shortest-manhattan-distance
    when steps2 minimize (+ steps steps2) into fewest-combined-steps
    finally (format t "~a~%~a~%" shortest-manhattan-distance fewest-combined-steps)))

(defun main ()
  (with-open-file
    (stream "inputs/day03")
    (loop
      for line = (read-line stream nil)
      while line
      collect (make-table line) into tables
      finally (get-intersections tables))))
