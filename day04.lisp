(defpackage :day04
  (:use :cl :trivia :cl-ppcre)
  (:export main))

(in-package :day04)


(defun is-valid (num)
  (loop
    with repeat = 1
    with valid = nil
    with num-lst = (coerce (write-to-string num) 'list)
    for a = (first  num-lst)
    for b = (second num-lst)
    until (null num-lst)
    do (progn
         (cond
           ((or (null b) (char< a b))
            (cond
              ((= repeat 2) (setf valid 'both))
              ((> repeat 2) (unless valid (setf valid 'first-only))))
            (setf repeat 1))
           ((char= a b) (incf repeat))
           ((char> a b) (return-from is-valid nil)))
         (setf num-lst (rest num-lst)))
    finally (return valid)))


(defun main ()
  (match
    (with-open-file (stream "inputs/day04")
      (loop for num in (split "-" (read-line stream))
            collect (parse-integer num)))
    ((list begin end)
     (loop for n from begin to end
           for valid = (is-valid n)
           count valid into nvalid
           count (eq valid 'both) into nboth
           finally (format t "~a~%~a~%" nvalid nboth)))))
