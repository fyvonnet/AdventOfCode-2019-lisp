(defpackage :day04
  (:use :cl :trivia :cl-ppcre)
  (:export main))

(in-package :day04)


(defun is-valid (n)
  (let*
    ((digits (format nil "~a " n))
     (valid nil)
     (previous-digit (aref digits 0))
     (digit-repeats 1))
    (when (= (length digits) 7)
      (loop
        for i from 1 to 6 do
        (let
          ((current-digit (aref digits i)))
          (cond
            ((or (= i 6) (char< previous-digit current-digit))
             (cond
               ((= digit-repeats 2) (setf valid 'both))
               ((> digit-repeats 2) (unless valid (setf valid t))))
             (setf digit-repeats 1))
            ((char> previous-digit current-digit) (return-from is-valid nil))
            ((char= previous-digit current-digit) (incf digit-repeats)))
          (setf previous-digit current-digit))))
    valid))

(defun main ()
  (match
    (with-open-file (stream "inputs/day04")
      (loop for num in (split "-" (read-line stream))
            collect (parse-integer num)))
    ((list begin end)
     (loop for n from begin to end
           for valid = (is-valid n)
           count valid into nvalid
           count (eq valid 'both) into ntwo
           finally (print nvalid)
           (print ntwo)))))
