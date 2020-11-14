(defpackage :day04
  (:use :cl :trivia :cl-ppcre)
  (:export main))

(in-package :day04)


(defun digit-list (n)
  (reverse 
    (loop
      until (zerop n)
      collect (rem n 10)
      do (setf n (floor n 10)))))

(defun is-valid (n)
  (loop with digits = (digit-list n)
        with valid = nil
        with previous-digit   = -1
        with digit-repeats   = 1
        for current-digit = (first digits)
        do (cond
             ((or (null digits) (< previous-digit current-digit))
              (cond
                ((= digit-repeats 2) (setf valid 'both))
                ((> digit-repeats 2) (unless valid (setf valid 'first-only))))
              (setf digit-repeats 1))
             ((> previous-digit current-digit) (return-from is-valid nil))
             ((= previous-digit current-digit) (incf digit-repeats)))
        until (null digits)
        do (pop digits)
        do (setf previous-digit current-digit)
        finally (return-from is-valid valid)))

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
