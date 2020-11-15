(defpackage :day05
  (:use :cl :intcode)
  (:export main))

(in-package :day05)


(defun run-diagnostic (code)
  (let
    ((is (intstate-from-file "inputs/day05")))
    (car (last (start-intstate is (list code))))))

(defun main () (dolist (i '(1 5)) (format t "~a~%" (run-diagnostic i))))
