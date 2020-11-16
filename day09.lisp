(load "intcode.lisp")

(defpackage :day09
  (:use :cl :intcode)
  (:export main))

(in-package :day09)


(defun main ()
  (loop
    for i in '(1 2)
    do (format t "~a~%"
               (first (start-intstate (intstate-from-file "inputs/day09") (list i))))))
