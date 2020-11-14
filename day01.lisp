(defpackage :day01
  (:use :cl)
  (:export main))

(in-package :day01)


(defun fuelreq (mass)
  (- (floor (/ mass 3)) 2))

(defun morefuel (mass)
  (loop
    for fuel = mass then (fuelreq fuel)
    while (plusp fuel)
    sum fuel))

(defun main ()
  (with-open-file (stream "inputs/day01")
    (loop
      for mass = (read stream nil)
      while mass
      for fuel = (fuelreq mass)
      sum fuel into sum-fuel
      sum (morefuel fuel) into sum-morefuel
      finally (format t "~a~%~a~%" sum-fuel sum-morefuel))))
