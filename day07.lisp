(defpackage :day07
  (:use :cl :intcode)
  (:export main))

(in-package :day07)



(defun bootstrap-amplifiers (prog modes)
  (loop
    for mode in modes
    for is = (intstate-from-str prog)
    do (start-intstate is (list mode))
    collect is))

(defun run-amplifiers-loop (amplifiers)
  (loop
    with input = (list 0)
    with output
    with running
    do (loop
         for amp in amplifiers
         do (setf output (start-intstate amp input))
         do (setf input output)
         finally (setf running (running-p amp)))
    while running
    finally (return (car output))))

(defun permutations (list)
  (if list
    (mapcan
      (lambda (x)
        (mapcar 
          (lambda (y) (cons x y))
          (permutations (remove x list))))
      list)
    (list nil)))

(defun find-max-output (prog modes)
  (reduce
    #'max
    (mapcar
      (lambda (p) (run-amplifiers-loop (bootstrap-amplifiers prog p)))
      (permutations modes))))

(defun main ()
  (let*
    ((prog
       (with-open-file (stream "inputs/day07")
         (read-line stream))))
    (loop
      for settings in (list '(0 1 2 3 4) '(5 6 7 8 9))
      do (format t "~a~%" (find-max-output prog settings)))))
