(defpackage :day06
  (:use :cl :cl-ppcre :queues)
  (:export main))

(in-package :day06)


(defparameter *graph* (make-hash-table :test 'equal))


(defun explore-graph (init-name init-depth test-name set-solution)
  (loop
    with queue = (make-queue :simple-queue)
    with solution = 0
    with visited = (make-hash-table :test 'equal)
    initially (progn
                (qpush queue init-name)
                (qpush queue init-depth))
    for name  = (qpop queue)
    for depth = (qpop queue)
    until (funcall test-name name)
    do (progn
         (setf (gethash name visited) t)
         (setf solution (funcall set-solution solution depth))
         (dolist (n (gethash name *graph*))
           (unless (gethash n visited)
             (qpush queue n)
             (qpush queue (1+ depth)))))
    finally (format t "~a~%" solution)))

(defun populate-graph (input key-func val-func)
  (dolist (orbit input)
    (let ((lst (gethash (funcall key-func orbit) *graph*)))
      (setf
        (gethash (funcall key-func orbit) *graph*)
        (cons (funcall val-func orbit) lst)))))

(defun main ()
  (let
    ((input
       (with-open-file (stream "inputs/day06")
         (loop
           for line = (read-line stream nil)
           while line
           collect (split "\\)" line)))))

    (populate-graph input #'first #'second)
    (explore-graph "COM"  0 (lambda (n) (null n)) #'+)
    (populate-graph input #'second #'first)
    (explore-graph "YOU" -1 (lambda (n) (string= n "SAN")) (lambda (s d) d))))
