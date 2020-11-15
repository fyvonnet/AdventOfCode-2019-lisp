(defpackage :day06
  (:use :cl :cl-ppcre :queues)
  (:import-from :fset
                :contains?
                :empty-map
                :empty-set
                :lookup
                :with)
  (:export main))

(in-package :day06)


(defun explore-graph (graph init-name init-depth test-name set-solution)
  (loop
    with queue = (make-queue :simple-queue)
    with solution = 0
    with visited = (empty-set)
    initially (progn
                (qpush queue init-name)
                (qpush queue init-depth))
    for name  = (qpop queue)
    for depth = (qpop queue)
    until (funcall test-name name)
    do (progn
         (setf visited (with visited name))
         (setf solution (funcall set-solution solution depth))
         (dolist (n (lookup graph name))
           (unless (contains? visited n)
             (qpush queue n)
             (qpush queue (1+ depth)))))
    finally (format t "~a~%" solution)))

(defun populate-graph (graph input)
  (reduce
    (lambda (g i) (with g (first i) (cons (second i) (lookup g (first i)))))
    input
    :initial-value graph))

(defun main ()
  (let*
    ((input
       (with-open-file (stream "inputs/day06")
         (loop
           for line = (read-line stream nil)
           while line
           collect (split "\\)" line))))
     (directed-graph   (populate-graph (empty-map) input))
     (undirected-graph (populate-graph directed-graph (mapcar #'reverse input))))

    (explore-graph directed-graph   "COM"  0 (lambda (n) (null n)) #'+)
    (explore-graph undirected-graph "YOU" -1 (lambda (n) (string= n "SAN")) (lambda (_ d) d))))
