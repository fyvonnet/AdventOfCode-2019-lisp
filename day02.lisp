(defpackage :day02
  (:use :cl :intcode)
  (:export main))

(in-package :day02)


(defun run-prog (prog noun verb)
  (let ((is (intstate-from-str prog)))
    (write-memory is 1 noun)
    (write-memory is 2 verb)
    (start-intstate is nil)
    (read-memory is 0)))

(defun main ()
  (let 
    ((prog
       (with-open-file (stream "inputs/day02")
         (read-line stream))))

    (format t "~a~%" (run-prog prog 12 2))

    (block blk
           (dotimes (noun 100)
             (dotimes (verb 100)
               (when (eq (run-prog prog noun verb) 19690720)
                 (format t "~a~%" (+ (* 100 noun) verb))
                 (return-from blk)))))))
