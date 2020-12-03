(defpackage :day13
  (:use :cl :aoc-misc :intcode :trivia)
  (:export main))

(in-package :day13)


(defun make-tiles (lst)
  (when lst
    (cons
      (list :x  (first  lst)
            :y  (second lst)
            :id (third  lst))
      (make-tiles (cdddr lst)))))

(defun main ()
  (let*
    ((is (intstate-from-file "inputs/day13"))
     (lst (start-intstate is nil)))
    (print (count-valid (lambda (e) (= 2 (getf e :id))) (make-tiles lst)))))
