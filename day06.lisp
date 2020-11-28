(defpackage :day06
  (:use :cl :cl-ppcre :sycamore :trivia)
  (:import-from :fset
                :contains?
                :empty-map
                :empty-set
                :lookup
                :with)
  (:export main))

(in-package :day06)


(defun explore-graph (graph start-name end-name update-solution)
  (labels
    ((rec (queue visited solution)
       (multiple-value-bind (new-queue element) (amortized-dequeue queue)
         (let*
           ((depth (car element)) (name  (cdr element)))
           (if (string= name end-name)
             solution
             (rec
               (reduce
                 (lambda (q n) (amortized-enqueue q (cons (1+ depth) n)))
                 (remove-if (lambda (n) (contains? visited n)) (lookup graph name))
                 :initial-value new-queue)
               (with visited name)
               (funcall update-solution solution depth)))))))
    (rec (amortized-enqueue (make-amortized-queue) (cons 0 start-name)) (empty-set) 0)))

(defun populate-graph (graph input)
  (reduce
    (lambda (g i) (with g (first i) (cons (second i) (lookup g (first i)))))
    input :initial-value graph))

(defun read-input (stream)
  (match (read-line stream nil)
     (nil nil)
     (line (cons (split "\\)" line) (read-input stream)))))

(defun main ()
  (let*
    ((input
       (with-open-file (stream "inputs/day06")
         (read-input stream)))
     (directed-graph   (populate-graph (empty-map) input))
     (undirected-graph (populate-graph directed-graph (mapcar #'reverse input))))

    (format t "~a~%"     (explore-graph   directed-graph "COM"  nil   #'+))
    (format t "~a~%" (1- (explore-graph undirected-graph "YOU" "SAN" (lambda (_ d) d))))))

