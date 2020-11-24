(defpackage :functional-queue
  (:use :cl)
  (:export :empty-queue
           :queue-empty-p
           :queue-head
           :queue-tail
           :queue-pop
           :queue-snoc))

(in-package :functional-queue)


(defun empty-queue () (cons nil nil))

(defun queue-empty-p (queue)
  (null (car queue))) 

(defun checkf (queue)
  (if (null (car queue))
    (cons (reverse (cdr queue)) nil)
    queue))

(defun queue-head (queue)
  (first (car queue)))

(defun queue-tail (queue)
  (checkf
    (cons (rest (car queue)) (cdr queue))))

(defun queue-snoc (queue x)
  (checkf
    (cons
      (car queue)
      (cons x (cdr queue)))))
