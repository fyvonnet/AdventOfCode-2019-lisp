(defpackage :intcode
  (:use :cl :alexandria)
  (:export :duplicate-intstate
           :write-memory
           :read-memory
           :read-memory-from-ptr
           :init-memory
           :intstate-from-str
           :intstate-from-file
           :running-p
           :start-intstate))

(in-package :intcode)


(defstruct intstate
  (memory (make-hash-table))
  (pointer 0)
  (relbase 0)
  (input  nil)
  (output nil)
  (running t))

(defun duplicate-intstate (is)
  (let
    ((is-dup (copy-intstate is)))
    (setf (intstate-memory is-dup) (copy-hash-table (intstate-memory is)))
    (setf (intstate-input  is-dup) (copy-list (intstate-input  is)))
    (setf (intstate-output is-dup) (copy-list (intstate-output is)))
    is-dup))

(defun write-memory (is addr val)
  (setf (gethash addr (intstate-memory is)) val))

(defun read-memory (is addr)
  (gethash addr (intstate-memory is) 0))

(defun read-memory-from-ptr (is &optional (val 0))
  (read-memory is (+ (intstate-pointer is) val)))

(defun init-memory (is lst)
  (loop
    for addr from 0
    for value in lst
    do (write-memory is addr value)))

(defun intstate-from-str (str)
  (let 
    ((is (make-intstate)))
    (init-memory is (mapcar #'parse-integer (cl-ppcre:split "," str)))
    is))

(defun intstate-from-file (file)
  (with-open-file
    (stream file)
    (intstate-from-str (read-line stream nil))))

(defun inc-pointer (is in)
  (incf (intstate-pointer is) in))

(defun set-pointer (is val)
  (setf (intstate-pointer is) val))

(defun running-p (is)
  (intstate-running is))

(defun exec-instruction (is)
  (let
    ((oc nil) (vals  nil) (addrs nil))

    (multiple-value-bind
      (q r)
      (floor (read-memory-from-ptr is) 100)
      (setf oc r)
      (loop
        for i from 1
        for m across (reverse (format nil "~3,'0d" q))
        for v = (read-memory-from-ptr is i)
        for reladdr = (+ v (intstate-relbase is))
        collect (case m
                  (#\0 (read-memory is v))        ; position mode
                  (#\1 v)                         ; immediate mode
                  (#\2 (read-memory is reladdr))) ; relative mode
        into vs
        collect (if (eq m #\2) reladdr v) into as
        finally (progn (setf vals  vs) (setf addrs as))))

    ;(format t "~a ~a ~a ~a~%" oc raws modes vals)

    (case oc
      (1 ; addition
       (write-memory is (third addrs)
                     (+ (first vals) (second vals)))
       (inc-pointer is 4)
       t)
      (2 ; multiplication
       (write-memory is (third addrs)
                     (* (first vals) (second vals)))
       (inc-pointer is 4)
       t)
      (3 ; input
       (cond
         ((null (intstate-input is)) nil)
         (t
           (write-memory is (first addrs) (pop (intstate-input is)))
           (inc-pointer is 2)
           t)))
      (4 ; output
       ;(push (read-memory is (first addrs)) (intstate-output is))
       (push (first vals) (intstate-output is))
       (inc-pointer is 2)
       t)
      (5 ; jump-if-true
       (if (not (zerop (first vals)))
         (set-pointer is (second vals))
         (inc-pointer is 3))
       t)
      (6 ; jump-if-false
       (if (zerop (first vals))
         (set-pointer is (second vals))
         (inc-pointer is 3))
       t)
      (7 ; less-than
       (write-memory is (third addrs)
                     (if (< (first vals) (second vals)) 1 0))
       (inc-pointer is 4)
       t)

      (8 ; equals
       (write-memory is (third addrs)
                     (if (= (first vals) (second vals)) 1 0))
       (inc-pointer is 4)
       t)
      (9 ; adjust relative base
       (incf (intstate-relbase is) (first vals))
       (inc-pointer is 2)
       t)
      (99 ; exit
       (setf (intstate-running is) nil)
       nil)
      (otherwise
        (error (format nil "wrong opcode received: ~s~%" oc))))))

(defun start-intstate (is input)
  (setf (intstate-input is) input)
  (loop while (exec-instruction is))
  (let ((output (intstate-output is)))
    (setf (intstate-output is) nil)
    (reverse output)))
