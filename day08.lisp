(defpackage :day08
  (:use :cl)
  (:export main))

(in-package :day08)


(defparameter *layer-width* 25)
(defparameter *layer-height* 6)
(defparameter *pixels-per-layer* (* *layer-width* *layer-height*))

(defun count-digits (layer)
  (loop
    with counters = (list :zero 0 :one 0 :two 0)
    for pixel in layer
    do (case pixel
         (#\0 (incf (getf counters :zero)))
         (#\1 (incf (getf counters :one)))
         (#\2 (incf (getf counters :two))))
    finally (return counters)))

(defun stack-layers (layer1 layer2)
  (loop
    for pixel1 in layer1
    for pixel2 in layer2
    collect (if (char= #\2 pixel1) pixel2 pixel1)))


(defun main ()
  (let*
    ((input
       (with-open-file (stream "inputs/day08")
         (coerce (read-line stream) 'list)))
     (layers
       (loop
         collect (loop for i below *pixels-per-layer* collect (pop input))
         while input))
     (picture (reduce #'stack-layers layers)))

         (loop
           with counters = (mapcar #'count-digits layers)
           with minimal = (first counters)
           for c in (rest counters)
           do (when (< (getf c :zero) (getf minimal :zero)) (setf minimal c))
           finally (format t "~a~%" (* (getf minimal :one) (getf minimal :two))))

         (dotimes (_ *layer-height*)
           (dotimes (_ *layer-width*)
             (case (pop picture)
               (#\0 (format t " "))
               (#\1 (format t "*"))))
           (terpri))))
