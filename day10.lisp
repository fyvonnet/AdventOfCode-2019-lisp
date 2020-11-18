(defpackage :day10
  (:use :cl :aoc-coord :iterate :trivia)
  (:export main))

(in-package :day10)


(defun angle (a b)
  (let ((x (- (get-x b) (get-x a)))
        (y (- (get-y b) (get-y a))))
    (- (atan x y))))

(defun distance (a b)
  (let
    ((x (- (get-x a) (get-x b)))
     (y (- (get-y a) (get-y b))))
    (sqrt (+ (* x x) (* y y)))))

(defun compare-distance-from (orig)
  (lambda (a b)
    (< (distance orig a) (distance orig b))))

(defun consume-lists (lists)
  (iterate
    (for l in lists)
    (for r = (rest l))
    (if r (collect r))))


(defun main ()
  (let*

    ((asteroids-coordinates 
       (with-open-file (stream "inputs/day10")
         (iterate
           (for line = (read-line stream nil))
           (for y from 0)
           (while line)
           (appending
             (iterate
               (for char in-vector line)
               (for x from 0)
               (if (char= #\# char) (collect (make-coord x y))))))))

     (best-asteroid
       (iterate
         (with max-count = 0)
         (with best-hash)
         (with best-candidate)
         (for candidate in asteroids-coordinates)
         (iterate
           (with hash = (make-hash-table))
           (for coord in (remove candidate asteroids-coordinates))
           (push coord (gethash (angle candidate coord) hash))
           (finally
             (let ((count (hash-table-count hash)))
               (when (> count max-count)
                 (setf max-count count)
                 (setf best-hash hash)
                 (setf best-candidate candidate)))))
         (finally (return (list :hash best-hash :coord best-candidate :count max-count)))))

     (sorted-coordinates
       (mapcar
         (lambda (l) (sort (rest l) (compare-distance-from (getf best-asteroid :coord))))
         (sort
           (iterate
             (for (key value) in-hashtable (getf best-asteroid :hash))
             (collect (cons key value)))
           (lambda (a b) (< (first a) (first b))))))

     (destruction-list
       (iterate
         (for lists first sorted-coordinates then (consume-lists lists))
         (while lists)
         (appending (mapcar #'first lists)))))


    (princ (getf best-asteroid :count))
    (terpri)
    (princ
       (let*
         ((coord (nth 199 destruction-list))
          (x (get-x coord))
          (y (get-y coord)))
         (+ (* 100 x) y)))))
