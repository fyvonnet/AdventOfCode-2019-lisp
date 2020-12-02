(defpackage :day20
  (:use :cl :aoc-coord :aoc-misc :trivia :leftist-heap)
  (:export main)
  (:import-from :fset
                :empty-map
                :lookup
                :map-intersection
                :convert))

(in-package :day20)


(defparameter *maze-map* nil)


(defun decode (c)
  (case c
    (#\. 0)
    (#\Space 'void)
    (#\# 'wall)
    (otherwise c)))

(defun getsquare (coordinates)
  (aref *maze-map* (get-y coordinates) (get-x coordinates)))

(defun setsquare (coordinates value)
  (setf (aref *maze-map* (get-y coordinates) (get-x coordinates)) value))

(defun walk-perimeter (start-coord end-side-func label-turn reverse-dirs)
  (labels
    ((rec (prev-coord direction gates-map)
       (let*
         ((coord (next-coord direction prev-coord))
          (square (getsquare coord)))
         (cond
           ((eq 'wall square)
             (if (funcall end-side-func coord direction)
               (if (coord= coord start-coord)
                 gates-map
                 (rec coord (turn 'right direction) gates-map))
               (rec coord direction gates-map)))
           ((numberp square)
             (let*
               ((label-dir (turn label-turn direction))
                (entrance-coord (next-coord label-dir coord))
                (gate-name
                  (let 
                    ((letters (make-array 2))
                     (read-coord entrance-coord))
                    (dolist (i (if (find direction reverse-dirs) '(0 1) '(1 0)))
                      (setf (aref letters i) (getsquare read-coord))
                      (setf read-coord (next-coord label-dir read-coord)))
                    (coerce letters 'string))))
               (rec
                 (next-coord direction coord)
                 direction
                 (fset:with gates-map gate-name (list :entry entrance-coord :exit coord)))))))))
    (rec start-coord 'east (empty-map))))

(defun find-inner-corner (&optional (coord (make-coord 2 2)))
  (when (not (eq 'void (getsquare coord)))
    (let ((result (find-inner-corner (coord+ coord (make-coord 1 1)))))
      (if result result coord))))

(defun add-tunnels (outer-gate inner-gate)
  (when inner-gate
    (setsquare (getf outer-gate :entry) (cons 'outer (getf inner-gate :exit)))
    (setsquare (getf inner-gate :entry) (cons 'inner (getf outer-gate :exit)))))

(defun compare (elm1 elm2)
  (cond
    ((< (first elm1) (first elm2)) t)
    ((> (first elm1) (first elm2)) nil)
    (t (< (third elm1) (third elm2)))))


(defun mark-blocked (coordinate mask)
  (setsquare coordinate (logior (getsquare coordinate) mask)))

(defun make-enqueue-func (inner-func outer-func)
  (lambda (distance mask)
    (lambda (queue coord)
      (labels
        ((enqueue-if-unexplored (m c)
           (if (zerop (logand (getsquare c) m))
             (leftist-insert (list m c (1+ distance)) queue #'compare)
             queue)))
        (let ((square (getsquare coord)))
          (cond
            ((numberp square) (enqueue-if-unexplored mask coord))
            ((consp square)
             (enqueue-if-unexplored
               (funcall (if (eq 'inner (car square)) inner-func outer-func) mask)
               (cdr square)))
            (t queue)))))))

(defun explore-maze (enqueue-func start-coord end-coord)
  (labels
    ((rec (queue)
       (match (leftist-find-min queue)
         ((list mask coord distance)
            (mark-blocked coord mask)
            (if (coord= coord end-coord)
              distance
              (rec
                (reduce
                  (funcall enqueue-func distance mask)
                  (mapcar (lambda (d) (next-coord d coord)) *all-absolute-dirs*)
                  :initial-value (leftist-delete-min queue #'compare)))))
         (nil (error "queue empty"))
         (x (error (format nil "wrong dequeue: ~a~%" x))))))
    (rec (leftist-insert (list 1 start-coord 0) nil #'compare))))


(defun main ()
  (let*
    ((input (read-input-as-list 20 (lambda (line) (map 'vector #'decode line))))
     (maze-height (length input))
     (maze-width  (length (car input))))

    (setf *maze-map* (make-array (list maze-height maze-width) :initial-contents input))

    (let*
      ((gates-map-outer
         (walk-perimeter
           (make-coord 2 2)
           (lambda (c d) (eq 'void (getsquare (next-coord d c)))) 'left '(south west)))
       (gates-map-inner
         (walk-perimeter
           (find-inner-corner)
           (lambda (c d) (eq 'wall (getsquare (next-coord (turn 'right d) c)))) 'right '(north east)))
       (start-gate (lookup gates-map-outer "AA"))
       (start-coord (getf start-gate  :exit))
       (end-coord   (getf (lookup gates-map-outer "ZZ") :exit )))

      (map-intersection gates-map-outer gates-map-inner #'add-tunnels)


      ; part 1 ;

      (princ (explore-maze (make-enqueue-func #'identity #'identity) start-coord end-coord))
      (terpri)


      ; part 2 ;

      ; reset maze
      (dotimes (y maze-height)
        (dotimes (x maze-width)
          (let
            ((coord (make-coord x y)))
            (when (numberp (getsquare coord)) (setsquare coord 0)))))

      ; outer gates are closed at level 0
      (dolist (name (mapcar 'first (convert 'list gates-map-inner)))
        (mark-blocked (getf (lookup gates-map-outer name) :exit) 1))

      ; exit gate is closed at all levels except level 0
      (setsquare end-coord (lognot 1))

      (princ
        (explore-maze (make-enqueue-func (lambda (m) (* m 2)) (lambda (m) (/ m 2))) start-coord end-coord))
      (terpri))))
