(defpackage :aoc-coord
  (:use :cl)
  (:export :make-coord
          :make-coord
          :get-x
          :get-y
          :*coord-origin*
          :+all-absolute-dirs+
          :coord+
          :coord-
          :coord=
          :coord<
          :coordp
          :next-coord
          :turn
          :manhattan-distance
          :manhattan-distance-from-origin
          :coord-angle
          :next-column
          :next-row
          :left :right :front :back
          :north :south :east :west))

(in-package :aoc-coord)


(defun make-coord (&optional (x 0) (y 0)) (cons x y))
(defun get-x (coord) (car coord))
(defun get-y (coord) (cdr coord))

(defvar *coord-origin* (make-coord 0 0))
(defconstant +all-absolute-dirs+ '(north east south west))

(defun coord+ (a b)
  (cons (+ (get-x a) (get-x b)) (+ (get-y a) (get-y b))))

(defun coord- (a b)
  (cons (- (get-x a) (get-x b)) (- (get-y a) (get-y b))))

(defun coord= (a b)
  (and (eq (get-x a) (get-x b)) (eq (get-y a) (get-y b))))

(defun coord< (a b)
  (cond
    ((< (get-y a) (get-y b)) t)
    ((> (get-y a) (get-y b)) nil)
    (t
      (cond
        ((< (get-x a) (get-x b)) t)
        (t nil)))))

(defun coordp (c)
  (and
    (consp c)
    (numberp (get-x c))
    (numberp (get-y c))))

(defun next-coord (dir c)
  (case dir
    (north (coord- c (make-coord 0 1)))
    (south (coord+ c (make-coord 0 1)))
    (west  (coord- c (make-coord 1 0)))
    (east  (coord+ c (make-coord 1 0)))))

(defun turn (reldir absdir)
  (ccase reldir
    (left
      (case absdir
        (north 'west )
        (west  'south)
        (south 'east )
        (east  'north)))
    (right
      (case absdir
        (north 'east )
        (east  'south)
        (south 'west )
        (west  'north)))
    (back
      (case absdir
        (north 'south)
        (south 'north)
        (east  'west )
        (west  'east )))
    (front absdir)))

(defun manhattan-distance (p q)
  (+ (abs (- (get-x p) (get-x q))) (abs (- (get-y p) (get-y q)))))

(defun manhattan-distance-from-origin (p)
  (manhattan-distance *coord-origin* p))

(defun coord-angle (crd)
  (atan (cdr crd) (car crd)))

(defun next-column (crd) (cons (1+ (get-x crd)) (get-y crd)))
(defun next-row    (crd) (cons 0 (1+ (get-y crd))))
