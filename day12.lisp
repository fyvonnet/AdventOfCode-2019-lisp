(defpackage :day12
  (:use :cl :cl-ppcre :trivia)
  (:export main))

(in-package :day12)


(defun read-input (str &optional list)
  (match (read-line str nil)
     (nil (rotate (reverse list)))
     (line
       (read-input
         str
         (cons 
           (multiple-value-bind (_ matches)
             (scan-to-strings "^<x=(.*), y=(.*), z=(.*)>$" line)
             (declare (ignorable _))
             (map 'list #'parse-integer matches))
           list)))))

(defun rotate (matrix)
  (if (null (first matrix))
    nil
    (cons
      (mapcar #'first matrix)
      (rotate (mapcar #'rest matrix)))))

(defun compare-to-others (pos others-pos)
  (match others-pos
    (nil 0)
    ((cons o os)
       (+ (if (> pos o) -1 1)
          (compare-to-others pos os)))))

(defun apply-gravity (positions velocities &optional (all-pos positions))
  (match (cons positions velocities)
    ((cons nil nil) nil)
    ((cons (cons elt-p lst-p) (cons elt-v lst-v))
       (cons
         (+ elt-v (compare-to-others elt-p (remove elt-p all-pos)))
         (apply-gravity lst-p lst-v all-pos)))))

(defun apply-velocity (positions velocities)
  (mapcar #'+ positions velocities))

(defun time-step (positions velocities)
  (let*
    ((new-velocities (apply-gravity  positions     velocities))
     (new-positions  (apply-velocity positions new-velocities)))
    (values new-positions new-velocities)))

(defun time-step-in-all-dims (positions velocities &optional list-pos list-vel)
  (match (cons positions velocities)
    ((cons nil nil)
       (values (reverse list-pos) (reverse list-vel)))
    ((cons (cons elt-p lst-p) (cons elt-v lst-v))
       (multiple-value-bind (new-pos new-vel) (time-step elt-p elt-v)
         (time-step-in-all-dims lst-p lst-v (cons new-pos list-pos) (cons new-vel list-vel))))))

(defun run-steps (steps positions velocities)
  (if (zerop steps)
    (values positions velocities)
    (multiple-value-bind (new-pos new-vel) (time-step-in-all-dims positions velocities)
      (run-steps (1- steps) new-pos new-vel))))

(defun sum-abs (lst) (apply #'+ (mapcar #'abs lst)))

(defun total-energy-rec (positions velocities)
  (match (cons positions velocities)
    ((cons nil nil) 0)
    ((cons (cons elt-p lst-p) (cons elt-v lst-v))
       (+ (* (sum-abs elt-p) (sum-abs elt-v))
          (total-energy-rec lst-p lst-v)))))

(defun total-energy (positions velocities)
    (total-energy-rec (rotate positions) (rotate velocities)))


(defun until-repeat (positions velocities &optional (count 1) (ref (cons positions velocities)))
    (multiple-value-bind (new-pos new-vel) (time-step positions velocities)
      (if (equal ref (cons new-pos new-vel))
        (/ count 2)
        (until-repeat new-pos new-vel (1+ count) ref))))

(defun main ()
  (let
    ((positions (with-open-file (str "inputs/day12") (read-input str)))
     (velocities '((0 0 0 0) (0 0 0 0) (0 0 0 0))))

    (multiple-value-bind (new-pos new-vel) (run-steps 1000 positions velocities)
      (princ (total-energy new-pos new-vel)))
    (terpri)

    (princ (apply #'* (mapcar #'until-repeat positions velocities)))
    (terpri)))

