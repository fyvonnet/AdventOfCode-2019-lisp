
; crashes with 16-keys example and input

(defpackage :day18
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :alexandria :copy-array)
  (:import-from :fset :empty-set :with :contains?)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail :queue-empty-p)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day18)

(defun get-bits (&optional (v 1) (n 26))
  (unless (zerop n)
    (cons v (get-bits (* 2 v) (1- n)))))

(defvar bits (coerce (get-bits) 'vector))

(defun explore-maze (maze-map queue &optional keys)
  (if (queue-empty-p queue)
    (reverse keys)
    (destructuring-bind (steps doors coord) (queue-head queue)
      (destructuring-bind (new-queue new-keys)
        (reduce
          (lambda (data d)
            (destructuring-bind (q k) data
              (let*
                ((new-coord (next-coord d coord))
                 (square (aref-coord maze-map new-coord)))
                (setf (aref-coord maze-map new-coord) nil)
                (match square
                  (nil data)
                  ((cons :KEY found-key)
                   (list
                     (queue-snoc q (list (1+ steps) doors new-coord))
                     (cons (list found-key (1+ steps) doors) k)))
                  ((cons :DOOR found-door)
                   (list (queue-snoc q (list (1+ steps) (logior found-door doors) new-coord)) k))
                  (_ (list (queue-snoc q (list (1+ steps) doors new-coord)) k))))))
          *all-absolute-dirs*
          :initial-value (list (queue-tail queue) keys))
        (explore-maze maze-map new-queue new-keys)))))

(defun compare (a b)
  (< (first a) (first b)))

(defun collect-keys (full-inventory matrix heap &optional (visited (empty-set)))
  (destructuring-bind (path-len current-key keys-inventory) (leftist-find-min heap)
    (if (= keys-inventory full-inventory)
      path-len
      (destructuring-bind (new-heap new-visited)
        (reduce
          (lambda (data next-key-data)
            (destructuring-bind (num steps doors) next-key-data
              (if 
                (or
                  (not (= doors (logand doors keys-inventory)))  ; not enough keys to open all doors
                  (contains? visited (list num keys-inventory))) ; already encountered situation
                data
                (let ((new-keys-inventory (logior (aref bits num) keys-inventory)))
                  (destructuring-bind (h v) data
                    (list
                      (leftist-insert (list (+ path-len steps) num new-keys-inventory) h #'compare)
                      (with v (list num new-keys-inventory))))))))
          (aref matrix current-key)
          :initial-value (list (leftist-delete-min heap #'compare) visited))
        (collect-keys full-inventory matrix new-heap new-visited)))))

(defun main ()
  (let
    ((maze-map (read-input-as-array 18 #'identity))
     (keys-num)
     (keys-coord)
     (nkeys 0)
     (entrance)
     (matrix))

    (for-all-coords coord maze-map 
      (let ((c (aref-coord maze-map coord)))
        (cond
          ((char= #\@ c)
           (setf entrance coord))
          ((char= #\# c)
           (setf (aref-coord maze-map coord) nil))
          ((lower-case-p c)
           (let ((n (- (char-code c) (char-code #\a))))
             (push n     keys-num)
             (push coord keys-coord)
             (setf (aref-coord maze-map coord) (cons :KEY n))
             (incf nkeys)))
          ((upper-case-p c)
           (let ((n (- (char-code c) (char-code #\A))))
             (setf (aref-coord maze-map coord) (cons :DOOR (aref bits n))))))))

    (setf
      matrix
      (coerce
        (mapcar
          #'cdr
          (sort
            (mapcar
              (lambda (num coord)
                (let ((maze-map-copy (copy-array maze-map)))
                  (setf (aref-coord maze-map-copy coord) nil)
                  (cons num (explore-maze maze-map-copy (queue-snoc (empty-queue) (list 0 0 coord))))))
              (cons nkeys keys-num)
              (cons entrance keys-coord))
            (lambda (a b) (< (first a) (first b)))))
        'vector))

    (print (collect-keys (1- (aref bits nkeys)) matrix (leftist-insert (list 0 nkeys 0) nil #'compare)))))

