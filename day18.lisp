
(defpackage :day18
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :alexandria :copy-array)
  (:import-from :fset :empty-map :with :lookup)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail :queue-empty-p)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day18)

(defun get-bits (&optional (v 1) (n 27))
  (unless (zerop n)
    (cons v (get-bits (* 2 v) (1- n)))))

(defvar bits (coerce (get-bits) 'vector))

; found through trial and error
; needs to be modified for example inputs
(defvar n-heur 200)

(defun explore-maze (maze-map queue &optional keys)
  (if (queue-empty-p queue)
    (reverse keys)
    (destructuring-bind (steps doors coord) (queue-head queue)
      (multiple-value-bind (new-queue new-keys)
        (nlet rec ((dirs *all-absolute-dirs*) (q (queue-tail queue)) (k keys))
          (if (null dirs)
            (values q k)
            (destructuring-bind (d &rest rest) dirs
              (let*
                ((new-coord (next-coord d coord))
                 (square (aref-coord maze-map new-coord)))
                (setf (aref-coord maze-map new-coord) nil)
                (match square
                  (nil (rec rest q k))
                  ((cons :KEY found-key)
                   (rec
                     rest
                     (queue-snoc q (list (1+ steps) doors new-coord))
                     (cons (list found-key (1+ steps) doors) k)))
                  ((cons :DOOR found-door)
                   (rec rest (queue-snoc q (list (1+ steps) (logior found-door doors) new-coord)) k))
                  (_ (rec rest (queue-snoc q (list (1+ steps) doors new-coord)) k)))))))
        (explore-maze maze-map new-queue new-keys)))))

(defun compare (a b)
  (<
    (+ (first a) (fourth a))
    (+ (first b) (fourth b))))

(defun collect-keys (full-inventory matrix heap &optional (visited (empty-map)))
  (destructuring-bind (path-len current-key keys-inventory remkeys) (leftist-find-min heap)
    (if (= keys-inventory full-inventory)
      path-len
      (multiple-value-bind (new-heap new-visited)
        (nlet rec ((next-keys (aref matrix current-key)) (h (leftist-delete-min heap #'compare)) (v visited))
          (if (null next-keys)
            (values h v)
            (destructuring-bind (num steps doors) (car next-keys)
              (if 
                (or
                  (plusp (logand (aref bits num) keys-inventory)) ; already have the key
                  (not (= doors (logand doors keys-inventory))))  ; not enough keys to open all doors
                (rec (cdr next-keys) h v)
                (let*
                  ((new-keys-inventory (logior (aref bits num) keys-inventory))
                   (visited-key (list num new-keys-inventory))
                   (visited-steps (lookup v visited-key))
                   (new-path-len (+ path-len steps)))
                  ; a similar key/inventory situation has already been encountered with less steps
                  (if (and visited-steps (< visited-steps new-path-len))
                    (rec (cdr next-keys) h v)
                    (rec
                      (cdr next-keys)
                      (leftist-insert (list (+ path-len steps) num new-keys-inventory (- remkeys n-heur)) h #'compare)
                      (with v visited-key new-path-len))))))))
        (collect-keys full-inventory matrix new-heap new-visited)))))

(defun main ()
  (let
    ((maze-map (read-input-as-array 18))
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

    (print (collect-keys (1- (aref bits nkeys)) matrix (leftist-insert (list 0 nkeys 0 (* 0 nkeys)) nil #'compare)))))

