
(defpackage :day18
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :alexandria :copy-array)
  (:import-from :fset :empty-map :with :lookup)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail :queue-empty-p)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min)
  (:import-from :trivia :match)
  (:import-from :forfuncs :for/fold)
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
      (for/fold
        ((q (queue-tail queue))
         (k keys))
        ((dir *all-absolute-dirs*))
        (let*
          ((new-coord (next-coord dir coord))
           (square (aref-coord maze-map new-coord)))
          (setf (aref-coord maze-map new-coord) nil)
          (match square
            (nil (values q k))
            ((cons :KEY found-key)
             (values
               (queue-snoc q (list (1+ steps) doors new-coord))
               (cons (list found-key (1+ steps) doors) k)))
            ((cons :DOOR found-door)
             (values (queue-snoc q (list (1+ steps) (logior found-door doors) new-coord)) k))
            (_ (values (queue-snoc q (list (1+ steps) doors new-coord)) k))))
        :result (explore-maze maze-map q k)))))

(defun compare (a b)
  (<
    (+ (first a) (fourth a))
    (+ (first b) (fourth b))))

(defun collect-keys (full-inventory matrix heap &optional (visited (empty-map)))
  (destructuring-bind (path-len current-key keys-inventory remkeys) (leftist-find-min heap)
    (if (= keys-inventory full-inventory)
      path-len
      (for/fold
        ((h (leftist-delete-min heap #'compare))
         (v visited))
        ((next-key (aref matrix current-key)))
        (destructuring-bind (num steps doors) next-key
          (if 
            (or
              (plusp (logand (aref bits num) keys-inventory)) ; already have the key
              (not (= doors (logand doors keys-inventory))))  ; not enough keys to open all doors
            (values h v)
            (let*
              ((new-keys-inventory (logior (aref bits num) keys-inventory))
               (visited-key (list num new-keys-inventory))
               (visited-steps (lookup v visited-key))
               (new-path-len (+ path-len steps)))
              ; a similar key/inventory situation has already been encountered with less steps
              (if (and visited-steps (< visited-steps new-path-len))
                (values h v)
                (values
                  (leftist-insert (list (+ path-len steps) num new-keys-inventory (- remkeys n-heur)) h #'compare)
                  (with v visited-key new-path-len))))))
        :result (collect-keys full-inventory matrix h v)))))

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

