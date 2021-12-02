
; crashes with 16-keys example and input

(defpackage :day18
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :functional-queue :empty-queue :queue-snoc :queue-head :queue-tail :queue-empty-p)
  (:import-from :leftist-heap :leftist-insert :leftist-find-min :leftist-delete-min)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:export main))

(in-package :day18)

(defun explore-maze (maze-map matrix start)
  (destructuring-bind (origin-key . start-coord) start
    (let ((unexplored (make-array (array-dimensions maze-map) :initial-element t)))
      (setf (aref-coord unexplored start-coord) nil)
      (nlet rec ((queue (queue-snoc (empty-queue) (list 0 nil start-coord))))
        (unless (queue-empty-p queue)
          (rec
            (destructuring-bind (steps doors coord) (queue-head queue)
              (reduce
                (lambda (q d)
                  (let*
                    ((new-coord (next-coord d coord))
                     (square (aref-coord maze-map new-coord)))
                    (if (and square (aref-coord unexplored new-coord))
                      (progn
                        (setf (aref-coord unexplored new-coord) nil)
                        (queue-snoc
                          q
                          (cons
                            (1+ steps)
                            (match square
                              ((cons :KEY found-key)
                               (setf
                                 (aref matrix origin-key found-key)
                                 (list (1+ steps) doors))
                               (list doors new-coord))
                              ((cons :DOOR found-door)
                               (list (cons found-door doors) new-coord))
                              (_ (list doors new-coord))))))
                      q)))
                *all-absolute-dirs*
                :initial-value (queue-tail queue)))))))))

(defun compare (a b)
  (< (third a) (third b)))

(defun collect-keys (matrix heap)
  (destructuring-bind (current-key keys-inventory path-len) (leftist-find-min heap)
    (if (= (length keys-inventory) (array-dimension matrix 1))
      (cons
        path-len
        (map 'string (lambda (x) (code-char (+ 97 x))) (reverse keys-inventory)))
      (collect-keys
        matrix
        (nlet rec ((key 0) (h (leftist-delete-min heap #'compare)))
          (if (= key (array-dimension matrix 1))
            h
            (rec
              (1+ key)
              (let ((key-data (aref matrix current-key key)))
                (if (or (member key keys-inventory) (null key-data))
                  h
                  (destructuring-bind (steps doors) key-data
                    (if (null (set-difference doors keys-inventory))
                      (leftist-insert (list key (cons key keys-inventory) (+ path-len steps)) h #'compare)
                      h)))))))))))

(defun main ()
  (let
     ((maze-map (read-input-as-array 18))
     (keys)
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
             (push (cons n coord) keys)
             (setf (aref-coord maze-map coord) (cons :KEY n))
             (incf nkeys)))
          ((upper-case-p c)
           (let ((n (- (char-code c) (char-code #\A))))
             (setf (aref-coord maze-map coord) (cons :DOOR n)))))))

    (setf matrix (make-array (list (1+ nkeys) nkeys) :initial-element nil))
    (dolist (x (cons `(,nkeys . ,entrance) keys))
      (explore-maze maze-map matrix x))
    (print (collect-keys matrix (leftist-insert (list nkeys nil 0) nil #'compare)))))

