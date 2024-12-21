(defpackage 2024-day-21
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-21)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-21.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

;; +---+---+---+
;; | 7 | 8 | 9 |
;; +---+---+---+
;; | 4 | 5 | 6 |
;; +---+---+---+
;; | 1 | 2 | 3 |
;; +---+---+---+
;;     | 0 | A |
;;     +---+---+


(defun coord-of (x)
  (case x
    (#\7 (complex 0 0))
    (#\8 (complex 1 0))
    (#\9 (complex 2 0))
    (#\4 (complex 0 1))
    (#\5 (complex 1 1))
    (#\6 (complex 2 1))
    (#\1 (complex 0 2))
    (#\2 (complex 1 2))
    (#\3 (complex 2 2))
    (#\0 (complex 1 3))
    (#\A (complex 2 3))))

(defun in-bounds-keypad (c)
  (bind ((x (realpart c))
         (y (imagpart c)))
    (and (>= x 0)
         (>= y 0)
         (< x 3)
         (< y 4)
         (not (and (= x 0)
                   (= y 3))))))

(defun ways-from-to (a b)
  (bind ((xs (queues:make-queue :simple-queue))
         (ways nil)
         (seen (make-hash-table :test #'equal))
         (best most-positive-fixnum)
         (start (coord-of a))
         (end (coord-of b))
         (directions (list (cons #c(1 0) #\>)
                           (cons #c(-1 0) #\<)
                           (cons #c(0 1) #\v)
                           (cons #c(0 -1) #\^))))
    (queues:qpush xs (list nil 0 (coord-of a)))
    (setf (gethash start seen) 0)
    (iter
      (for x = (queues:qpop xs))
      (while x)
      (bind (((path dist c) x))
        (when (<= dist best)
          (if (and (<= dist best)
                   (= c end))
              (progn
                (setf best dist)
                (push (reverse path) ways))
              (iter
                (for (d . m) in directions)
                (for nc = (+ c d))
                (for n-dist = (1+ dist))
                (when (or (not (in-bounds-keypad nc))
                          (> n-dist #1=(gethash nc seen most-positive-fixnum)))
                  (next-iteration))
                (for n-path = (cons m path))
                (setf #1# n-dist)
                (queues:qpush xs (list n-path n-dist nc))))))
      (finally (return ways)))))

(defun shortest-pad-1 (code)
  (labels ((recur (i x y)
             (bind ((c (aref code i)))
               ())))))

(defun shortest-keys (code)
  ())

(defun part-1 ()
  (->> (read-problem)
    (mapcar #'shortest-keys)
    (apply #'+)))
