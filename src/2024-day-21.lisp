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


(defun coord-of-keypad (x)
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

(defun ways-from-to-keypad (a b)
  (bind ((xs (queues:make-queue :simple-queue))
         (ways nil)
         (seen (make-hash-table :test #'equal))
         (best most-positive-fixnum)
         (start (coord-of-keypad a))
         (end (coord-of-keypad b))
         (directions (list (cons #c(1 0) #\>)
                           (cons #c(-1 0) #\<)
                           (cons #c(0 1) #\v)
                           (cons #c(0 -1) #\^))))
    (queues:qpush xs (list nil 0 (coord-of-keypad a)))
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

(defun turns-in-path (path)
  (iter
    (for a in path)
    (for b previous a initially #\x)
    (counting (not (char-equal a b)))))

(defun score-path (path)
  (if (= 0 (length path))
      0
      (turns-in-path path)))

(defun shortest-keypad (code)
  (bind ((result-paths (list nil)))
    (iter
      (for b in-string code)
      (for a previous b initially #\A)
      (for paths = (ways-from-to-keypad a b))
      (for scores = (mapcar #'score-path paths))
      (for lowest = (apply #'min scores))
      (for candidates = (iter
                          (for path in paths)
                          (for score in scores)
                          (when (= score lowest)
                            (collecting path))))
      (setf result-paths
            (iter outer
              (for candidate in candidates)
              ;; (format t "paths: ~a~%" paths)
              ;; (format t "candidate: ~a~%" candidate)
              (iter
                (for result-path in result-paths)
                (in outer (collecting (append result-path candidate (list #\A))))))))
    result-paths))

;;     +---+---+
;;     | ^ | A |
;; +---+---+---+
;; | < | v | > |
;; +---+---+---+


(defun coord-of-arrows (x)
  (case x
    (#\^ (complex 1 0))
    (#\< (complex 0 1))
    (#\v (complex 1 1))
    (#\> (complex 2 1))
    (#\A (complex 2 0))))

(defun in-bounds-arrows (c)
  (bind ((x (realpart c))
         (y (imagpart c)))
    (and (>= x 0)
         (>= y 0)
         (< x 3)
         (< y 2)
         (not (and (= x 0)
                   (= y 0))))))

(defun ways-from-to-arrows (a b)
  ;; (format t "(list a b): ~a~%" (list a b))
  (bind ((xs (queues:make-queue :simple-queue))
         (ways nil)
         (seen (make-hash-table :test #'equal))
         (best most-positive-fixnum)
         (start (coord-of-arrows a))
         (end (coord-of-arrows b))
         (directions (list (cons #c(1 0) #\>)
                           (cons #c(-1 0) #\<)
                           (cons #c(0 1) #\v)
                           (cons #c(0 -1) #\^))))
    (queues:qpush xs (list nil 0 (coord-of-arrows a)))
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
                (when (or (not (in-bounds-arrows nc))
                          (> n-dist #1=(gethash nc seen most-positive-fixnum)))
                  (next-iteration))
                (for n-path = (cons m path))
                (setf #1# n-dist)
                (queues:qpush xs (list n-path n-dist nc))))))
      (finally (return ways)))))

(defun shortest-arrows (moves start)
  (bind ((result-paths (list nil)))
    (iter
      (for b in moves)
      (for a previous b initially start)
      (for paths = (ways-from-to-arrows a b))
      (for scores = (mapcar #'score-path paths))
      (for lowest = (apply #'min scores))
      (for candidates = (iter
                          (for path in paths)
                          (for score in scores)
                          (when (= score lowest)
                            (collecting path))))
      (setf result-paths (iter outer
                           (for candidate in candidates)
                           ;; (format t "paths: ~a~%" paths)
                           ;; (format t "candidate: ~a~%" candidate)
                           (iter
                             (for result-path in result-paths)
                             (in outer (collecting (append result-path candidate (list #\A))))))))
    result-paths))

(defun shortest (code)
  (bind ((pad-sets (shortest-keypad code))
         (shortest-1 (mapcan #l(shortest-arrows %1 #\A) pad-sets))
         (shortest-2 (mapcan #l(shortest-arrows %1 #\A) shortest-1)))
    (->> (mapcar #'length shortest-2)
      (reduce #'min))))

(defun number-part (code)
  (->> (subseq code 0 (1- (length code)))
    read-from-string))

(defun complexity (code)
  (prog1
    ;; (format t "(shortest code): ~a~%" (shortest code))
    
    (* (number-part code)
       (shortest code))
    (print "tick")))

(defun part-1 ()
  (->> (read-problem)
    (mapcar #'complexity)
    (apply #'+)))

;; Too High: 157692

(defun shortest-2 (code)
  (bind ((pad-sets (shortest-keypad code))
         (res (mapcan #l(shortest-arrows %1 #\A) pad-sets))
         (m (reduce #'min (mapcar #'score-path res))))
    (iter
      (for i from 0 below 1)
      (setf res (mapcan #l(shortest-arrows %1 #\A) res))
      (format t "(length res): ~a~%" (length res))
      (setf m (reduce #'min (mapcar #'score-path res)))
      (setf res (remove-if #l(/= m (score-path %1)) res))
      (format t "(length res): ~a~%" (length res)))
    (->> (mapcar #'length res)
      (reduce #'min))))

(defun complexity-2 (code)
  (prog1
    (* (number-part code)
       (shortest-2 code))
    (format t "tick~%")))

(defun part-2 ()
  (->> (read-problem)
    (mapcar #'complexity-2)
    (apply #'+)))
