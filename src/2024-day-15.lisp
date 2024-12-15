(defpackage 2024-day-15
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-15)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-15.in"))
    (labels ((read-instructions ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (read-instructions)))))
             (read-grid (grid-lines)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((equal line "") (cons (map 'vector #'identity (nreverse grid-lines))
                                          (apply #'concatenate 'string (read-instructions))))
                   (t (read-grid (cons line grid-lines)))))))
      (read-grid nil))))

(defun find-start (grid)
  (iter outer
    (for y from 0 below (length grid))
    (for row in-vector grid)
    (iter
      (for x from 0 below (length row))
      (for c in-string row)
      (when (char-equal c #\@)
        (return-from outer (complex x y))))))

(defun sum-box-gps (grid)
  (iter outer
    (for y from 0 below (length grid))
    (for row in-vector grid)
    (iter
      (for x from 0 below (length row))
      (for c in-string row)
      (when (or (char-equal c #\O)
                (char-equal c #\[))
        (in outer (summing (+ (* 100 y) x)))))))

(defun print-grid (grid)
  (fresh-line)
  (iter
    (for y from 0 below (length grid))
    (for row in-vector grid)
    (iter
      (for x from 0 below (length row))
      (for c in-string row)
      (princ c))
    (fresh-line)))

(defun part-1 ()
  (bind (((grid . instructions) (read-problem))
         (c (find-start grid)))
    (labels ((grid-aref (coord)
               (aref (aref grid (imagpart coord))
                     (realpart coord))))
      (iter
        ;; (print-grid grid)
        (for instruction in-string instructions)
        (for d = (case instruction
                   (#\> #c(1 0))
                   (#\^ #c(0 -1))
                   (#\v #c(0 1))
                   (#\< #c(-1 0))))
        (for new-c = (+ c d))
        (for end-c = new-c)
        (for s = (grid-aref new-c))
        (for s-end = s)
        (for ss = nil)
        (iter
          (while (char-equal s-end #\O))
          (push s-end ss)
          (incf end-c d)
          (setf s-end (grid-aref end-c)))
        (format t "instruction: ~a~%" instruction)
        (format t "c: ~a~%" c)
        (format t "d: ~a~%" d)
        (format t "(list end-c new-c): ~a~%" (list end-c new-c))
        (format t "ss: ~a~%" ss)
        (when (and (or (/= end-c new-c)
                       (char-equal s #\.))
                   (not (char-equal s-end #\#)))
          (setf (aref (aref grid (imagpart c))
                      (realpart c))
                #\.)
          (setf (aref (aref grid (imagpart new-c))
                      (realpart new-c))
                #\@)
          (setf c new-c)
          (iter
            (for i from 0 below (abs (- c end-c)))
            (with cur-c = (+ new-c d))
            (for w-s = (pop ss))
            (setf (aref (aref grid (imagpart cur-c))
                        (realpart cur-c))
                  w-s)
            (incf cur-c d))))
      (sum-box-gps grid))))

(defun expand-grid (grid)
  (map 'vector #'identity
       (iter
         (for row in-vector grid)
         (collecting
          (map 'string #'identity
               (iter
                 (for char in-string row)
                 (case char
                   (#\# (progn
                          (collecting #\#)
                          (collecting #\#)))
                   (#\. (progn
                          (collecting #\.)
                          (collecting #\.)))
                   (#\@ (progn
                          (collecting #\@)
                          (collecting #\.)))
                   (#\O (progn
                          (collecting #\[)
                          (collecting #\]))))))))))

(defun part-2 ()
  (bind (((raw-grid . instructions) (read-problem))
         (grid (expand-grid raw-grid))
         (next-grid (map 'vector #'copy-seq grid))
         (c (find-start grid))
         (seen (make-hash-table :test #'equal))
         (collided (make-hash-table :test #'equal)))
    (labels ((reset-next-grid ()
               (map-into next-grid #'copy-seq grid))
             (grid-aref (coord)
               (aref (aref grid (imagpart coord))
                     (realpart coord)))
             (grid-set (coord value)
               (setf (aref (aref next-grid (imagpart coord))
                           (realpart coord))
                     value))
             (collide-forward (m c1 d)
               (format t "(list m c1 d): ~a~%" (list m c1 d))
               (or #1=(gethash c1 seen)
                   (bind ((new-c (+ c1 d))
                          (s     (grid-aref new-c))
                          (n-m   (grid-aref c1))
                          (vert  (= 1 (abs (imagpart d)))))
                     (format t "(list new-c s n-m vert): ~a~%" (list new-c s n-m vert))
                     (setf #1# (case s
                                 (#\. (progn
                                        (grid-set new-c n-m)
                                        (grid-set c1 m)))
                                 (#\# nil)
                                 (#\[ (when (if vert
                                                (and (collide-forward n-m new-c d)
                                                     (collide-forward (if (char-equal m #\.) #\. (if (gethash (+ c1 #c(1 0)) collided)
                                                                                                     (grid-aref (+ c1 #c(1 0)))
                                                                                                     #\.))
                                                                      (+ new-c #c(1 0))
                                                                      d))
                                                (collide-forward n-m new-c d))
                                        (grid-set c1 m)))
                                 (#\] (when (if vert
                                                (and (collide-forward n-m new-c d)
                                                     (collide-forward (if (char-equal m #\.) #\. (if (gethash (+ c1 #c(-1 0)) collided)
                                                                                                     (grid-aref (+ c1 #c(-1 0)))
                                                                                                     #\.))
                                                                      (+ new-c #c(-1 0))
                                                                      d))
                                                (collide-forward n-m new-c d))
                                        (grid-set c1 m)))))))))
      (print-grid grid)
      (iter
        (for instruction in-string instructions)
        (for d = (case instruction
                   (#\> #c(1 0))
                   (#\^ #c(0 -1))
                   (#\v #c(0 1))
                   (#\< #c(-1 0))))
        (format t "(list instruction c d): ~a~%" (list instruction c d))
        (for new-c = (+ c d))
        (for s = (grid-aref new-c))
        (setf seen (make-hash-table :test #'equal))
        (cond
          ((char-equal s #\.)
           (progn
             (grid-set new-c #\@)
             (grid-set c #\.)
             (rotatef grid next-grid)
             (setf c new-c)))
          ((collide-forward #\. c d)
           (progn
             (reset-next-grid)
             (setf collided seen)
             (setf seen (make-hash-table :test #'equal))
             (collide-forward #\. c d)
             (format t "MOVED~%")
             (grid-set c #\.)
             (rotatef grid next-grid)
             (setf c new-c))))
        (reset-next-grid)
        ;; (print-grid grid)
        )
      (print-grid grid)
      (sum-box-gps grid))))

;; Too high: 1498392
