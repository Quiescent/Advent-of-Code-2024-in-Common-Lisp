(defpackage 2024-day-6
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-6)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-6.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (->> (recur)
        (map 'vector #'identity)))))

(defun find-start (grid)
  (iter outer
    (for y from 0)
    (for ys in-vector grid)
    (iter
      (for x from 0)
      (for s in-string ys)
      (when (char-equal s #\^)
        (return-from outer (complex x y))))))

(defun positions-on-patrol ()
  (iter
    (with seen = (make-hash-table :test #'equal))
    (with grid = (read-problem))
    (with coord = (find-start grid))
    (initially (setf (gethash coord seen) t))
    (with direction = #c(0 -1))
    (with max-x = (1- (length (aref grid 0))))
    (with max-y = (1- (length grid)))
    (for next-pos = (+ direction coord))
    (while (and (<= (realpart next-pos) max-x)
                (<= (imagpart next-pos) max-y)
                (>= (realpart next-pos) 0)
                (>= (imagpart next-pos) 0)))
    (for s = (aref (aref grid (imagpart next-pos))
                   (realpart next-pos)))
    (cond
      ((char-equal s #\#) (setf direction (* direction #c(0 1))))
      (t (progn
           (setf coord next-pos)
           (setf (gethash coord seen) t))))
    (finally (return seen))))

(defun part-1 ()
  (hash-table-count (positions-on-patrol)))

(defun loops (grid coord)
  (iter
    (with seen = (make-hash-table :test #'equal))
    (with direction = #c(0 -1))
    (initially (setf (gethash (cons direction coord) seen) t))
    (with max-x = (1- (length (aref grid 0))))
    (with max-y = (1- (length grid)))
    (for next-pos = (+ direction coord))
    (while (and (<= (realpart next-pos) max-x)
                (<= (imagpart next-pos) max-y)
                (>= (realpart next-pos) 0)
                (>= (imagpart next-pos) 0)))
    (for s = (aref (aref grid (imagpart next-pos))
                   (realpart next-pos)))
    (cond
      ((char-equal s #\#) (setf direction (* direction #c(0 1))))
      (t (progn
           (setf coord next-pos)
           (when (gethash (cons direction coord) seen)
             (return t))
           (setf (gethash (cons direction coord) seen) t))))))

(defun deep-copy-seq (xss)
  (iter
    (with new-seq = (copy-seq xss))
    (for xs in-vector xss)
    (for i from 0)
    (setf (aref new-seq i) (copy-seq xs))
    (finally (return new-seq))))

(defun part-2 ()
  (bind ((grid      (read-problem))
         (coord     (find-start grid))
         (positions (positions-on-patrol)))
    (remhash coord positions)
    (iter
      (for (position _) in-hashtable positions)
      (for tmp-grid = (deep-copy-seq grid))
      (setf (aref (aref tmp-grid (imagpart position))
                  (realpart position))
            #\#)
      (counting (loops tmp-grid coord)))))
