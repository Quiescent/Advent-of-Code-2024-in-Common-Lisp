(defpackage 2024-day-1
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-1)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-1.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons (->> (split "   " line)
                           (mapcar #'read-from-string))
                         (recur))))))
      (bind ((lists (recur)))
        (cons (mapcar #'car lists)
              (mapcar #'cadr lists))))))

(defun part-1 ()
  (bind (((xs . ys) (read-problem)))
    (->> (map 'list
              #'-
              (sort xs #'<)
              (sort ys #'<))
      (mapcar #'abs)
      (apply #'+))))

(defun counts (xs)
  (iter
    (with c = (make-hash-table))
    (for x in xs)
    (incf (gethash x c 0))
    (finally (return c))))

(defun part-2 ()
  (bind (((xs . ys) (read-problem))
         (c (counts xs)))
    (iter
      (for y in ys)
      (summing (* y (gethash y c 0))))))
