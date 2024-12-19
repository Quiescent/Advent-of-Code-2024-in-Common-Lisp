(defpackage 2024-day-19
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-19)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-19.in"))
    (labels ((read-patterns ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (read-patterns)))))
             (read-towels ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (read-line f nil nil)
                   (cons (split ", " line)
                         (read-patterns))))))
      (read-towels))))

(defun can-construct (available pattern)
  (labels ((recur (i)
             (or (= i (length pattern))
                 (iter
                   (for towel in available)
                   (thereis (and (<= (+ i (length towel)) (length pattern))
                                 (search towel pattern :start2 i :end2 (+ i (length towel)))
                                 (recur (+ i (length towel)))))))))
    (recur 0)))

(defun part-1 ()
  (bind (((available . patterns) (read-problem)))
    (count-if #l(/= 0 (all-ways-to-construct available %1)) patterns)))

(defun all-ways-to-construct (available pattern)
  (bind ((from (make-array (list (1+ (length pattern))) :initial-element nil)))
    (labels ((recur (i)
               (or #1=(aref from i)
                   (if (= i 0) 1
                       (setf #1# (iter
                                   (for towel in available)
                                   (when (and (>= (- i (length towel)) 0)
                                              (progn
                                                (search towel pattern :start2 (- i (length towel)) :end2 i)))
                                     (summing (recur (- i (length towel)))))))))))
      (progn
        (recur (length pattern))
        (aref from (length pattern))))))

(defun part-2 ()
  (bind (((available . patterns) (read-problem)))
    (->> (mapcar #l(all-ways-to-construct available %1) patterns)
      (apply #'+))))
