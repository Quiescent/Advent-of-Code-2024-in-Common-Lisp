(defpackage 2024-day-3
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-3)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-3.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

(defun part-1 ()
  (iter
    (for line in (read-problem))
    (summing
     (iter
       (for mul in (cl-ppcre:all-matches-as-strings "mul\\((\\d+),(\\d+)\\)" line))
       (summing (match mul
                  ((ppcre "mul\\((\\d+),(\\d+)\\)"
                          (read a )
                          (read b))
                   (* a b))))))))

(defun part-2 ()
  (iter outer
    (for line in (read-problem))
    (with enabled = t)
    (iter
      (for mul in (cl-ppcre:all-matches-as-strings "(mul\\((\\d+),(\\d+)\\))|(don't\\(\\))|(do\\(\\))" line))
      (match mul
        ((ppcre "mul\\((\\d+),(\\d+)\\)"
                (read a)
                (read b))
         (when enabled (in outer (summing (* a b)))))
        ((ppcre "don't\\(\\)")
         (setf enabled nil))
        ((ppcre "do\\(\\)")
         (setf enabled t))))))
