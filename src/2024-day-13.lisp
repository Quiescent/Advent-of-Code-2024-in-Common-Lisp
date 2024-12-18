(defpackage 2024-day-13
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-13)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct machine
  (a (complex 0 0) :type complex)
  (b (complex 0 0) :type complex)
  (prize (complex 0 0) :type complex))

(defun parse-machine (machine-lines)
  (make-machine
   :a (match (caddr machine-lines)
        ((ppcre "Button A: X\\+(\\d+), Y\\+(\\d+)"
                (read x)
                (read y))
         (complex x y)))
   :b (match (cadr machine-lines)
        ((ppcre "Button B: X\\+(\\d+), Y\\+(\\d+)"
                (read x)
                (read y))
         (complex x y)))
   :prize (match (car machine-lines)
            ((ppcre "Prize: X=(\\d+), Y=(\\d+)"
                    (read x)
                    (read y))
             (complex x y)))))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-13.in"))
    (labels ((recur (machine)
               (bind ((line (read-line f nil nil)))
                 (cond
                   ((null line) (list (parse-machine machine)))
                   ((equal line "") (cons (parse-machine machine)
                                          (recur nil)))
                   (t (recur (cons line machine)))))))
      (recur nil))))

(defun min-tokens (machine)
  (bind ((dest (machine-prize machine))
         (a    (machine-a     machine))
         (b    (machine-b     machine)))
    (or (iter
          (for i from 0 below 100)
          (for x = (* i a))
          (for r = (- dest x))
          (for j = (/ r b))
          (when (= 0 (imagpart j))
            (minimizing (+ (* 3 i) j))))
        0)))

(defun part-1 ()
  (bind ((machines (read-problem)))
    (->> (mapcar #'min-tokens machines)
      (apply #'+))))

(defun min-tokens-alt-part-2 (machine)
  (bind ((dest (+ (complex 10000000000000 10000000000000) (machine-prize machine)))
         (a    (machine-a machine))
         (b    (machine-b machine))
         (i    (floor (- (* (realpart dest) (imagpart b))
                         (* (realpart b)    (imagpart dest)))
                      (- (* (realpart a)    (imagpart b))
                         (* (imagpart a)    (realpart b)))))
         (j    (floor (- (imagpart dest)
                         (* (imagpart a) i))
                      (imagpart b))))
    (if (= (+ (* i a) (* j b)) dest)
        (+ (* i 3) j)
        0)))

(defun part-2 ()
  (bind ((machines (read-problem)))
    (->> (mapcar #'min-tokens-alt-part-2 machines)
      (apply #'+))))
