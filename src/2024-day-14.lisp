(defpackage 2024-day-14
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-14)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defstruct robot
  (pos #c(0 0))
  (vel #c(0 0)))

(defun parse-robot (line)
  (match line
    ((ppcre "p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)"
            (read x)
            (read y)
            (read vx)
            (read vy))
     (make-robot :pos (complex x y)
                 :vel (complex vx vy)))))

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-14.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons (parse-robot line) (recur))))))
      (recur))))

(defun complex-mod (c bound-r bound-i)
  (complex (mod (realpart c) bound-r)
           (mod (imagpart c) bound-i)))

(defun count-in-quad (robots x1 y1 x2 y2)
  (iter
    (for robot in robots)
    (with-slots (vel pos) robot
      (bind ((x (realpart pos))
             (y (imagpart pos)))
        (counting (and (>= x x1)
                       (>= y y1)
                       (< x x2)
                       (< y y2)))))))

(defun part-1 ()
  (bind ((robots (read-problem))
         (x-bound 101)
         (mid-x   (floor x-bound 2))
         (y-bound 103)
         (mid-y   (floor y-bound 2)))
    (iter
      (for i from 0 below 100)
      (iter
        (for robot in robots)
        (with-slots (vel pos) robot
          (incf pos vel)
          (setf pos (complex-mod pos x-bound y-bound)))))
    
    (*
     ;; Top Left
     (count-in-quad robots 0  0  mid-x  mid-y)
     ;; Top Right
     (count-in-quad robots (1+ mid-x) 0  x-bound mid-y)
     ;; Bottom left
     (count-in-quad robots 0 (1+ mid-y) mid-x y-bound)
     ;; Bottom right
     (count-in-quad robots (1+ mid-x) (1+ mid-y) x-bound y-bound))))

(defun draw-robots (robots x-bound y-bound)
  (bind ((grid (make-array (list y-bound x-bound) :initial-element nil)))
    (iter
      (for robot in robots)
      (with-slots (pos) robot
        (bind ((x (realpart pos))
               (y (imagpart pos)))
          (setf (aref grid y x) t))))
    (fresh-line)
    (iter
      (for y from 0 below y-bound)
      (iter
        (for x from 0 below x-bound)
        (princ (if (aref grid y x) "#" " ")))
      (fresh-line))))

(defun part-2 ()
  (bind ((robots (read-problem))
         (x-bound 101)
         (y-bound 103)
         (j 33)
         (d 101))
    (iter
      (for i from 0 below 10000)
      (iter
        (for robot in robots)
        (with-slots (vel pos) robot
          (incf pos vel)
          (setf pos (complex-mod pos x-bound y-bound))))
      (when (= i (1- j))
        (incf j d)
        (format t "==========Iteration ~a==========" (1+ i))
        (draw-robots robots x-bound y-bound)))))

;; 33
;; 134
;; ...
;; 538
;; 740
;; 841
