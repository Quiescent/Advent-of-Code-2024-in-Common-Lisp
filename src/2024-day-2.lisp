(defpackage 2024-day-2
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-2)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-2.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

(defun part-1 ()
  (iter
    (for line in (read-problem))
    (counting
     (iter
       (with nums = (read-from-string (format nil "(~a)" line)))
       (with direction = (> (- (car nums) (cadr nums)) 0))
       (for x in nums)
       (for y previous x)
       (always (or (null y)
                   (and (eq direction (> (- y x) 0))
                        (<= 1 (abs (- y x)) 3))))))))

(defun is-good (nums)
  (iter
    (for i from 0 below (length nums))
    (for direction = (cond
                       ((= i 0) (> (- (aref nums 1) (aref nums 2)) 0))
                       ((= i 1) (> (- (aref nums 0) (aref nums 2)) 0))
                       (t       (> (aref nums 0)    (aref nums 2)))))
    (thereis
     (iter
       (for j from 0 below (length nums))
       (when (= j i)
         (incf j))
       (while (< j (length nums)))
       (for x = (aref nums j))
       (for y previous x)
       (always (or (null y)
                   (and (eq direction (> (- y x) 0))
                        (<= 1 (abs (- y x)) 3))))))))

(defun part-2 ()
  (iter
    (for line in (read-problem))
    (for nums = (map 'vector #'identity (read-from-string (format nil "(~a)" line))))
    (counting (is-good nums))))
