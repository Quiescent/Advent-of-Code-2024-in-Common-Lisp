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
    (count-if #l(can-construct available %1) patterns)))

(defun all-ways-to-construct (available pattern)
  (bind ((from (make-hash-table)))
    (labels ((recur (i)
               ;; (format t "i: ~a~%" i)
               (or #1=(gethash i from)
                   (setf #1# (or (= i (length pattern))
                                 (iter
                                   (for towel in available)
                                   (counting (and (<= (+ i (length towel)) (length pattern))
                                                  (search towel pattern :start2 i :end2 (+ i (length towel)))
                                                  (recur (+ i (length towel)))))))))))
      (prog1 (recur 0)
        (print "tick")))))

(defun all-ways-to-construct-2 (available pattern)
  (bind ((from (make-array (list (length pattern)) :initial-element nil)))
    (labels ((recur (i)
               ;; (format t "i: ~a~%" i)
               (or #1=(aref from i)
                   (setf #1# (if (= i 0) 1
                                 (iter
                                   (for towel in available)
                                   (when (and (>= (- i (length towel)) 0)
                                              (search towel pattern :start2 (- i (length towel)) :end2 i))
                                     (summing (recur (- i (length towel)))))))))))
      (progn
        (recur (1- (length pattern)))
        (format t "result: ~a~%" (aref from (1- (length pattern))))
        (aref from (1- (length pattern)))))))

(defun part-2 ()
  (bind (((available . patterns) (read-problem)))
    (->> (mapcar #l(all-ways-to-construct-2 available %1) patterns)
      (apply #'+))))

;; Wrong: 383080585227721
