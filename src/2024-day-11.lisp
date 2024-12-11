(defpackage 2024-day-11
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-11)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-11.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

(defun has-even-digits (x)
  (= 1 (mod (floor (log x 10)) 2)))

(defun number-halves (x)
  (bind ((e (1+ (log x 10)))
         (m (->> (floor e 2)
              (expt 10))))
    (floor x m)))

(defun part-1 ()
  (bind ((stones (mapcar #'read-from-string (split " " (car (read-problem))))))
    (labels ((transform-stones (acc xs)
               (cond
                 ((null xs) acc)
                 ((= (car xs) 0)
                  (transform-stones (cons 1 acc)
                                    (cdr xs)))
                 ((has-even-digits (car xs))
                  (bind (((:values a b) (number-halves (car xs))))
                    (transform-stones (cons a (cons b acc))
                                      (cdr xs))))
                 (t (transform-stones (cons (* (car xs) 2024) acc)
                                      (cdr xs))))))
      (iter
        (for i from 0 below 25)
        (setf stones (nreverse (transform-stones nil stones)))
        (finally (return (length (print stones))))))))

(defun part-2 ()
  (bind ((all-stones (mapcar #'read-from-string (split " " (car (read-problem)))))
         (results (make-hash-table :test #'equal)))
    (labels ((transform-stone (x i)
               ;; (format t "x: ~a~%" x)
               (cond
                 ((>= i 6 ;25
                      ) (progn ;; (print "done")
                               (princ x) (princ " ")
                                   1))
                 ((= x 0) (or #1=(gethash (cons 0 i) results)
                              (setf #1# (transform-stone 1 (1+ i)))))
                 ((has-even-digits x)
                  (bind (((:values a b) (number-halves x)))
                    (or #2=(gethash (cons x i) results)
                        (setf #2# (+ (or #3=(gethash (cons a i) results)
                                         (setf #3# (transform-stone a (1+ i))))
                                     (or #4=(gethash (cons b i) results)
                                         (setf #4# (transform-stone b (1+ i)))))))))
                 (t (or #2#
                        (setf #2# (transform-stone (* x 2024) (1+ i))))))))
      (iter
        (for stone in all-stones)
        (summing (transform-stone stone 0))))))
