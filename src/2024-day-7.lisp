(defpackage 2024-day-7
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-7)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-7.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (mapcar #l(bind (((res rest) (split ": " %1)))
                  (cons (read-from-string res)
                        (read-from-string (format nil "(~a)" rest))))
              (recur)))))

(defun can-produce (res nums)
  (labels ((recur (acc xs)
             (cond
               ((null xs)   (= acc res))
               ((> acc res) nil)
               (t           (or (recur (* (car xs) acc)
                                       (cdr xs))
                                (recur (+ (car xs) acc)
                                       (cdr xs)))))))
    (recur (car nums)
           (cdr nums))))

(defun part-1 ()
  (iter
    (for (res . nums) in (read-problem))
    (when (can-produce res nums)
      (summing res))))

(defun can-produce-advanced (res nums)
  (labels ((recur (acc xs)
             (cond
               ((null xs)   (= acc res))
               ((> acc res) nil)
               (t           (or (recur (* (car xs) acc)
                                       (cdr xs))
                                (recur (+ (car xs) acc)
                                       (cdr xs))
                                (recur (->> (format nil "~a~a" acc (car xs))
                                         read-from-string)
                                       (cdr xs)))))))
    (recur (car nums)
           (cdr nums))))

(defun part-2 ()
  (iter
    (for (res . nums) in (read-problem))
    (when (can-produce-advanced res nums)
      (summing res))))
