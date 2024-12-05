(defpackage 2024-day-5
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-5)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-5.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

(defun parse-problem ()
  (bind ((lines (read-problem))
         (orderings (make-hash-table))
         (pages nil))
    (iter
      (for line in lines)
      (while (not (equal line "")))
      (for (p1 p2) = (mapcar #'read-from-string (split "\\|" line)))
      (push p1 (gethash p2 orderings)))
    (iter
      (for line in (nthcdr (1+ (position "" lines :test #'equal)) lines))
      (for xs = (->> (split "," line) (mapcar #'read-from-string)))
      (push xs pages))
    (list orderings pages)))

(defun part-1 ()
  (bind (((orderings pages) (parse-problem)))
    (iter
      (for xs in pages)
      (when (iter
              (for xt on xs)
              (for x = (car xt))
              (for not-allowed = (gethash x orderings))
              (always
               (iter
                 (for n in not-allowed)
                 (always (not (find n xt))))))
        (summing (nth (floor (length xs) 2) xs))))))

(defun incorrectly-ordered ()
  (bind (((orderings pages) (parse-problem)))
    (iter
      (for xs in pages)
      (when (iter
              (for xt on xs)
              (for x = (car xt))
              (for not-allowed = (gethash x orderings))
              (thereis
               (iter
                 (for n in not-allowed)
                 (thereis (find n xt)))))
        (collecting xs)))))

(defun part-2 ()
  (bind (((orderings pages) (parse-problem))
         (problems (incorrectly-ordered)))
    (declare (ignore pages))
    (iter
      (for xs in problems)
      (iter
        (while
         (iter
           (for xt on xs)
           (for i from 0)
           (for x = (car xt))
           (for not-allowed = (gethash x orderings))
           (for bad-pos = (iter
                            (for n in not-allowed)
                            (for pos = (position n xt))
                            (when pos
                              (return pos))))
           (when bad-pos
             (rotatef (nth i xs)
                      (nth (+ i bad-pos) xs))
             (return t))))))
    (iter
      (for problem in problems)
      (summing (nth (floor (length problem) 2) problem)))))
