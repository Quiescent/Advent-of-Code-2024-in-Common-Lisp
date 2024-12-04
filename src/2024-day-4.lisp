(defpackage 2024-day-4
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-4)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-4.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

(defun part-1 ()
  (bind ((grid (->> (read-problem)
                 (map 'vector #'identity))))
    (+ (iter outer
         (for y-start from 0 below (length grid))
         (iter
           (for y from y-start below (length grid))
           (for x from 0 below (length (aref grid 0)))
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d))))))
         (iter
           (for y from y-start downto 0)
           (for x from 0 below (length (aref grid 0)))
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d)))))))
       (iter outer
         (for y-start from 1 below (1- (length grid)))
         (iter
           (for y from y-start below (length grid))
           (for x from (1- (length (aref grid 0))) downto 0)
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d))))))
         (iter
           (for y from y-start downto 0)
           (for x from (1- (length (aref grid 0))) downto 0)
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d)))))))
       (iter outer
         (for x from 0 below (length (aref grid 0)))
         (iter
           (for y from 0 below (length grid))
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d)))))))
       (iter outer
         (for y from 0 below (length grid))
         (iter
           (for x from 0 below (length (aref grid 0)))
           (for a = (aref (aref grid y) x))
           (for b previous a)
           (for c previous b)
           (for d previous c)
           (in outer (counting (or (equal '(#\X #\M #\A #\S) (list a b c d))
                                   (equal '(#\S #\A #\M #\X) (list a b c d))))))))))

;; wrong: 1288

(defun part-2 ()
  (bind ((grid (->> (read-problem)
                 (map 'vector #'identity))))
    (labels ((safe-aref (x y)
               (cond
                 ((or (< x 0) (< y 0))          nil)
                 ((>= x (length (aref grid 0))) nil)
                 ((>= y (length grid))          nil)
                 (t (aref (aref grid y) x)))))
      (iter outer
        (for x from 0 below (length (aref grid 0)))
        (iter
          (for y from 0 below (length grid))
          (for a = (safe-aref (1- x) (1- y)))
          (for b = (safe-aref (1+ x) (1- y)))
          (for c = (safe-aref x y))
          (for d = (safe-aref (1- x) (1+ y)))
          (for e = (safe-aref (1+ x) (1+ y)))
          (in outer (counting (and (or (equal (list a c e) '(#\M #\A #\S))
                                       (equal (list a c e) '(#\S #\A #\M)))
                                   (or (equal (list b c d) '(#\M #\A #\S))
                                       (equal (list b c d) '(#\S #\A #\M)))))))))))
