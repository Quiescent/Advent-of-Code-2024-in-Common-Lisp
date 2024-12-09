(defpackage 2024-day-9
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-9)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-9.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (map 'vector #'digit-char-p (car (recur))))))

(defun fill-res (res line)
  (iter
    (for i from 0)
    (for is-even = (= 0 (mod i 2)))
    (with idx = 0)
    (with j = 0)
    (for x in-vector line)
    (iter
      (for k from 0 below x)
      (setf (aref res idx) (when is-even j))
      (incf idx))
    (when is-even
      (incf j))))

(defun compact (res)
  (bind ((idx (position nil res))
         (src (1- (length res))))
    (iter
      (while (position nil res :from-end t :start idx :end src))
      (rotatef (aref res idx)
               (aref res src))
      (setf idx (position nil res :start idx))
      (decf src))
    res))

(defun expand (line)
  (bind ((len (apply #'+ (map 'list #'identity line)))
         (res (make-array (list len) :initial-element nil)))
    (fill-res res line)
    res))

(defun part-1 ()
  (iter
    (for i from 0)
    (for x in-vector (->> (read-problem)
                       expand
                       compact))
    (while (not (null x)))
    (summing (* i x))))

(defun compact-w-o-frags (space res)
  res
  (bind ((orig-res (copy-seq res))
         (src-end (1- (length orig-res)))
         (src-start (1+ (position-if-not #l(eq (aref res src-end) %1) orig-res :from-end t :end src-end)))
         (len (- src-end src-start)))
    (iter
      (for i from (1- (length space)) above 0)
      (while (and src-start src-end))
      (iter inner
        (with start-idx = 0)
        (for i from 0 below (length space))
        (for l = (aref space i))
        (while (< start-idx src-start))
        (for is-odd = (= 1 (mod i 2)))
        (when (and is-odd (>= l (1+ len)))
          (fill res (aref res src-start) :start start-idx :end (+ 1 start-idx len))
          (fill res nil                  :start src-start :end (1+ src-end))
          (decf (aref space i) (1+ len))
          (incf (aref space (1- i)) (1+ len))
          (return-from inner))
        (incf start-idx l))
      (setf src-end (position-if-not #l(eq %1 nil) orig-res :from-end t :end src-start))
      (for new-start = (position-if-not #l(eq %1 (aref res src-end)) orig-res :from-end t :end src-end))
      (setf src-start (when new-start (1+ new-start)))
      (when (and src-end src-start)
        (setf len (- src-end src-start))))
    res))

(defun part-2 ()
  (iter
    (with space = (read-problem))
    (for i from 0)
    (for x in-vector (->> space
                       expand
                       (compact-w-o-frags space)))
    (when (not (null x))
      (summing (* i x)))))
