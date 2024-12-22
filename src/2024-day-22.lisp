(defpackage 2024-day-22
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2024-day-22)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-22.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (recur))))

;; 16777216 is a power of 2

(defun prune (x)
  (logand x (1- 16777216)))

;; 64 and 32 are powers of 2

(defun gen-secret (x)
  (bind ((a (-> (ash x 6)
              (logxor x)
              prune))
         (b (-> (ash a -5)
              (logxor a)
              prune)))
    (-> (ash b 11)
      (logxor b)
      prune)))

(defun gen-2000th-secret (x)
  (iter
    (for i from 0 below 2000)
    (setf x (gen-secret x))
    (finally (return x))))

(defun part-1 ()
  (->> (read-problem)
    (mapcar #'read-from-string)
    (mapcar #'gen-2000th-secret)
    (apply #'+)))

(defun all-two-thousand (x)
  (cons x (iter
            (for i from 0 below 2000)
            (setf x (gen-secret x))
            (collecting x))))

(defun ones (xs)
  (iter
    (for x in xs)
    (collecting (mod x 10))))

(defun deltas (xs)
  (iter
    (for x in xs)
    (for px previous x)
    (when px
      (collecting (- x px)))))

(defun digit-sequence-to-number (xs)
  (iter
    (for digit in xs)
    (with power = 0)
    (summing (* (expt 18 power) (+ digit 9)))
    (incf power)))

(defun compress-deltas (prices deltas)
  (iter
    (with positions = (make-array (list (1+ (digit-sequence-to-number (list 9 9 9 9))))
                                  :initial-element nil))
    (for x in deltas)
    (for i from 1)
    (for px previous x)
    (for ppx  previous px)
    (for pppx previous ppx)
    (when pppx
      (bind ((n (digit-sequence-to-number (list pppx ppx px x))))
        (when (null #1=(aref positions n))
          (setf #1# (nth i prices)))))
    (finally (return positions))))

(defun best-sequence (prices deltas)
  (bind ((compressed (mapcar #'compress-deltas prices deltas)))
    (format t "max: ~a~%" (digit-sequence-to-number (list 9 9 9 9)))
    (iter
      (for x from 0 to (digit-sequence-to-number (list 9 9 9 9)))
      (format t "x: ~a~%" x)
      (maximize
       (iter
         (for cs in compressed)
         (summing (or (aref cs x) 0)))))))

(defun part-2 ()
  (bind ((initial-secrets (mapcar #'read-from-string (read-problem)))
         (2000s (mapcar #'all-two-thousand initial-secrets))
         (prices (mapcar #'ones 2000s))
         (deltas (mapcar #'deltas prices)))
    (best-sequence prices deltas)))

;; Wrong: 2096
