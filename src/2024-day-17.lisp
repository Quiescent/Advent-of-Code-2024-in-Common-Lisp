(defpackage 2024-day-17
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-17)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-17.in"))
    (labels ((read-program ()
               (bind ((line (read-line f nil nil)))
                 (->> (ppcre:all-matches-as-strings "(\\d+)" line)
                   (map 'vector #'read-from-string))))
             (read-registers (registers)
               (bind ((line (read-line f nil nil)))
                 (if (string= line "")
                     (cons (nreverse registers) (read-program))
                     (read-registers (cons (->> (ppcre:all-matches-as-strings "(\\d+)" line)
                                             car
                                             read-from-string)
                                           registers))))))
      (read-registers nil))))

(defun run-program (a b c program)
  (bind ((i 0)
         (bound (length program))
         (out nil))
    (labels ((combo (op)
               (cond
                 ((and (<= op 3) (>= op 0)) op)
                 ((= op 4) a)
                 ((= op 5) b)
                 ((= op 6) c)
                 (t (error (format nil "invalid combo op ~a" op)))))
             (execute (inst op)
               (ecase inst
                 (0 (progn (setf a (floor a (expt 2 (combo op)))) (incf i 2)))
                 (1 (progn (setf b (logxor b op)) (incf i 2)))
                 (2 (progn (setf b (mod (combo op) 8)) (incf i 2)))
                 (3 (if (/= a 0) (setf i op) (incf i 2)))
                 (4 (progn (setf b (logxor b c)) (incf i 2)))
                 (5 (progn ;(format t "(list a b c): ~a~%" (list a b c))
                           (push (mod (combo op) 8) out) (incf i 2)))
                 (6 (progn (setf b (floor a (expt 2 (combo op)))) (incf i 2)))
                 (7 (progn (setf c (floor a (expt 2 (combo op)))) (incf i 2))))))
      (iter
        (while (and (< i bound)
                    (>= i 0)))
        (execute (aref program i)
                 (aref program (1+ i))))
      (nreverse out))))

(defun part-1 ()
  (bind (((registers . program) (read-problem))
         ((a b c) registers)
         (out (run-program a b c program)))
    (iter
      (for j from 0)
      (for i in out)
      (princ i)
      (when (/= j (1- (length out)))
       (princ ",")))))

(defun list-vec-equal (xs ys)
  (and
   (= (length xs) (length ys))
   (iter
     (for x in xs)
     (for y in-vector ys)
     (always (= x y)))))

(defun part-2 ()
  (bind ((input (read-problem))
         (program (cdr input))
         (e (* 3 (expt 8 (1- (print (length program)))))))

    (format t "e: ~a~%" e)

    (iter
      (with cnt = 0)
      (with j = 0)
      (for i from (+ 281474976710656 (* 3 (floor (- 2251799813685248 281474976710656) 4))) below 2251799813685248)
      (for res = (run-program i 0 0 program))
      (finding i such-that (list-vec-equal res program))
      (incf j)
      (when (= j 100000)
        (setf j 0)
        (incf cnt)
        (format t "cnt: ~a~%" cnt))
      ;; (/ (- 129140163 43046721) 100000.0)
      (format t "~a: ~a~%" i res)
      )

    (mod 3 8)
    ;; 3849

    ;; (iter
    ;;   (for i from 1 below (length program))
    ;;   (format t "~a~%" (* 3 (expt 8 i))))
    ;; (run-program (lcm 105553116266496 8) 0 0 program)
    ))
