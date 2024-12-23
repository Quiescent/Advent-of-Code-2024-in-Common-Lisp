(defpackage 2024-day-12
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>))
(in-package 2024-day-12)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-12.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons line (recur))))))
      (map 'vector #'identity (recur)))))

(defun part-1 ()
  (bind ((grid (read-problem))
         (explored (make-hash-table :test #'equal))
         (ds '(#c(1 0) #c(-1 0) #c(0 1) #c(0 -1)))
         (bound-x (length (aref grid 0)))
         (bound-y (length grid))
         (perimeter 0)
         (area 0))
    (labels ((in-bounds (c)
               (and (>= (realpart c) 0)
                    (>= (imagpart c) 0)
                    (< (realpart c) bound-x)
                    (< (imagpart c) bound-y)))
             (grid-get (c)
               (aref (aref grid (imagpart c))
                     (realpart c)))
             (recur (c l)
               (iter
                 (for d in ds)
                 (for n-c = (+ c d))
                 (if (in-bounds n-c)
                     (bind ((n-l (grid-get n-c)))
                       (if (char-equal n-l l)
                           (when (not (gethash n-c explored))
                             (progn
                               (incf area)
                               (setf (gethash n-c explored) t)
                               (recur n-c l)))
                           (incf perimeter)))
                     (incf perimeter)))))
      (iter outer
        (for y from 0 below bound-y)
        (iter
          (for x from 0 below bound-x)
          (for c = (complex x y))
          (when (not (gethash c explored))
            (setf (gethash c explored) t
                  perimeter            0
                  area                 1)
            (recur c (grid-get c))
            (in outer (summing (* area perimeter)))))))))

(defun part-2-alt ()
  (bind ((grid (read-problem))
         (explored (make-hash-table :test #'equal))
         (ds '(#c(1 0) #c(-1 0) #c(0 1) #c(0 -1)))
         (bound-x (length (aref grid 0)))
         (bound-y (length grid))
         (perimeter 0)
         (area 0))
    (labels ((in-bounds (c)
               (and (>= (realpart c) 0)
                    (>= (imagpart c) 0)
                    (< (realpart c) bound-x)
                    (< (imagpart c) bound-y)))
             (grid-get (c)
               (when (in-bounds c)
                 (aref (aref grid (imagpart c))
                       (realpart c))))
             (recur (c l)
               (bind ((edges (mapcar #l(+ %1 c) ds))
                      (squares (mapcar #l(when (in-bounds %1)
                                           (grid-get %1))
                                       edges))
                      ((right left down up) squares))

                 ;; Convex corners
                 (when (and (not (eq right l))
                            (not (eq up    l)))
                   (incf perimeter))
                 (when (and (not (eq right l))
                            (not (eq down  l)))
                   (incf perimeter))
                 (when (and (not (eq left  l))
                            (not (eq up    l)))
                   (incf perimeter))
                 (when (and (not (eq left  l))
                            (not (eq down  l)))
                   (incf perimeter))

                 ;; Concave corners
                 (when (and (eq right l)
                            (eq up    l)
                            (not (eq (grid-get (+ c #c(1 -1))) l)))
                   (incf perimeter))
                 (when (and (eq right l)
                            (eq down  l)
                            (not (eq (grid-get (+ c #c(1 1))) l)))
                   (incf perimeter))
                 (when (and (eq left  l)
                            (eq up    l)
                            (not (eq (grid-get (+ c #c(-1 -1))) l)))
                   (incf perimeter))
                 (when (and (eq left  l)
                            (eq down  l)
                            (not (eq (grid-get (+ c #c(-1 1))) l)))
                   (incf perimeter))
                 
                 (iter
                   (for n-c in edges)
                   (when (in-bounds n-c)
                     (bind ((n-l (grid-get n-c)))
                       (when (and (not (gethash n-c explored))
                                  (char-equal n-l l))
                         (incf area)
                         (setf (gethash n-c explored) t)
                         (recur n-c l))))))))
      (iter outer
        (for y from 0 below bound-y)
        (iter
          (for x from 0 below bound-x)
          (for c = (complex x y))
          (when (not (gethash c explored))
            (setf (gethash c explored) t
                  perimeter            0
                  area                 1)
            (recur c (grid-get c))
            (in outer (summing (* area perimeter)))))))))

;; Tried tracing the shape here, but I forgot about shapes like this:
;;
;; #
;; ##
;; #
;;
;; Where you have to backtrack to continue tracing :/
;;
;; So I decided to count corners as in the above solution.
(defun part-2 ()
  (bind ((grid (read-problem))
         (explored (make-hash-table :test #'equal))
         (ds '(#c(1 0) #c(-1 0) #c(0 1) #c(0 -1)))
         (bound-x (length (aref grid 0)))
         (bound-y (length grid))
         (perimeter 0)
         (area 0))
    (labels ((in-bounds (c)
               (and (>= (realpart c) 0)
                    (>= (imagpart c) 0)
                    (< (realpart c) bound-x)
                    (< (imagpart c) bound-y)))
             (grid-get (c)
               (aref (aref grid (imagpart c))
                     (realpart c)))
             (recur (c l)
               (iter
                 (for d in ds)
                 (for n-c = (+ c d))
                 (when (in-bounds n-c)
                   (bind ((n-l (grid-get n-c)))
                     (if (char-equal n-l l)
                         (when (not (gethash n-c explored))
                           (progn
                             (incf area)
                             (setf (gethash n-c explored) t)
                             (recur n-c l))))))))
             (trace-recur (c-s c l t-d)
               (format t "(list c-s c l t-d): ~a~%" (list c-s c l t-d))
               (bind ((n-c (+ t-d c)))
                 (cond
                   ((equal c-s n-c) (incf perimeter))
                   ((is-on-boundary n-c l) (trace-recur c-s n-c l t-d))
                   (t (progn
                        (incf perimeter)
                        (bind ((n-t-d (find-border-dir c l t-d)))
                          (trace-recur c-s (+ c n-t-d) l n-t-d)))))))
             (is-on-boundary (c l)
               (and (char-equal (grid-get c) l)
                    (iter
                      (for d in ds)
                      (for n-c = (+ c d))
                      (finding d such-that (or (not (in-bounds n-c))
                                               (not (char-equal (grid-get n-c) l)))))))
             (find-border-dir (c l t-d)
               (iter
                 (for d in ds)
                 (when (and t-d (equal d (- t-d)))
                   (next-iteration))
                 (for n-c = (+ c d))
                 (when (in-bounds n-c)
                   (bind ((n-l (grid-get n-c)))
                     (when (and (char-equal n-l l)
                                (is-on-boundary n-c l))
                       (return d))))))
             (trace-boundary (c-s c l)
               (trace-recur c-s c l (print (find-border-dir c l nil)))))
      (iter outer
        (for y from 0 below bound-y)
        (iter
          (for x from 0 below bound-x)
          (for c = (complex x y))
          (when (not (gethash c explored))
            (setf (gethash c explored) t
                  perimeter            0
                  area                 1)
            (recur c (grid-get c))
            (trace-boundary c c (grid-get c))
            (in outer (summing (* area perimeter)))))))))
