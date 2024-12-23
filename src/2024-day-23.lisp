(defpackage 2024-day-23
  (:use :cl :iterate :cl-ppcre :metabang-bind :trivia :trivia.ppcre)
  (:shadowing-import-from :arrow-macros :->>)
  (:shadowing-import-from :arrow-macros :->))
(in-package 2024-day-23)
(neat-lambda:enable-lambda-syntax)
(currying:enable-currying-syntax)

(defun read-problem ()
  (with-open-file (f (asdf:system-relative-pathname :advent-of-code-2024 "src/2024-day-23.in"))
    (labels ((recur ()
               (bind ((line (read-line f nil nil)))
                 (when line
                   (cons (->> (split "-" line) (mapcar #'read-from-string)) (recur))))))
      (recur))))

(defun edge-list-to-graph (edge-list)
  (iter
    (with graph = (make-hash-table))
    (for (start end) in edge-list)
    (push end (gethash start graph))
    (push start (gethash end graph))
    (finally (return graph))))

(defun find-tri-sets (graph start)
  (bind ((tri-sets (list)))
   (labels ((recur (path i)
              (cond
                ((> i 3) t)
                ((and (= i 3)
                      (member start (gethash (car path) graph)))
                 (push path tri-sets))
                (t (iter
                     (for n in (gethash (car path) graph))
                     (when (not (member n path))
                       (recur (cons n path) (1+ i))))))))
     (recur (list start) 1)
     tri-sets)))

(defun tri-sets (graph)
  (bind ((all-tri-sets))
    (iter
      (for (start ends) in-hashtable graph)
      (for tri-sets = (find-tri-sets graph start))
      (when tri-sets
        (setf all-tri-sets (append all-tri-sets tri-sets))))
    (-> (mapcar #l(sort (copy-seq %1) #'string-lessp :key #'symbol-name) all-tri-sets)
      (remove-duplicates :test #'equal))))

(defun tri-set-contains-t-computer (tri-set)
  (some #l(-> (symbol-name %1) (aref 0) (char= #\T)) tri-set))

(defun part-1 ()
  (bind ((edge-list (read-problem))
         (graph (edge-list-to-graph edge-list)))
    (->> (tri-sets graph)
      (remove-if-not #'tri-set-contains-t-computer)
      length)))

(defun sort-vertices (vertices)
  (sort (copy-seq vertices) #'string-lessp :key #'symbol-name))

(defun connection-sets (graph)
  (-> (iter
        (for (start ends) in-hashtable graph)
        (collecting (sort-vertices (cons start ends))))
    (sort #'> :key #'length)))

(defun biggest-party-sets (sorted-sets)
  (bind ((biggest (list)))
    (labels ((update-biggest (next)
               (when (> (length next) (length biggest))
                 (setf biggest next)))
             (recur-intersect (acc l rest)
               (cond
                 ((or (null rest) (< (length acc) (length biggest)))
                  nil)
                 ((= l (length acc))
                  (update-biggest acc))
                 (t
                  (recur-intersect (intersection acc (car rest))
                                   (1+ l)
                                   (cdr rest))))))
      (iter
        (for sets on sorted-sets)
        (recur-intersect (car sets) 1 (cdr sets)))
      biggest)))

(defun part-2 ()
  (bind ((edge-list (read-problem))
         (graph (edge-list-to-graph edge-list))
         (sorted-sets (sort (copy-seq (connection-sets graph))
                            #'string-lessp
                            :key #l(->> (car %1) symbol-name)))
         (party (biggest-party-sets sorted-sets)))
    (->> (format nil "~{~a~^,~}" party)
      string-downcase)))
