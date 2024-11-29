(defun shell-sort-functional (lst &key (key #'identity) (test #'<))
  (let* ((keyed-list (mapcar (lambda (x) (cons (funcall key x) x)) lst))
         (gaps (generate-gaps (length lst))))
    (let ((sorted-keyed-list (sort-with-gaps keyed-list gaps :test test)))
      (mapcar #'cdr sorted-keyed-list))))

(defun generate-gaps (n)
  (if (<= n 1)
      nil
      (let ((next-gap (floor (/ n 2))))
        (cons next-gap (generate-gaps next-gap)))))

(defun sort-with-gaps (lst gaps &key test)
  (if (null gaps)
      lst
      (let ((gap (car gaps)))
        (sort-with-gaps (sort-by-gap lst gap :test test)
                        (cdr gaps)
                        :test test))))

(defun sort-by-gap (lst gap &key test)
  (labels ((process-list (lst i)
             (if (>= i (length lst))
                 lst
                 (let ((new-lst (insert-at-index lst i gap :test test)))
                   (process-list new-lst (1+ i))))))
    (process-list lst gap)))

(defun insert-at-index (lst index gap &key test)
  (let ((value (nth index lst)))
    (labels ((insert-helper (lst value j)
               (if (< j gap)
                   (replace lst (list value) :start1 j :end1 (1+ j))
                   (let ((prev-value (nth (- j gap) lst)))
                     (if (funcall test (car value) (car prev-value))
                         (insert-helper
                          (replace lst (list prev-value) :start1 j :end1 (1+ j))
                          value
                          (- j gap))
                         (replace lst (list value) :start1 j :end1 (1+ j)))))))
      (insert-helper lst value index))))

(defun check-shell-sort (name input expected &key (key #'identity) (test #'<))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (shell-sort-functional input :key key :test test) expected)
          name))

(defun test-shell-sort ()
  (check-shell-sort "test 1" '(5 3 8 6 2) '(2 3 5 6 8))
  (check-shell-sort "test 2" '(5 3 8 6 2) '(8 6 5 3 2) :test #'>)
  (check-shell-sort "test 3" '() '())
  (check-shell-sort "test 4" '(42) '(42))
  (check-shell-sort "test 5" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-shell-sort "test 6" '(5 4 3 2 1) '(5 4 3 2 1) :test #'>)
  (check-shell-sort "test 7" '("aaa" "a" "bb" "aaaa")
                    '("a" "bb" "aaa" "aaaa") :key #'length)
  (check-shell-sort "test 8" '("aaa" "a" "bb" "aaaa")
                    '("aaaa" "aaa" "bb" "a") :key #'length :test #'>))
(test-shell-sort)

