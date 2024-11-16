(defun shell-sort-functional (lst &key (key #'identity) (test #'<))
  (let ((gaps (generate-gaps (length lst))))
    (sort-with-gaps (copy-list lst) gaps :key key :test test)))

(defun generate-gaps (n)
  (if (<= n 1)
      nil
      (let ((next-gap (floor (/ n 2))))
        (cons next-gap (generate-gaps next-gap)))))

(defun sort-with-gaps (lst gaps &key key test)
  (if (null gaps)
      lst
      (let ((gap (car gaps)))
        (sort-with-gaps (sort-by-gap lst gap :key key :test test) 
                        (cdr gaps)
                        :key key
                        :test test))))

(defun sort-by-gap (lst gap &key key test)
  (let ((len (length lst)))
    (loop for i from gap below len
          do (setf lst (insert-at-index lst gap i :key key :test test)))
    lst))

(defun insert-at-index (lst gap index &key key test)
  (let* ((value (funcall key (nth index lst)))
         (j index)
         (new-lst (copy-list lst)))
    (loop while (and (>= (- j gap) 0) 
                     (funcall test (funcall key (nth (- j gap) new-lst)) value))
          do (setf (nth j new-lst) (nth (- j gap) new-lst))
             (setf j (- j gap)))
    (setf (nth j new-lst) (nth index lst))
    new-lst))

(defun check-shell-sort (name input expected &key (key #'identity) (test #'<))
  "Execute shell-sort-functional on input, compare result with expected, and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (shell-sort-functional input :key key :test test) expected)
          name))
          
(defun test-shell-sort ()
  (check-shell-sort "test 1" '(5 3 8 6 2) '(2 3 5 6 8) :test #'>)
  (check-shell-sort "test 2" '(5 3 8 6 2) '(8 6 5 3 2) :test #'<)
  (check-shell-sort "test 3" '() '())
  (check-shell-sort "test 4" '(42) '(42))
  (check-shell-sort "test 5" '(1 2 3 4 5) '(1 2 3 4 5) :test #'>)
  (check-shell-sort "test 6" '(5 4 3 2 1) '(5 4 3 2 1) :test #'<)
  (check-shell-sort "test 7" '("aaa" "a" "bb" "aaaa")
                    '("a" "bb" "aaa" "aaaa") :key #'length :test #'>)
  (check-shell-sort "test 8" '("aaa" "a" "bb" "aaaa")
                    '("aaaa" "aaa" "bb" "a") :key #'length :test #'<))