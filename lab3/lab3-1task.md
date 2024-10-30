(defun shell-sort-functional (lst)
  (let ((gaps (generate-gaps (length lst))))
    (sort-with-gaps lst gaps)))

(defun sort-with-gaps (lst gaps)
  (if (null gaps)
      lst
      (let ((gap (car gaps)))
        (sort-with-gaps (sort-by-gap lst gap 0) (cdr gaps)))))

(defun sort-by-gap (lst gap index)
  (if (>= index (length lst))
      lst
      (let ((updated-list (insert-at-index lst gap index)))
        (sort-by-gap updated-list gap (+ index 1)))))

(defun insert-at-index (lst gap index)
  (let ((value (nth index lst)))
    (insert-helper lst value index gap)))

(defun insert-helper (lst value index gap)
  (if (< index gap)
      (replace-nth lst index value)
      (let* ((new-index (- index gap))
             (new-list (replace-nth lst index (nth new-index lst))))
        (if (> (nth new-index lst) value)
            (insert-helper new-list value new-index gap)
            (replace-nth new-list index value)))))

(defun replace-nth (lst index value)
  (append (subseq lst 0 index)
          (list value)
          (nthcdr (+ 1 index) lst)))

(defun generate-gaps (n)
  (if (<= n 1)
      nil
      (let ((next-gap (floor (/ n 2)))) 
        (cons next-gap (generate-gaps next-gap)))))

(defun check-shell-sort-functional (name input expected)
  "Execute `shell-sort-functional' on `input', compare result with `expected' and print
comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (shell-sort-functional input) expected)
          name))

(defun test-shell-sort-functional ()
  (check-shell-sort-functional "test 1" '(34 8 64 51 32 21) '(8 21 32 34 51 64))
  (check-shell-sort-functional "test 2" '(1 0 0 0 34 15 86 74 13 13 29) '(0 0 0 1 13 13 15 29 34 74 86))
  (check-shell-sort-functional "test 3" nil nil)
  (check-shell-sort-functional "test 4" '(1 2 3 4) '(1 2 3 4))
  (check-shell-sort-functional "test 5" '(4 3 2 1) '(1 2 3 4))
  (check-shell-sort-functional "test 6" '(5 5 5 5) '(5 5 5 5)))

(test-shell-sort-functional)