 (defun shell-sort-functional (lst)
  (let ((gaps (generate-gaps (length lst))))
    (sort-with-gaps lst gaps)))

(defun generate-gaps (n)
  (if (<= n 1)
      nil
      (let ((next-gap (floor (/ n 2))))
        (cons next-gap (generate-gaps next-gap)))))

(defun sort-with-gaps (lst gaps)
  (if (null gaps)
      lst
      (let ((gap (car gaps)))
        (sort-with-gaps (sort-by-gap lst gap) (cdr gaps)))))

(defun sort-by-gap (lst gap)
  (let ((len (length lst)))
    (loop for i from gap below len
          do (setf lst (insert-at-index lst gap i)))
    lst))

(defun insert-at-index (lst gap index)
  (let ((value (nth index lst))
        (j index))
    (loop while (and (>= (- j gap) 0) (> (nth (- j gap) lst) value))
          do (setf lst (replace-nth lst j (nth (- j gap) lst)))
             (setf j (- j gap)))
    (replace-nth lst j value)))

(defun replace-nth (lst index value)
  (append (subseq lst 0 index)
          (list value)
          (nthcdr (+ index 1) lst)))