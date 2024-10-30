(defun generate-gaps (n)
  (let ((gaps '())
        (gap (floor (/ n 2))))
    (loop while (> gap 0)
          do (progn
               (push gap gaps)
               (setq gap (floor (/ gap 2)))))
    gaps))

(defun shell-sort-imperative (lst)
  (let* ((sorted (copy-list lst))
         (n (length sorted))
         (gaps (generate-gaps n)))
    (dolist (gap gaps)
      (loop for i from gap below n
            do (let ((temp (nth i sorted))
                     (j i))
                 (loop while (and (>= j gap)
                                  (> (nth (- j gap) sorted) temp))
                       do (progn
                            (setf (nth j sorted) (nth (- j gap) sorted))
                            (setq j (- j gap))))
                 (setf (nth j sorted) temp))))
    sorted))

(defun check-shell-sort-imperative (name input expected)
  "Execute `shell-sort-imperative' on `input', compare result with `expected' and print
comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (shell-sort-imperative input) expected)
          name))

(defun test-shell-sort-imperative ()
  (check-shell-sort-imperative "test 1" '(34 8 64 51 32 21) '(8 21 32 34 51 64))
  (check-shell-sort-imperative "test 2" '(1 0 0 0 34 15 86 74 13 13 29) '(0 0 0 1 13 13 15 29 34 74 86))
  (check-shell-sort-imperative "test 3" nil nil)
  (check-shell-sort-imperative "test 4" '(1 2 3 4) '(1 2 3 4))
  (check-shell-sort-imperative "test 5" '(4 3 2 1) '(1 2 3 4))
  (check-shell-sort-imperative "test 6" '(5 5 5 5) '(5 5 5 5)))

(test-shell-sort-imperative)