(defun compress-list (lst &optional (result '()) (count 1))
    (if (null lst)
      (reverse result)
      (let ((current (car lst))
            (next (cadr lst)))
        (if (eq current next)
            (compress-list (cdr lst) result (1+ count))
            (compress-list (cdr lst) (cons (list count current) result) 1)))))

(defun check-compress-list (name input expected)
  "Execute `compress-list' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (compress-list input) expected)
          name))

(defun test-compress-list ()
  (check-compress-list "test 1" '(1 1 1 b a a a a c 2 2 2) '((3 1) (1 B) (4 A) (1 C) (3 2)))
  (check-compress-list "test 2" nil nil)
  (check-compress-list "test 3" '(x x x y y z) '((3 X) (2 Y) (1 Z)))
  (check-compress-list "test 4" '(a) '((1 A)))
  (check-compress-list "test 5" '(1 1 1 1 1) '((5 1))))