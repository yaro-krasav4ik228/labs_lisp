(defun reverse-and-nest-tail (lst &optional (temp nil))
  (if (null lst)
      temp
      (reverse-and-nest-tail (cdr lst)
                (if (null temp)
                     (list (car lst))
                     (list (car lst) temp)))))

(defun check-reverse-and-nest-tail (name input expected)
  "Execute `reverse-and-nest-tail' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reverse-and-nest-tail input) expected)
          name))
          
(defun test-reverse-and-nest-tail ()
  (check-reverse-and-nest-tail "test 1" '(1 2 3) '(3 (2 (1))))
  (check-reverse-and-nest-tail "test 2" nil nil)
  (check-reverse-and-nest-tail "test 3" '(a b c) '(c (b (a))))
  (check-reverse-and-nest-tail "test 4" '(x) '(x))
  (check-reverse-and-nest-tail "test 5" '(1 (2) (3 (4))) '((3 (4)) ((2) (1)))))