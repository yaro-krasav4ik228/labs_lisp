(defun compress-list (lst &optional (result '()) (count 1))
  (if (null lst)
      result
      (let ((current (car lst))(next (cadr lst)))
        (if (eq current next)
            (compress-list (cdr lst) result (1+ count))
            (compress-list (cdr lst) (append result (list (list count current))) 1)))))
