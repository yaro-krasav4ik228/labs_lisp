(ql:quickload '(:alexandria :cl-ppcre))

(defun create-hash-record (keys values)
  (let ((hash (make-hash-table :test 'equal)))
    (loop for key in keys
          for value in values
          do (setf (gethash key hash) value))
    hash))

(defun read-csv (file-path)
  (with-open-file (stream file-path)
    (let* ((lines (loop for line = (read-line stream nil)
                        while line collect line))
           (headers (mapcar (lambda (header) (string-trim "\"" header))
                            (cl-ppcre:split "," (first lines))))
           (records (mapcar (lambda (line)
                              (create-hash-record headers
                                                  (mapcar (lambda (value)
                                                            (string-trim "\"" value))
                                                          (cl-ppcre:split "," line))))
                            (rest lines))))
      records)))

(defun select (file-path)
  (let ((records (read-csv file-path)))
    (lambda (&rest conditions)
      (let ((filtered-records records))
        (loop for (key value) on conditions by #'cddr
              do (setf filtered-records 
                       (remove-if-not (lambda (record)
                                        (equal value (gethash key record)))
                                      filtered-records)))
        filtered-records))))

(defun write-csv (file-path records)
  (let ((headers (alexandria:hash-table-keys (first records))))
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (format stream "~{~A,~}~%" headers)
      (loop for record in records
            do (format stream "~{~A,~}~%" 
                       (loop for key in headers
                             collect (gethash key record)))))))

(defun hash-to-alist (hash)
  (let (alist)
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash)
    (reverse alist)))

(defun alist-to-hash (alist &optional (test 'equal))
  (let ((hash (make-hash-table :test test)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

(defun pretty-print-hash (hash)
  (format t "~{~A: ~A~^, ~}~%"
          (loop for key being the hash-keys of hash
                append (list key (gethash key hash)))))

(defun pretty-print-records (records)
  (loop for record in records
        do (pretty-print-hash record)))

(defvar *test-csv* "test.csv")

(defun check (name actual expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal actual expected)
          name))

(defun test-select ()
  (let ((test-data '("ID,Name,Type"
                     "1,Apollo,Orbiter"
                     "2,Luna,Lander"
                     "3,Curiosity,Rover")))
    (with-open-file (stream "temp-test.csv" :direction :output :if-exists :supersede)
      (dolist (line test-data)
        (format stream "~A~%" line)))
    (let* ((selector (select "temp-test.csv"))
           (result-1 (funcall selector "Type" "Orbiter"))
           (result-2 (funcall selector "Name" "Luna")))
      (check "select function test 1"
             (mapcar #'hash-to-alist result-1)
             '((("ID" . "1") ("Name" . "Apollo") ("Type" . "Orbiter"))))
      (check "select function test 2"
             (mapcar #'hash-to-alist result-2)
             '((("ID" . "2") ("Name" . "Luna") ("Type" . "Lander")))))
    (uiop:delete-file-if-exists "temp-test.csv")))

(defun test-functions ()
  (check "create-hash-record test"
         (let ((record (create-hash-record '("ID" "Name" "Type") '("1" "Apollo" "Orbiter"))))
           (hash-to-alist record))
         '(("ID" . "1") ("Name" . "Apollo") ("Type" . "Orbiter")))
  (test-select)
  (let* ((hash (create-hash-record '("ID" "Name" "Type") '("4" "Vostok" "Orbiter")))
         (alist (hash-to-alist hash))
         (converted-hash (alist-to-hash alist)))
    (check "hash-to-alist test" alist '(("ID" . "4") ("Name" . "Vostok") ("Type" . "Orbiter")))
    (check "alist-to-hash test" (hash-to-alist converted-hash) alist)))

(test-functions)

