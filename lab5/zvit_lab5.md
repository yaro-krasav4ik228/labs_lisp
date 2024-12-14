<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Угнівенко Я. В. КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант <23 (11)>
База даних: Космічні апарати
Тип записів: Геш-таблиця

## Лістинг реалізації завдання
```lisp
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

(defun select (file-path &rest filter-keys)
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
  (let ((headers (alexandria:hash-table-keys (first records)))) ; Використовуємо alexandria
    (with-open-file (stream file-path :direction :output :if-exists :supersede)
      (format stream "~{~A,~}~%" headers) ; записуємо заголовки
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
```

### Тестові набори та утиліти
```lisp
(defun check (name actual expected)
  (format t "~:[FAILED~;PASSED~]... ~a~%"
          (equal actual expected)
          name))

(defun test-functions ()
  (check "create-hash-record test"
         (let ((record (create-hash-record '("ID" "Name" "Type") '("1" "Apollo" "Orbiter"))))
           (hash-to-alist record))
         '(("ID" . "1") ("Name" . "Apollo") ("Type" . "Orbiter")))

  (let ((test-data '("ID,Name,Type"
                     "1,Apollo,Orbiter"
                     "2,Luna,Lander")))
    (let* ((headers (mapcar (lambda (header) (string-trim "\"" header))
                            (cl-ppcre:split "," (string (first test-data)))))
           (records (mapcar (lambda (line)
                              (create-hash-record headers
                                                  (mapcar (lambda (value)
                                                            (string-trim "\"" value))
                                                          (cl-ppcre:split "," (string line)))))
                            (rest test-data)))
           (select-fn (lambda (&rest conditions)
                        (let ((filtered-records records))
                          (loop for (key value) on conditions by #'cddr
                                do (setf filtered-records
                                         (remove-if-not (lambda (record)
                                                          (equal value (gethash key record)))
                                                        filtered-records)))
                          filtered-records))))
      (check "select test"
             (mapcar #'hash-to-alist (funcall select-fn "Type" "Orbiter"))
             '((("ID" . "1") ("Name" . "Apollo") ("Type" . "Orbiter"))))))

  (let* ((hash (create-hash-record '("ID" "Name" "Type") '("4" "Vostok" "Orbiter")))
         (alist (hash-to-alist hash))
         (converted-hash (alist-to-hash alist)))
    (check "hash-to-alist test" alist '(("ID" . "4") ("Name" . "Vostok") ("Type" . "Orbiter")))
    (check "alist-to-hash test" (hash-to-alist converted-hash) alist)))

(test-functions)
```

### Тестування
```lisp
PASSED... create-hash-record test
PASSED... select test
PASSED... hash-to-alist test
PASSED... alist-to-hash test
NIL
```
