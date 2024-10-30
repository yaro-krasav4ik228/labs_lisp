<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Угнівенко Ярослав Вікторович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>
## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку.
Не допускається використання: псевдо-функцій, деструктивних операцій, циклів,
функцій вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Також реалізована функція не має
бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).
## Варіант <23 (7)>
Алгоритм сортування Шелла за незменшенням.
## Лістинг функції з використанням конструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
(test-shell-sort-functional)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
NIL
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
(test-shell-sort-imperative)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
NIL
```