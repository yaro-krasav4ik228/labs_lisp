<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Угнівенко Я.В. КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>
## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно реалізувати, задаються варіантом (п. 2.1.1). Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів (див. п. 2.3).
## Варіант 23 (8)
1. Написати функцію reverse-and-nest-tail , яка обертає вхідний список та утворює
вкладeну структуру з підсписків з його елементами, починаючи з хвоста:
```
CL-USER> (reverse-and-nest-tail '(a b c))
(C (B (A)))
```
2. Написати функцію compress-list , яка заміщає сукупності послідовно
розташованих однакових елементів списку двоелементними списками виду
(кількість-повторень елемент):
```
CL-USER> (compress-list '(1 a a 3 3 3 b))
((1 1) (2 A) (3 3) (1 B))
```
## Лістинг функції <reverse-and-nest-tail>
```
CL-USER> (defun reverse-and-nest-tail (lst &optional (temp nil))
  (if (null lst)
      temp
      (reverse-and-nest-tail (cdr lst)
                (if (null temp)
                     (list (car lst))
                     (list (car lst) temp)))))
```
### Тестові набори
```
CL-USER> (defun check-reverse-and-nest-tail (name input expected)
  "Execute `reverse-and-nest-tail' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (reverse-and-nest-tail input) expected)
          name))
          
CL-USER> (defun test-reverse-and-nest-tail ()
  (check-reverse-and-nest-tail "test 1" '(1 2 3) '(3 (2 (1))))
  (check-reverse-and-nest-tail "test 2" nil nil)
  (check-reverse-and-nest-tail "test 3" '(a b c) '(c (b (a))))
  (check-reverse-and-nest-tail "test 4" '(x) '(x))
  (check-reverse-and-nest-tail "test 5" '(1 (2) (3 (4))) '((3 (4)) ((2) (1)))))
```
### Тестування
```
CL-USER> (test-reverse-and-nest-tail)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
NIL
```
## Лістинг функції <compress-list>
```
 CL-USER> (defun compress-list (lst &optional (result '()) (count 1))
    (if (null lst)
      (reverse result)
      (let ((current (car lst))
            (next (cadr lst)))
        (if (eq current next)
            (compress-list (cdr lst) result (1+ count))
            (compress-list (cdr lst) (cons (list count current) result) 1)))))
```
### Тестові набори
```
CL-USER> (defun check-compress-list (name input expected)
  "Execute `compress-list' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (compress-list input) expected)
          name))
          
CL-USER> (defun test-compress-list ()
  (check-compress-list "test 1" '(1 1 1 b a a a a c 2 2 2) '((3 1) (1 B) (4 A) (1 C) (3 2)))
  (check-compress-list "test 2" nil nil)
  (check-compress-list "test 3" '(x x x y y z) '((3 X) (2 Y) (1 Z)))
  (check-compress-list "test 4" '(a) '((1 A)))
  (check-compress-list "test 5" '(1 1 1 1 1) '((5 1))))
```
### Тестування
```
CL-USER> (test-compress-list)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
NIL
```