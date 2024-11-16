<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Угнівенко Ярослав Вікторович КВ-11</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання

1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де це
доречно);
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини <23 (7)>
Алгоритм сортування Шелла за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun shell-sort-functional (lst &key (key #'identity) (test #'<))
  (let ((gaps (generate-gaps (length lst))))
    (sort-with-gaps (copy-list lst) gaps :key key :test test)))

(defun generate-gaps (n)
  (if (<= n 1)
      nil
      (let ((next-gap (floor (/ n 2))))
        (cons next-gap (generate-gaps next-gap)))))

(defun sort-with-gaps (lst gaps &key key test)
  (if (null gaps)
      lst
      (let ((gap (car gaps)))
        (sort-with-gaps (sort-by-gap lst gap :key key :test test) 
                        (cdr gaps)
                        :key key
                        :test test))))

(defun sort-by-gap (lst gap &key key test)
  (let ((len (length lst)))
    (loop for i from gap below len
          do (setf lst (insert-at-index lst gap i :key key :test test)))
    lst))

(defun insert-at-index (lst gap index &key key test)
  (let* ((value (funcall key (nth index lst)))
         (j index)
         (new-lst (copy-list lst)))
    (loop while (and (>= (- j gap) 0) 
                     (funcall test (funcall key (nth (- j gap) new-lst)) value))
          do (setf (nth j new-lst) (nth (- j gap) new-lst))
             (setf j (- j gap)))
    (setf (nth j new-lst) (nth index lst))
    new-lst))
```

### Тестові набори та утиліти першої частини

```lisp
(defun check-shell-sort (name input expected &key (key #'identity) (test #'<))
  "Execute shell-sort-functional on input, compare result with expected, and print comparison status."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (shell-sort-functional input :key key :test test) expected)
          name))
          
(defun test-shell-sort ()
  (check-shell-sort "test 1" '(5 3 8 6 2) '(2 3 5 6 8) :test #'>)
  (check-shell-sort "test 2" '(5 3 8 6 2) '(8 6 5 3 2) :test #'<)
  (check-shell-sort "test 3" '() '())
  (check-shell-sort "test 4" '(42) '(42))
  (check-shell-sort "test 5" '(1 2 3 4 5) '(1 2 3 4 5) :test #'>)
  (check-shell-sort "test 6" '(5 4 3 2 1) '(5 4 3 2 1) :test #'<)
  (check-shell-sort "test 7" '("aaa" "a" "bb" "aaaa")
                    '("a" "bb" "aaa" "aaaa") :key #'length :test #'>)
  (check-shell-sort "test 8" '("aaa" "a" "bb" "aaaa")
                    '("aaaa" "aaa" "bb" "a") :key #'length :test #'<))
```

### Тестування першої частини

```lisp
CL-USER> (test-shell-sort)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
passed... test 6
passed... test 7
passed... test 8
NIL
```

## Варіант другої частини <23 (11)>

## Лістинг реалізації другої частини завдання

```lisp
(defun merge-spinning-tuples-fn (&key (shift-step 1))
  (let ((current-shift 0))
    (lambda (&rest lists)
      (let* ((tuple (mapcar #'identity lists)) 
             (length (length tuple))          
             (shift (mod current-shift length))) 
        (setq tuple (append (nthcdr shift tuple)
                            (subseq tuple 0 shift)))
        (setq current-shift (+ current-shift shift-step))
        tuple))))
```

### Тестові набори та утиліти

```lisp
(defun check-merge-spinning-tuples (name func inputs expected)
  "Test merge-spinning-tuples-fn with given inputs and expected output."
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (apply #'mapcar func inputs) expected)
          name))
          
 (defun test-merge-spinning-tuples ()
  (check-merge-spinning-tuples
   "test 1"
   (merge-spinning-tuples-fn)
   '((1 2 3) (a b c))
   '((1 A) (B 2) (3 C)))

  (check-merge-spinning-tuples
   "test 2"
   (merge-spinning-tuples-fn :shift-step 2)
   '((a b c) (1 2 3) (d e f))
   '((A 1 D) (E B 2) (3 F C)))

  (check-merge-spinning-tuples
   "test 3"
   (merge-spinning-tuples-fn)
   '((1 2 3))
   '((1) (2) (3)))

  (check-merge-spinning-tuples
   "test 4"
   (merge-spinning-tuples-fn :shift-step 0)
   '((x y z) (1 2 3) (a b c))
   '((X 1 A) (Y 2 B) (Z 3 C)))

  (check-merge-spinning-tuples
   "test 5"
   (merge-spinning-tuples-fn :shift-step -1)
   '((1 2 3) (a b c) (x y z))
   '((1 A X) (Y 2 B) (C Z 3))))
```

### Тестування

```lisp
CL-USER> (test-merge-spinning-tuples)
passed... test 1
passed... test 2
passed... test 3
passed... test 4
passed... test 5
NIL
```