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