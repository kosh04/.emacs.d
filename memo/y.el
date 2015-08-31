;; -*- lexical-binding: t -*-

;; Y combinator

(defun f (q)
  (lambda (n)
    (if (= n 0)
        1
        (* n (funcall (funcall q q) (- n 1))))))

(funcall (f 'f) 10)                     ;=> 3628800
