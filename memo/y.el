;;; y.el --- Y combinator         -*- lexical-binding: t -*-

(defun f (q)
  (lambda (n)
    (if (= n 0)
        1
      (* n (funcall (funcall q q) (- n 1))))))

(funcall (f 'f) 10)
;;=> 3628800
