;;; memo/Generator -*- lexical-binding: t -*-

(require 'generator)
;;(require 'dash)

(iter-defun fibonacci ()
  (let ((prev 1)
        (curr 1))
    (while t
      (setq prev curr
            curr (+ prev curr))
      (iter-yield curr))))

(catch 'iter-break
  (iter-do (f (fibonacci))
    (message "%d" f)
    (if (<= 1000 f) (throw 'iter-break f))))
;; [output]
;; 2
;; 4
;; 8
;; 16
;; 32
;; 64
;; 128
;; 256
;; 512
;; 1024
