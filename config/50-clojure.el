;;; config/clojure.el

(use-package clojure-mode
  :defer t)

;; M-x cider-jack-in
(use-package cider
  :defer t
  :config (progn
            (defun user:end-of-symbol (&rest args)
              (skip-syntax-forward "w_"))
            (advice-add 'cider-eval-last-sexp :before 'user:end-of-symbol)
            (setq cider-repl-result-prefix ";;=> "))
  :ensure clojure-mode)
