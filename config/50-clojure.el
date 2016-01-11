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
            (setq cider-repl-result-prefix ";;=> ")
            (setq nrepl-buffer-name-show-port nil) ; [PROJECT/NAME@PORT]
            (setq nrepl-hide-special-buffers t)
            (add-hook 'clojure-mode-hook 'cider-mode)
            (add-hook 'cider-mode-hook 'eldoc-mode)
            ;; cider-repl
            (custom-set-variables
             `(cider-repl-history-file ,(locate-user-emacs-file "nrepl-history"))
             `(cider-repl-use-pretty-printing t))
            )
  :ensure clojure-mode)
