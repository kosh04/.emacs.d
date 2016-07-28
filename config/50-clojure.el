;;; config/clojure.el

(use-package clojure-mode
  :pin melpa-stable
  :defer t
  :config
  (setq clojure-align-forms-automatically nil) ; or clojure-align <C-c SPC>
  (add-hook 'clojure-mode-hook #'subword-mode))

;; M-x cider-jack-in
(use-package cider
  :pin melpa-stable
  :defer t
  :init
  (fset 'lein-repl #'cider-jack-in)
  :config
  (defun user:end-of-symbol (&rest args)
    (skip-syntax-forward "w_"))
  (advice-add 'cider-eval-last-sexp :before 'user:end-of-symbol)
  (setq cider-repl-result-prefix ";;=> ")
  (setq nrepl-buffer-name-show-port nil) ; [PROJECT/NAME@PORT]
  (setq nrepl-hide-special-buffers t)
  ;;(add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  ;; cider-repl
  (custom-set-variables
   '(cider-repl-history-file (locate-user-emacs-file "nrepl-history"))
   '(cider-repl-use-pretty-printing t))
  :ensure clojure-mode)
