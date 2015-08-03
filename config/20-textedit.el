;;; config/textedit.el

;; auto-fill
(setq fill-column 74)
;(add-hook 'latex-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'html-mode-hook 'turn-off-auto-fill)

(use-package markdown-mode
  :defer t
  :config (fset 'markdown-buffer #'markdown)
  :ensure t)

(use-package yaml-mode
  :defer t
  :mode "\\.yml\\'"
  :ensure t)

(use-package flymake-yaml
  :defer t
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  :ensure yaml-mode)

(use-package flycheck
  :defer t
  :init (custom-set-variables
         '(flycheck-emacs-lisp-load-path 'inherit))
  :config (add-hook 'after-init-hook #'global-flycheck-mode))
