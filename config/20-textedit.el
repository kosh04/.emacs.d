;;; config/textedit.el

;; auto-fill
(setq fill-column 74)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Ispell
(require 'ispell)
(when (eq system-type 'windows-nt)
  (setq ispell-program-name "/cygwin/bin/aspell"))

(use-package markdown-mode
  :config (fset 'markdown-buffer #'markdown)
  :ensure t)

(use-package yaml-mode
  :mode "\\.yml\\'"
  :ensure t)

(use-package flymake-yaml
  :config (add-hook 'yaml-mode-hook 'flymake-yaml-load)
  :ensure t)
