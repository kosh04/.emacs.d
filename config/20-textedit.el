;;; config/textedit.el

;; auto-fill
(setq fill-column 74)
;(add-hook 'latex-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'html-mode-hook 'turn-off-auto-fill)

(use-package markdown-mode
  :defer t
  :config
  (defun user:markdown-preview-buffer ()
    (interactive)
    (shr-render-buffer (markdown-standalone markdown-output-buffer-name)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-buffer)
  :ensure t)

(use-package yaml-mode
  :defer t
  :mode "\\.yml\\'"
  :ensure t)
