;;; config/textedit.el

;; auto-fill
(setq fill-column 74)
;(add-hook 'latex-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'html-mode-hook 'turn-off-auto-fill)

(use-package markdown-mode
  :defer t
  :config
  (defun user:markdown-preview-in-buffer ()
    "作業中のMarkdownファイルをバッファにプレビュー表示する."
    (interactive)
    (require 'shr)
    (shr-render-buffer (markdown-standalone)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-in-buffer)
  :ensure t)

(use-package yaml-mode
  :defer t
  :mode "\\.yml\\'"
  :ensure t)
