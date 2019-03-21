;;; config/Markdown

(declare-function shr-render-buffer "shr")
(declare-function markdown-standalone "markdown-mode")

(use-package markdown-mode
  :custom
  ;;(markdown-fontify-code-blocks-natively t)
  (markdown-url-compose-char ?\U0001F517) ; 🔗 (LINK SYMBOL)
  (markdown-fontify-code-blocks-natively t)
  :config
  (require 'shr)
  (defun user:markdown-preview-in-buffer ()
    "作業中のMarkdownファイルをバッファにプレビュー表示する."
    (interactive)
    (shr-render-buffer (markdown-standalone)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-in-buffer)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  )
