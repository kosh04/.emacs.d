;;; config/Markdown

(declare-function 'shr-render-buffer "shr")

(use-package markdown-mode
  :defer t
  :config
  (custom-set-variables
   '(markdown-url-compose-char ?\U0001F517)) ; 🔗 (LINK SYMBOL)
  (defun user:markdown-preview-in-buffer ()
    "作業中のMarkdownファイルをバッファにプレビュー表示する."
    (interactive)
    (require 'shr)
    (shr-render-buffer (markdown-standalone)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-in-buffer))
