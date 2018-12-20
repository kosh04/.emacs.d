;;; config/Markdown

(declare-function 'shr-render-buffer "shr")

(use-package markdown-mode
  :defer t
  :config
  (custom-set-variables
   ;;'(markdown-fontify-code-blocks-natively t)
   '(markdown-url-compose-char ?\U0001F517) ; 🔗 (LINK SYMBOL)
   '(markdown-fontify-code-blocks-natively t)
   )
  (defun user:markdown-preview-in-buffer ()
    "作業中のMarkdownファイルをバッファにプレビュー表示する."
    (interactive)
    (require 'shr)
    (shr-render-buffer (markdown-standalone)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-in-buffer)
  (add-hook 'markdown-mode-hook 'outline-minor-mode))
