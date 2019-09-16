;;; config/Markdown

(use-package markdown-mode
  :custom
  ;;(markdown-fontify-code-blocks-natively t)
  (markdown-url-compose-char ?\U0001F517) ; 🔗 (LINK SYMBOL)
  (markdown-fontify-code-blocks-natively t)
  (markdown-command
   (or (executable-find "markdown.sh") "markdown"))
  :preface
  (declare-function shr-render-buffer "shr")
  (declare-function markdown-standalone "markdown-mode")
  :config
  (require 'shr)
  (defun user::markdown-preview-in-buffer ()
    "作業中のMarkdownファイルをバッファにプレビュー表示する."
    (interactive)
    (shr-render-buffer (markdown-standalone)))
  (defalias 'markdown-preview-buffer 'user:markdown-preview-in-buffer)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  )

;; Hugo - https://gohugo.io/
(use-package easy-hugo
  :bind ("C-x t h" . easy-hugo)
  :custom
  (easy-hugo-basedir "~/Dropbox/Documents/bookshelf"))
