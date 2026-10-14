;;; config/Markdown

;; TODO: Markdown コマンド色々
;; - multimarkdown
;; - pandoc

(use-package markdown-mode
  :mode ("/README\\.md\\'" . gfm-mode)
  :custom
  (markdown-url-compose-char ?\U0001F517) ; 🔗 (LINK SYMBOL)
  (markdown-fontify-code-blocks-natively t)
  (markdown-command
   (or (executable-find "markdown.sh") "markdown"))
  (markdown-gfm-additional-languages "sh")
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
  :hook
  (markdown-mode . outline-minor-mode)
  (markdown-mode . prettify-symbols-mode)
  (markdown-mode . (lambda ()
                     (when window-system
                       (setq prettify-symbols-alist
                             ;; ☐☒ (BALLOT BOX WITH CHECK)
                             ;; ⬜🔳🔲🟩✅❎
                             '(("[ ]" . "🔳")
                               ("[x]" . "✅"))
                             ;; '(("[ ]" . "☐")
                             ;;   ("[x]" . "☑")) ;; ☒
                             ))))
  )

;; Hugo - https://gohugo.io/
(use-package easy-hugo
  :bind ("C-c t h" . easy-hugo)
  :custom
  (easy-hugo-basedir
   (cond ((memq window-system '(w32))
	  "~/Onedrive/Documents/bookshelf")
	 (t
	  "~/Documents/bookshelf")))
  :hook (easy-hugo-mode . hl-line-mode))
