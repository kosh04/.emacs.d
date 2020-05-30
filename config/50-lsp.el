;;; config/LSP --- Language Server Protocol

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md

(use-package lsp-mode
  :after (:any go-mode)
  :hook (go-mode . lsp-deferred)
  :hook (before-save . lsp-format-buffer)
  :custom
  (lsp-auto-guess-root t)
  (lsp-restart 'ignore)
  ;;(lsp-document-sync-method 'incremental) ;; always send incremental document
  ;;(lsp-prefer-flymake 'flymake)
  ;;(lsp-enable-completion-at-point nil)
  ;; :bind (:map lsp-mode-map
  ;;             ("C-c r" . lsp-rename))
  )

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-delay 0.4)
  (lsp-ui-doc-position 'top)
  :bind (:map lsp-mode-map
         ("C-c l" . lsp-ui-imenu)
         :map lsp-ui-imenu-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("<tab>"     . lsp-ui-imenu--next-kind)
         ("<backtab>" . lsp-ui-imenu--prev-kind))
  )

(use-package company-lsp
  :after lsp-mode
  :custom
  (company-lsp-cache-candidates t) ;; always using cache
  (company-lsp-async t)
  (company-lsp-enable-recompletion nil))

;; https://github.com/joaotavora/eglot
(use-package eglot
  :after go-mode
  :config
  ;; NOTE: default (go-mode "go-langserver" ...)
  (add-to-list 'eglot-server-programs '(go-mode "gopls"))
  (add-hook 'go-mode-hook 'eglot-ensure)
  nil)
