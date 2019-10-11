;;; config/LSP --- Language Server Protocol

(use-package lsp-mode
  :disabled
  :hook (go-mode . lsp)
  ;;:hook (before-save . lsp-format-buffer)
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
  :disabled
  :after lsp
  :bind (:map lsp-mode-map
              ("C-c l" . lsp-ui-imenu))
  )

(use-package company-lsp
  :after lsp
  :custom
  (company-lsp-cache-candidates t) ;; always using cache
  (company-lsp-async t)
  (company-lsp-enable-recompletion nil))

;; https://github.com/joaotavora/eglot
;;(require 'eglot nil t)
(use-package eglot
  :after go-mode
  :config
  ;; NOTE: default (go-mode "go-langserver" ...)
  (add-to-list 'eglot-server-programs '(go-mode "gopls"))
  (add-hook 'go-mode-hook 'eglot-ensure)
  nil)
