;;; config/LSP --- Language Server Protocol

;; https://emacs-lsp.github.io/lsp-mode/
;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md

(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  (lsp-restart 'ignore)
  ;;(lsp-document-sync-method 'incremental) ;; always send incremental document
  ;;(lsp-prefer-flymake 'flymake)
  ;;(lsp-enable-completion-at-point nil)
  (lsp-keymap-prefix "s-l")             ; C-x @ s
  :preface
  (defun user::lsp-enable ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil 'local)
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'local)
    (lsp-deferred))
  :bind (:map lsp-mode-map ("<f2>" . lsp-rename))
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
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
  :disabled
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
