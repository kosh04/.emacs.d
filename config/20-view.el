;;; config/View

;; 基本的にファイルは read-only+view-mode で開きたい
(setq view-read-only t)
(add-hook 'find-function-after-hook 'view-mode)

(use-package view
  :custom
  (view-inhibit-help-message t)
  ;;:config
  ;;(setq-default view-exit-action #'kill-buffer)
  :bind
  (:map view-mode-map
        ;; vi-like
        ("h" . backward-char)
        ("l" . forward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ;; less-like
        ("N" . View-search-last-regexp-backward)
        ("i" . read-only-mode)
        ("RET" . ignore)
        )
  :config
  (define-advice view-mode (:around (f &rest args) maybe)
    "ファイルと関連付けされていない特殊バッファでは利用しない."
    (if (buffer-file-name)
        (apply f args)
      (warn "`view-mode' ignored")))
  )
