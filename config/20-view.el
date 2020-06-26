;;; config/ViewMode

;; 基本的にファイルは read-only+view-mode で開きたい

;; C-x C-q (read-only-mode) を view-mode として機能させる
(setq view-read-only t)

(defun user::view-mode-maybe ()
  (when (buffer-file-name)
    (view-mode +1)))

(add-hook 'find-function-after-hook #'view-mode)
;; FIXME: ブックマーク登録したディレクトリ (dired) でも有効になってしまう
(add-hook 'bookmark-after-jump-hook #'user::view-mode-maybe)

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
  )
