;;; config/ViewMode

;; 基本的にファイルは read-only+view-mode で開きたい

;; TODO: Diff-mode+read-only-mode 時に有効になるキーバインドを view-mode で潰したくない

;; C-x C-q (read-only-mode) を view-mode として機能させる
(setq view-read-only t)

(defun user::view-mode-maybe ()
  (when (buffer-file-name)
    (view-mode +1)))

(add-hook 'find-function-after-hook #'view-mode)
;; FIXME: ブックマーク登録したディレクトリ (dired) でも有効になってしまう
(add-hook 'bookmark-after-jump-hook #'user::view-mode-maybe)

;; `view-mode' 有効化の有無を少しだけ視覚的にわかりやすく.
(defun user::hl-line-mode-dwim ()
  (hl-line-mode (if view-mode +1 -1)))
(add-hook 'view-mode-hook #'user::hl-line-mode-dwim)

(use-package view
  :custom
  (view-inhibit-help-message t)
  ;;:config
  ;;(setq-default view-exit-action #'kill-buffer)
  :bind
  (:map view-mode-map
        ("q" . View-kill-and-leave)     ; or "C"
        ;; vi-like
        ("h" . backward-char)
        ("l" . forward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ;; less-like
        ("N" . View-search-last-regexp-backward)
        ("i" . read-only-mode)
        ("RET" . ignore)
        ("g" . recenter)                  ; disable `View-goto-line'
        )
  )

;; TODO: view-lock-mode
;; https://qiita.com/s-fubuki/items/01943c4652484942c327
