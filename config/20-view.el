;;; config/ViewMode

;; ** 基本的にファイルは read-only+view-mode で開きたい
;; C-x C-r と使い分けるよりはそっちの方が楽なハズ

;; TODO: Diff-mode+read-only-mode 時に有効になるキーバインドを view-mode で潰したくない

;; C-x C-q (read-only-mode) を view-mode として機能させる
(setq view-read-only t)

(define-advice find-file (:after (&rest _args) view-mode-by-default)
  (when (called-interactively-p 'interactive)
    (view-mode +1)))

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
        ("i" . View-exit-and-edit)
        ("RET" . ignore)
        ("g" . recenter)                  ; disable `View-goto-line'
        )
  )

;; TODO: view-lock-mode
;; https://qiita.com/s-fubuki/items/01943c4652484942c327
