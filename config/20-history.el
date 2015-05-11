;;; config/history.el

;; ファイルの履歴等の管理

(setq history-length 250)               ; default 30

(savehist-mode)
(recentf-mode)

;; Desktop -- 終了時の状態を保存
;; http://www.emacswiki.org/emacs/DeskTop
(require 'desktop)
(desktop-save-mode)
;(setq desktop-load-locked-desktop nil)
(setq desktop-dirname user-emacs-directory) ; XXX: 上書きされている？@osx
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-globals-to-save 'command-history)

(defun my:restore-desktop (&optional dirname)
  ".emacs.desktop ファイルを基にセッションを復元します."
  ;;(interactive (list (read-file-name "Desktop file: " (desktop-full-file-name))))
  (interactive (list (read-directory-name "Desktop directory: " desktop-dirname)))
  (let ((desktop-first-buffer nil)
        (desktop-buffer-ok-count 0)
        (desktop-buffer-fail-count 0))
    ;; (load filename 'noerror 'nomsg 'nosfx)
    ;; (desktop-read desktop-dirname)
    (desktop-read dirname)
    ))

;; セッションの復元ならば `recover-session' も有効

(fset 'restore-desktop #'desktop-revert)
;; (defalias 'restore-desktop #'my:restore-desktop)
