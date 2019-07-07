;;; config/History --- ファイルの履歴等の管理

(setq history-length 250)               ; default 30
(setq history-length t)                 ; no truncation.

;; ミニバッファの履歴を保存
(savehist-mode +1)

;; ファイルを開いたときのカーソル位置を復元
(save-place-mode +1)
(defun user/save-alist-skip-remote ()
  "HTTPなどのリモートファイルの確認が煩わしいのでスキップする."
  (setq save-place-alist
        (seq-remove (lambda (x)
                      (pcase-let ((`(,file . ,pos) x))
                        (file-remote-p file)))
                    save-place-alist)))
(advice-add 'save-place-alist-to-file :before #'user/save-alist-skip-remote)

(require 'recentf)
(recentf-mode +1)
(setq recentf-save-file (locate-user-emacs-file "recentf"))
(setq recentf-max-saved-items 2000)
(setq recentf-auto-cleanup 'never)
;; 保存ファイルの設定に リモートファイル tramp の先等を追加。これを実施すると起動時にパスワード等の確認はされない
(setf (cdr recentf-keep) '(file-remote-p file-readable-p))
(add-to-list 'recentf-exclude "recentf")
(add-to-list 'recentf-exclude "\\.git/")
(add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa/")
(add-to-list 'recentf-exclude "^/ssh:")
(add-to-list 'recentf-exclude "^/sudo:")
(add-to-list 'recentf-exclude "^https?://")

;; Desktop -- 終了時の状態を保存
;; http://www.emacswiki.org/emacs/DeskTop
;; FIXME: NTEmacs(win)/Cygwin(unix) は .emacs.desktop を共有できないため分ける必要がある
(require 'desktop)
(desktop-save-mode)
;(setq desktop-load-locked-desktop nil)
(setq desktop-dirname user-emacs-directory) ; XXX: 上書きされている？@osx
(add-to-list 'desktop-globals-to-save 'file-name-history)
(add-to-list 'desktop-globals-to-save 'command-history)

(defun user:restore-desktop (&optional dirname)
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
