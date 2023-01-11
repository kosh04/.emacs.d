;;; config/History --- ファイルの履歴等の管理

;; - 終了時の状態をいろいろ保存する
;; - 履歴を扱う変数もできる限り保存する (ミニバッファ, キルリング, etc.)
;; - リモートファイルの履歴は削除しないが、オフライン時にハングアップしないよう注意

(setq history-length 250)               ; default 30
(setq history-length t)                 ; no truncation.
(setq history-delete-duplicates t)

;; ミニバッファの履歴を保存
(use-package savehist
  :demand
  :custom
  (savehist-file (locate-user-emacs-file "cache/history"))
  (savehist-additional-variables
   '(command-history
     extended-command-history))
  :config
  (savehist-mode +1))

;; ファイルを開いたときのカーソル位置を復元
(use-package saveplace
  :demand
  :custom
  (save-place-file (locate-user-emacs-file "cache/places"))
  (save-place-forget-unreadable-files nil)
  :config
  (save-place-mode +1))

(require 'recentf)
(setq recentf-save-file (locate-user-emacs-file "cache/recentf"))
(setq recentf-max-saved-items 2000)
(setq recentf-auto-cleanup 'never)
;; 保存ファイルの設定に リモートファイル tramp の先等を追加。これを実施すると起動時にパスワード等の確認はされない
;;(setf (cdr recentf-keep) '(file-remote-p file-readable-p))
(add-to-list 'recentf-exclude "recentf")
(add-to-list 'recentf-exclude "\\.git/")
(add-to-list 'recentf-exclude "/\\.emacs\\.d/elpa/")
;; (add-to-list 'recentf-exclude "^/ssh:")
(add-to-list 'recentf-exclude "^/sudo:")
;;(add-to-list 'recentf-exclude "^https?://")
(recentf-mode +1)
(global-set-key (kbd "C-c t r") #'recentf-open-files)

;; Desktop -- 終了時の状態を保存
;; http://www.emacswiki.org/emacs/DeskTop
;; FIXME: NTEmacs(win)/Cygwin(unix) は .emacs.desktop を共有できないため分ける必要がある
(use-package desktop
  :demand
  :custom
  ;;(desktop-load-locked-desktop nil)
  ;;(desktop-restore-in-current-display nil)
  ;; NOTE: tab-bar の復元にはフレームが必要
  (desktop-restore-frames t)
  :config
  ;; WARNING: 循環リストを含むコマンドを保存しようとすると無限ループの可能性あり '#0=(x . #0#)
  ;; そうでなくても重い気がする...
  ;;(add-to-list 'desktop-globals-to-save 'command-history)
  ;;(add-to-list 'desktop-globals-to-save 'extended-command-history)
  (add-to-list 'desktop-path (locate-user-emacs-file "cache/"))
  (desktop-save-mode +1)
  )

(defun user::restore-desktop (&optional dirname)
  ".emacs.desktop ファイルを基にセッションを復元します."
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
;; (defalias 'restore-desktop #'user::restore-desktop)
