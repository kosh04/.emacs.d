;;; conf/nw-init

;; mouse support
;; FIXME: マウスクリックの有効とコピペは共存できない？
(xterm-mouse-mode +1)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

;; SSH 経由で開いている場合はテキストブラウザ推奨
(use-package browse-url
  :when (getenv "SSH_TTY")
  :custom
  (browse-url-browser-function #'eww-browse-url))

;; TODO: 一部の Unicode は文字幅問題によりターミナル+縦分割ウィンドウとの相性が悪いためアスキー文字で代用したい
;; - 省略記号 (ellipsis; …) ?\u2026
;; - outline: 🔽 ▶️ v >

(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "..."))

(use-package magit
  :custom
  (magit-ellipsis "...")
  (magit-section-visibility-indicator '("..." . t)))

(use-package icons
  :custom
  (icon-preference '(text emoji image symbol)))

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))

;; コマンドラインからメニューバー呼び出し
;; M-x tmm-menubar (M-`)
