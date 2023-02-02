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

;; TODO: 省略記号 (ellipsis) ?\u2026 とターミナル+縦分割ウィンドウの相性が悪い
;; そのためアスキー文字で代用したい
(with-eval-after-load 'mule-util
  (setq truncate-string-ellipsis "..."))
(customize-set-variable 'magit-ellipsis "...")
(customize-set-variable 'magit-section-visibility-indicator '("..." . t))

;; Vertical window divider
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?\u2502))
