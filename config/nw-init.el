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
