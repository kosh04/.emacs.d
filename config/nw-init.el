;;; conf/nw-init

;; mouse support
(xterm-mouse-mode +1)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)

;; SSH 経由で開いている場合はテキストブラウザ推奨
(when (getenv "SSH_TTY")
  (custom-set-variables
   '(browse-url-browser-function #'eww-browse-url)))
