;;; config/Window

;; https://www.emacswiki.org/emacs/WindMove

(require 'windmove)

;; Shift+←→↑↓ でウィンドウ移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)           ; 端から端へ移動できるように
