;;; config/Window

;; https://www.emacswiki.org/emacs/WindMove

(require 'windmove)

;; Shift+←→↑↓ でウィンドウ移動
(windmove-default-keybindings 'shift)
(setq windmove-wrap-around t)           ; 端から端へ移動できるように

;; winner-undo: C-c <left>
;; winner-redo: C-c <right>
;(winner-mode +1)

(use-package ace-window
  :disabled
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys (string-to-list "asdfghjkl")))
