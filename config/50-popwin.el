;; config/popwin.el

(use-package popwin
  :config
  (popwin-mode +1)
  (custom-set-variables
   ;; Emacs26.2 にて `dired-find-file' などの挙動が変更されたため一時的に無効化
   ;;'(display-buffer-function #'popwin:display-buffer)
   ))

;; https://github.com/wasamasa/shackle
;; (use-package shackle)
