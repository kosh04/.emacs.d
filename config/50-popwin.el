;; config/popwin.el

(use-package popwin
  :config (progn
            (popwin-mode +1)
            (custom-set-variables
             '(display-buffer-function #'popwin:display-buffer)
             '(popwin:popup-window-position 'bottom))))
