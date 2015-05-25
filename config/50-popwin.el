;; config/popwin.el

(use-package popwin
  :config
  (custom-set-variables
   '(display-buffer-function #'popwin:display-buffer)
   '(popwin:popup-window-position 'bottom)))
