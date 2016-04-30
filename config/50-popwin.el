;; config/popwin.el

(use-package popwin
  ;;:disabled t
  :config
  (popwin-mode +1)
  (custom-set-variables
   '(display-buffer-function #'popwin:display-buffer)
   '(popwin:popup-window-position 'bottom)))
