;;; config/server --- Emacs as Daemon


(add-hook 'emacs-startup-hook 'server-start)

;; (custom-set-variables
;;  '(server-use-tcp t))
