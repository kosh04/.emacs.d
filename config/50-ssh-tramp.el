;;; config/SSH&Tramp

(with-eval-after-load 'tramp
  ;; easily sudo with /sudo:root@remote-host:<path-to-file>
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:")))

(use-package ssh-config-mode
  :commands
  (ssh-config-mode
   ssh-known-hosts-mode
   ssh-authorized-keys-mode)
  :mode "\\.ssh/config_.*\\'"
  :hook (ssh-known-hosts-mode . display-line-numbers-mode)
  :config
  (add-hook 'ssh-config-mode-hook
            (lambda ()
              (setq-default indent-tabs-mode t)))
  )
