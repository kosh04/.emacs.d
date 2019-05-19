;;; config/SSH&Tramp

(with-eval-after-load 'tramp
  ;; easily sudo with /sudo:root@remote-host:<path-to-file>
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:")))

(use-package ssh-config-mode
  :commands
  (ssh-config-mode
   ssh-known-hosts-mode
   ssh-authorized-keys-mode)
  :hook (ssh-known-hosts-mode . display-line-numbers-mode))
