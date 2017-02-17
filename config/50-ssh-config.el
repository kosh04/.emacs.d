;;; config/ssh-config

(use-package ssh-config-mode
  :defer t
  :commands
  (ssh-config-mode
   ssh-known-hosts-mode
   ssh-authorized-keys-mode))
