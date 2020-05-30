;;; init-minimum.el --- a minimum of init.el

;; Modes
(show-paren-mode +1)
(electric-pair-mode +1)

;; Keymap
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

;; Variables
(setq view-read-only t
      view-inhibit-help-message t)
(setq kill-whole-line t)
(setq disabled-command-function nil)
(setq custom-file (locate-user-emacs-file "custom.el"))

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(provide 'init-minimum)
