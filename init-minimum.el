;;; init-minimum.el --- a minimum of init.el

;; Modes
(show-paren-mode +1)
(electric-pair-mode +1)

;; Keymap
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Custom Variables
(custom-set-variables
 '(view-read-only t)
 '(kill-whole-line t)
 '(disabled-command-function nil)
 '(custom-file (locate-user-emacs-file "custom.el"))
 )

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(provide 'init-minimum)
