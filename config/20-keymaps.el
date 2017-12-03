;;; config/keymaps

(global-set-key (kbd "C-h") 'backward-delete-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "C-l") 'recenter)

(global-set-key [remap just-one-space] 'cycle-spacing)

;; [C-x C-v] 等は通常通り使いたいので全部載せはいらない
;;(require 'ffap)
;;(ffap-bindings)
(global-set-key (kbd "C-x C-f") 'find-file-at-point)

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; toggle
(global-set-key (kbd "C-x t f") 'toggle-truncate-lines)
(global-set-key (kbd "C-x t e") 'toggle-debug-on-error)
(global-set-key (kbd "C-x t v") 'toggle-viper-mode)
(global-set-key (kbd "C-x t p") 'list-packages)
(global-set-key (kbd "C-x t w") 'whitespace-mode)

(global-set-key [f11] 'toggle-frame-fullscreen)

;; disable
(global-unset-key (kbd "C-x m"))        ; compose-mail
(global-unset-key (kbd "C-x C-n"))      ; set-goal-column

(define-key completion-list-mode-map "q"
  (lambda () (interactive) (quit-window t)))

;; prefixキーのコマンド一覧を表示
(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay 1.5))
