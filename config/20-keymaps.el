;;; config/keymaps.el

(global-set-key (kbd "C-h") 'backward-delete-char)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "C-l") 'recenter)

(require 'ffap)
;; [C-x C-v] 等は通常通り使いたいので全部載せはいらない
;;(ffap-bindings)
(global-set-key (kbd "C-x C-f") 'find-file-at-point)

(electric-pair-mode +1)
;; (global-set-key (kbd "(") 'insert-pair)     ; ()
;; (global-set-key (kbd "[") 'insert-pair)     ; []
;; (global-set-key (kbd "{") 'insert-pair)     ; {}
;; (global-set-key (kbd "<") 'insert-pair)     ; <>
;; (global-set-key (kbd "\"") 'insert-pair)    ; ""
;; (global-set-key (kbd "ESC '") 'insert-pair) ; ''
;; (global-set-key (kbd "ESC `") 'insert-pair) ; `'

(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; toggle
(global-set-key (kbd "C-x t f") 'toggle-truncate-lines)
(global-set-key (kbd "C-x t e") 'toggle-debug-on-error)

;; disable
(global-unset-key (kbd "C-x m"))        ; compose-mail
(global-unset-key (kbd "C-x C-n"))      ; set-goal-column
