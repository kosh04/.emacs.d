;;; config/buffer.el

;; Buffer List
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

(define-key bs-mode-map (kbd "C-n") 'bs-down)
(define-key bs-mode-map (kbd "C-p") 'bs-up)

(add-hook 'bs-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)
