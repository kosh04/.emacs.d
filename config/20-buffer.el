;;; config/buffer.el

;; Buffer List
(require 'bs)
(global-set-key (kbd "C-x C-b") 'bs-show)

(add-hook 'bs-mode-hook 'hl-line-mode)
(add-hook 'buffer-menu-mode-hook 'hl-line-mode)

(defvar user::special-display-buffers
  '("*Messages*"
    "*Shell Command Output*")
  "`display-buffer' を内部で利用しているバッファの内")

(customize-set-variable
 'display-buffer-alist
 `((,(rx-to-string
      `(or ,@user::special-display-buffers))
    (display-buffer-same-window)))
 )
