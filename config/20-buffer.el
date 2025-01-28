;;; config/Buffer

;; TODO: buffer / window 設定の統合

;; Buffer List
(use-package bs
  :bind
  (("C-x C-b" . bs-show)
   :map bs-mode-map
   ("#" . clean-buffer-list)
   ;;("e" . view-echo-area-messages)
   ))

(add-hook 'bs-mode-hook 'hl-line-mode)
(add-hook 'Buffer-menu-mode-hook 'hl-line-mode)
(add-hook 'ibuffer-mode-hook #'hl-line-mode)

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

;; switch-to-buffer (C-x b) runs pop-to-buffer-same-window
;;(setq switch-to-buffer-obey-display-actions t)
