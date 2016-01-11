;;; config/company.el

;; テキスト補完
(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.2)
    (setq company-selection-wrap-around t)
    (define-key company-active-map (kbd "C-h") 'delete-backward-char)
    ))
