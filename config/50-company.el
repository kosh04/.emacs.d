;;; config/company.el

;; テキスト補完
(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.1)
    (setq company-selection-wrap-around t)))
