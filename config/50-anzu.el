;;; config/anzu.el --- isearch utility

(use-package anzu
  :diminish anzu-mode
  :config (progn
            (global-anzu-mode +1)
            (setq anzu-search-threshold 1000)
            (setq anzu-use-migemo (featurep 'migemo))
            (setq anzu-replace-to-string-separator " => "))
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :ensure t)
