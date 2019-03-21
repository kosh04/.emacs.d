;;; config/anzu.el --- isearch utility

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  :custom
  (anzu-search-threshold 1000)
  (anzu-use-migemo (featurep 'migemo))
  (anzu-replace-to-string-separator " => ")
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :ensure t)
