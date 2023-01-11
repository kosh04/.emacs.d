;;; config/anzu.el --- isearch utility

(use-package anzu
  :after isearch
  :hook (emacs-startup . global-anzu-mode)
  :custom
  (anzu-minimum-input-length 2)
  (anzu-search-threshold 1000)
  (anzu-replace-to-string-separator " => ")
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package anzu
  :after migemo
  :custom (anzu-use-migemo t))
