;;; config/anzu.el --- isearch utility

(use-package anzu
  :disabled
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

;; TODO: マッチ数カウントは emacs-27 より標準機能になった
;; ただし migemo とはまだ相性がよくない
(setopt isearch-lazy-count t)
(setopt isearch-allow-scroll t)
(setopt lazy-count-prefix-format nil
	lazy-count-suffix-format " (%s/%s)")
